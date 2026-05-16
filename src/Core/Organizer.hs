module Core.Organizer
  ( OrganizeOptions(..)
  , organizeByType
  , organizeByTypeWith
  , organizeByTypeDryRun
  ) where

import Core.RulePresets (CustomRule(..))
import Core.Detect (detectType)
import Core.Dedupe (renameOrCopy, uniqueDest)
import Core.Logger (logMove, logSkip)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, makeRelative, takeDirectory, equalFilePath)
import System.IO (Handle)
import Control.Exception (try, SomeException)
import Data.Char (toLower)
import Data.List (isPrefixOf, isInfixOf)
import Control.Monad (when)


ansiReset :: String
ansiReset = "\x1b[0m"

ansiBold :: String
ansiBold = "\x1b[1m"

ansiCyan :: String
ansiCyan = "\x1b[36m"

opTag :: String -> String
opTag label = ansiBold <> ansiCyan <> "[" <> label <> "]" <> ansiReset <> " "

logOp :: String -> Int -> Int -> IO ()
logOp label i total =
  putStrLn $ opTag "op" <> label <> " " <> show i <> "/" <> show total


data OrganizeOptions = OrganizeOptions
  { optDryRun      :: Bool
  , optVerbose     :: Bool
  , optCustomRules :: [CustomRule]
  }

ignoredNames :: [String]
ignoredNames = ["backup", ".backup", "_backup"]

ensureDirs :: FilePath -> IO ()
ensureDirs root = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")

-- | Simple organizer: MIME-based only, no options. Used by tests.
organizeByType :: FilePath -> Handle -> [FilePath] -> IO ()
organizeByType root h files = do
  ensureDirs root
  mapM_ (processFile (OrganizeOptions False False []) root h Nothing) files

-- | Full organizer: respects options including custom sort rules.
organizeByTypeWith :: OrganizeOptions -> FilePath -> Handle -> [FilePath] -> IO ()
organizeByTypeWith opts root h files = do
  ensureDirs root
  let customFolders   = map ruleFolder (optCustomRules opts)
      outputDirs      = map (root </>) (["text", "images", "other", "deleteme"] ++ customFolders)
      alreadySorted p = any (equalFilePath (takeDirectory p)) outputDirs
      files'          = filter (\p -> takeFileName p `notElem` ignoredNames
                                   && not (alreadySorted p)) files
      total           = length files'
  mapM_ (\(i, fp) -> processFile opts root h (Just (i, total)) fp) (zip [1..] files')

-- | Dry-run wrapper: shows what would happen without moving anything.
organizeByTypeDryRun :: FilePath -> Handle -> [FilePath] -> IO ()
organizeByTypeDryRun root h files =
  organizeByTypeWith (OrganizeOptions True True) root h files


processFile :: OrganizeOptions -> FilePath -> Handle -> Maybe (Int, Int) -> FilePath -> IO ()
processFile opts root h mProgress src = do
  let rel = makeRelative root src

  -- Progress line (GUI progress bar uses this)
  case mProgress of
    Just (i, total) ->
      putStrLn $
        "[progress] " <>
        (if optDryRun opts then "planning " else "moving ") <>
        show i <> "/" <> show total
    Nothing -> return ()

  -- ANSI operation tag (same color for all ops)
  case mProgress of
    Just (i, total) -> logOp "classify" i total
    Nothing -> return ()
  organizeByTypeWith (OrganizeOptions True True []) root h files

-- ─── Internal ───────────────────────────────────────────────────────────────

-- | Check custom rules before falling back to MIME type (case-insensitive).
applyRules :: [CustomRule] -> String -> Maybe String
applyRules rules fname =
  case filter (\r -> map toLower (ruleKeyword r) `isInfixOf` map toLower fname) rules of
    (r:_) -> Just (ruleFolder r)
    []    -> Nothing

processFile :: OrganizeOptions -> FilePath -> Handle -> Maybe (Int, Int) -> FilePath -> IO ()
processFile opts root h mProgress src = do
  let rel   = makeRelative root src
      fname = takeFileName src
  case mProgress of
    Just (i, total) -> putStrLn $ "[progress] " ++ (if optDryRun opts then "planning " else "moving ") ++ show i ++ "/" ++ show total
    Nothing         -> return ()

  result <- try (detectType src) :: IO (Either SomeException String)
  case result of
    Left e -> do
      putStrLn $
        opTag "op" <>
        (if optDryRun opts then "[dry-run] " else "") <>
        "Skipped " <> rel <> ": " <> show e
      logSkip h src (show e)
    Right mime -> do
      let subdir  = case applyRules (optCustomRules opts) fname of
                      Just folder -> folder
                      Nothing     -> mimeToDir mime
          destDir = root <> "/" <> subdir
      createDirectoryIfMissing True destDir
      dest <- uniqueDest destDir fname

      when (optVerbose opts) $
        putStrLn $
          opTag "op" <>
          (if optDryRun opts then "[dry-run] " else "[verbose] ") <>
          rel <> " classified as " <> mime

      if optDryRun opts
        then putStrLn $
          opTag "op" <>
          "[dry-run] " <> rel <> " -> " <> makeRelative root dest
        else do
          let isNoProgress = case mProgress of { Nothing -> True; _ -> False }

          when (optVerbose opts && not isNoProgress) $
            putStrLn $
              opTag "op" <>
              "[verbose] moving to " <> makeRelative root dest

          -- Actual move
          _ <- renameOrCopy src dest

          putStrLn $
            opTag "op" <>
            rel <> " -> " <> makeRelative root dest

          logMove h src dest


mimeToDir :: String -> String
mimeToDir mime
  | "text/"  `isPrefixOf` mime = "text"
  | "image/" `isPrefixOf` mime = "images"
  | otherwise                  = "other"
