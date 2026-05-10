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
import System.FilePath (takeFileName, makeRelative)
import System.IO (Handle)
import Control.Exception (try, SomeException)
import Data.Char (toLower)
import Data.List (isPrefixOf, isInfixOf)
import Control.Monad (when)

data OrganizeOptions = OrganizeOptions
  { optDryRun      :: Bool
  , optVerbose     :: Bool
  , optCustomRules :: [CustomRule]
  }

-- | Simple organizer: MIME-based only, no options. Used by tests.
organizeByType :: FilePath -> Handle -> [FilePath] -> IO ()
organizeByType root h files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")
  mapM_ (moveFile root h) files

-- | Full organizer: respects options including custom sort rules.
organizeByTypeWith :: OrganizeOptions -> FilePath -> Handle -> [FilePath] -> IO ()
organizeByTypeWith opts root h files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")
  let ignored = ["backup", ".backup", "_backup"]
      files'  = filter (\p -> takeFileName p `notElem` ignored) files
      total   = length files'
  mapM_ (\(i, fp) -> moveFileWith opts root h total i fp) (zip [1..] files')

-- | Dry-run wrapper: shows what would happen without moving anything.
organizeByTypeDryRun :: FilePath -> Handle -> [FilePath] -> IO ()
organizeByTypeDryRun root h files =
  organizeByTypeWith (OrganizeOptions True False []) root h files

-- ─── Internal ───────────────────────────────────────────────────────────────

-- | Check custom rules before falling back to MIME type (case-insensitive).
applyRules :: [CustomRule] -> String -> Maybe String
applyRules rules fname =
  case filter (\r -> map toLower (ruleKeyword r) `isInfixOf` map toLower fname) rules of
    (r:_) -> Just (ruleFolder r)
    []    -> Nothing

moveFile :: FilePath -> Handle -> FilePath -> IO ()
moveFile root h src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  case result of
    Left e     -> do
      putStrLn $ "Skipped " ++ src ++ ": " ++ show e
      logSkip h src (show e)
    Right mime -> do
      let subdir  = mimeToDir mime
          destDir = root <> "/" <> subdir
      dest <- uniqueDest destDir (takeFileName src)
      _ <- renameOrCopy src dest
      putStrLn $ src ++ " -> " ++ dest
      logMove h src dest

moveFileWith :: OrganizeOptions -> FilePath -> Handle -> Int -> Int -> FilePath -> IO ()
moveFileWith opts root h total i src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  let rel   = makeRelative root src
      fname = takeFileName src
  putStrLn $ "[progress] moving " ++ show i ++ "/" ++ show total
  case result of
    Left e -> do
      putStrLn $ "Skipped " ++ rel ++ ": " ++ show e
      logSkip h src (show e)
    Right mime -> do
      let subdir  = case applyRules (optCustomRules opts) fname of
                      Just folder -> folder
                      Nothing     -> mimeToDir mime
          destDir = root <> "/" <> subdir
      createDirectoryIfMissing True destDir
      dest <- uniqueDest destDir fname
      when (optVerbose opts && not (optDryRun opts)) $
        putStrLn $ "[verbose] " ++ rel ++ " classified as " ++ mime
      if optDryRun opts
        then do
          putStrLn $ "[dry-run] classify " ++ rel ++ " as " ++ mime
          putStrLn $ "[dry-run] " ++ rel ++ " -> " ++ makeRelative root dest
        else do
          when (optVerbose opts) $
            putStrLn $ "[verbose] moving to " ++ makeRelative root dest
          _ <- renameOrCopy src dest
          putStrLn $ rel ++ " -> " ++ makeRelative root dest
          logMove h src dest

mimeToDir :: String -> String
mimeToDir mime
  | "text/"  `isPrefixOf` mime = "text"
  | "image/" `isPrefixOf` mime = "images"
  | otherwise                  = "other"
