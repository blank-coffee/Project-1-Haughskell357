module Core.Organizer
  ( OrganizeOptions(..)
  , organizeByType
  , organizeByTypeWith
  , organizeByTypeDryRun
  ) where

import Core.Detect (detectType)
import Core.Dedupe (renameOrCopy, uniqueDest)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, makeRelative)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)
import Control.Monad (when)

data OrganizeOptions = OrganizeOptions
  { optDryRun  :: Bool
  , optVerbose :: Bool
  }

organizeByType :: FilePath -> [FilePath] -> IO ()
organizeByType root files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")
  mapM_ (moveFile root) files

organizeByTypeWith :: OrganizeOptions -> FilePath -> [FilePath] -> IO ()
organizeByTypeWith opts root files =
  if optDryRun opts
    then organizeByTypeDryRun root files
    else do
      createDirectoryIfMissing True (root <> "/text")
      createDirectoryIfMissing True (root <> "/images")
      createDirectoryIfMissing True (root <> "/other")

      let ignored = ["backup", ".backup", "_backup"]
          files'  = filter (\p -> takeFileName p `notElem` ignored) files
          total   = length files'
      mapM_ (\(i, fp) -> moveFileWith opts root total i fp) (zip [1..] files')

organizeByTypeDryRun :: FilePath -> [FilePath] -> IO ()
organizeByTypeDryRun root files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")

  let ignored = ["backup", ".backup", "_backup"]
      files'  = filter (\p -> takeFileName p `notElem` ignored) files
      total   = length files'
  mapM_ (\(i, fp) -> planMove root total i fp) (zip [1..] files')

moveFile :: FilePath -> FilePath -> IO ()
moveFile root src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  case result of
    Left e     -> putStrLn $ "Skipped " ++ src ++ ": " ++ show e
    Right mime -> do
      let subdir  = mimeToDir mime
          destDir = root <> "/" <> subdir
      dest <- uniqueDest destDir (takeFileName src)
      _ <- renameOrCopy src dest
      putStrLn $ src ++ " -> " ++ dest

moveFileWith :: OrganizeOptions -> FilePath -> Int -> Int -> FilePath -> IO ()
moveFileWith opts root total i src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  let rel = makeRelative root src
  putStrLn $ "[progress] moving " ++ show i ++ "/" ++ show total
  case result of
    Left e ->
      putStrLn $ "Skipped " ++ rel ++ ": " ++ show e

    Right mime -> do
      let subdir  = mimeToDir mime
          destDir = root <> "/" <> subdir
      dest <- uniqueDest destDir (takeFileName src)

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

planMove :: FilePath -> Int -> Int -> FilePath -> IO ()
planMove root total i src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  let rel = makeRelative root src
  putStrLn $ "[progress] planning " ++ show i ++ "/" ++ show total
  case result of
    Left e ->
      putStrLn $ "[dry-run] Skipped " ++ rel ++ ": " ++ show e

    Right mime -> do
      let subdir  = mimeToDir mime
          destDir = root <> "/" <> subdir
      dest <- uniqueDest destDir (takeFileName src)
      putStrLn $ "[dry-run] classify " ++ rel ++ " as " ++ mime
      putStrLn $ "[dry-run] " ++ rel ++ " -> " ++ makeRelative root dest

mimeToDir :: String -> String
mimeToDir mime
  | "text/"  `isPrefixOf` mime = "text"
  | "image/" `isPrefixOf` mime = "images"
  | otherwise                  = "other"

