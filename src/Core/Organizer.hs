module Core.Organizer (organizeByType) where

import Core.Detect (detectType)
import Core.Dedupe (renameOrCopy, uniqueDest)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

organizeByType :: FilePath -> [FilePath] -> IO ()
organizeByType root files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")
  mapM_ (moveFile root) files

moveFile :: FilePath -> FilePath -> IO ()
moveFile root src = do
  result <- try (detectType src) :: IO (Either SomeException String)
  case result of
    Left e    -> putStrLn $ "Skipped " ++ src ++ ": " ++ show e
    Right mime -> do
      let subdir = mimeToDir mime
          destDir = root <> "/" <> subdir
      dest <- uniqueDest destDir (takeFileName src)
      _ <- renameOrCopy src dest
      putStrLn $ src ++ " -> " ++ dest

mimeToDir :: String -> String
mimeToDir mime
  | "text/" `isPrefixOf` mime = "text"
  | "image/" `isPrefixOf` mime = "images"
  | otherwise = "other"

