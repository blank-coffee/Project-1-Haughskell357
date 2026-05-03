module Core.Organizer (organizeByType) where

import Core.Detect (detectType)
import Core.Dedupe (renameOrCopy, uniqueDest)
import Core.Logger (withRunLog, logMove, logSkip)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName)
import System.IO (Handle)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

organizeByType :: FilePath -> [FilePath] -> IO ()
organizeByType root files = do
  createDirectoryIfMissing True (root <> "/text")
  createDirectoryIfMissing True (root <> "/images")
  createDirectoryIfMissing True (root <> "/other")
  withRunLog root $ \h ->
    mapM_ (moveFile root h) files

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

mimeToDir :: String -> String
mimeToDir mime
  | "text/" `isPrefixOf` mime = "text"
  | "image/" `isPrefixOf` mime = "images"
  | otherwise = "other"

