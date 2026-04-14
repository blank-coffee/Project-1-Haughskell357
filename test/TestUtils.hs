module TestUtils (resetTestData) where

import System.Directory
import System.FilePath
import Control.Monad (forM_)

resetTestData :: IO ()
resetTestData = do
  removePathForcibly "test-data"
  copyDir "test-data-original" "test-data"

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectoryIfMissing True dst
  contents <- listDirectory src
  forM_ contents $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath
