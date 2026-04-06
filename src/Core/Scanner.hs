module Core.Scanner (listFiles) where

import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (filterM)

listFiles :: FilePath -> IO [FilePath]
listFiles dir = do
  names <- listDirectory dir
  let paths = map (dir </>) names
  filterM doesFileExist paths
