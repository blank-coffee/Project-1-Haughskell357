module Core.Duplicate
  ( findAndHandleDuplicates
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad (forM_, when)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory
  ( createDirectoryIfMissing
  , copyFile
  , removeFile
  , doesFileExist
  )
import Core.Hash (sha256File)

findAndHandleDuplicates :: FilePath -> [FilePath] -> Bool -> IO ()
findAndHandleDuplicates root files removeOriginals = do
  pairs <- mapM (\f -> do h <- sha256File f; return (h, f)) files
  let groups = Map.fromListWith (++) $ map (\(h,f) -> (h, [f])) pairs
      dupGroups = filter (\(_,fs) -> length fs > 1) $ Map.toList groups
      deletemeDir = root </> "deleteme"
  createDirectoryIfMissing True deletemeDir
  forM_ dupGroups $ \(_, fs) -> case fs of
    (_keeper:rest) -> forM_ rest $ \f -> do
      let fileName = takeFileName f
          dest = deletemeDir </> fileName
      createDirectoryIfMissing True (takeDirectory dest)
      copyFile f dest
      when removeOriginals $ do
        exists <- doesFileExist f
        when exists (removeFile f)
    _ -> return ()