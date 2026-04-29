module Core.Backup
  ( createBackup
  , restoreBackupFullReplace
  , cleanupBackup
  ) where

import System.Directory
  ( doesDirectoryExist
  , createDirectoryIfMissing
  , listDirectory
  , removeDirectoryRecursive
  , copyFile
  , doesFileExist
  , removeFile
  , getCurrentDirectory
  )
import System.FilePath
  ( (</>)
  , takeFileName
  , splitDirectories
  )
import Control.Monad (forM_, when)

backupRoot :: FilePath -> IO FilePath
backupRoot root = do
  cwd <- getCurrentDirectory
  let safeName =
        case reverse (splitDirectories root) of
          (n:_) | not (null n) -> n
          _                    -> "target"
  pure (cwd </> ".backup" </> "file-organizer" </> safeName)

createBackup :: FilePath -> IO ()
createBackup root = do
  br <- backupRoot root
  exists <- doesDirectoryExist br
  when exists (removeDirectoryRecursive br)
  createDirectoryIfMissing True br
  copyTree root br

restoreBackupFullReplace :: FilePath -> IO ()
restoreBackupFullReplace root = do
  br <- backupRoot root
  exists <- doesDirectoryExist br
  when exists $ do
    entries <- listDirectory root
    forM_ entries $ \e -> do
      let p = root </> e
      isDir  <- doesDirectoryExist p
      isFile <- doesFileExist p
      when isDir  (removeDirectoryRecursive p)
      when isFile (removeFile p)
    copyTree br root
    removeDirectoryRecursive br

cleanupBackup :: FilePath -> IO ()
cleanupBackup root = do
  br <- backupRoot root
  exists <- doesDirectoryExist br
  when exists (removeDirectoryRecursive br)

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
  entries <- listDirectory src
  forM_ entries $ \e -> do
    let s = src </> e
        d = dst </> e

    let ignored = ["backup", ".backup", "_backup"]
    when (takeFileName s `elem` ignored) $
      return ()

    isDir  <- doesDirectoryExist s
    isFile <- doesFileExist s
    when isDir $ do
      createDirectoryIfMissing True d
      copyTree s d
    when isFile $
      copyFile s d
