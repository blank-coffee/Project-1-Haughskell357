module Core.Scanner (listFilesRecursive) where

import System.Directory
  ( listDirectory
  , doesDirectoryExist
  , doesFileExist
  , getPermissions
  , readable
  , Permissions
  )
import System.FilePath ((</>))
import Control.Monad (filterM, forM)
import qualified Data.Set as Set
import Control.Exception (try, SomeException)

-- Public API: list all regular files under a directory (non-following symlink loops)
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive root = go Set.empty [root]
  where
    go _ [] = return []
    go seen (d:ds) = do
      eres <- try (listDirectory d) :: IO (Either SomeException [FilePath])
      case eres of
        Left _ -> go seen ds
        Right names -> do
          let paths = map (d </>) names
          files <- filterM doesFileExist paths
          dirs  <- filterM doesDirectoryExist paths
          readableDirs <- filterM isReadable dirs
          let newDirs = filter (\p -> not (Set.member p seen)) readableDirs
              seen' = foldr Set.insert seen newDirs
          rest <- go seen' (newDirs ++ ds)
          return (files ++ rest)

isReadable :: FilePath -> IO Bool
isReadable p = do
  eres <- try (getPermissions p) :: IO (Either SomeException Permissions)
  case eres of
    Left _    -> return False
    Right perms -> return (readable perms)