module Tester.Tests.DedupeTests (dedupeTests) where

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , getPermissions
  , setPermissions
  , setOwnerWritable
  , createDirectoryIfMissing
  )
import System.FilePath ((</>))
import Control.Monad (when)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

import Tester.TestTypes
import Core.Dedupe (dedupe, groupByHash, uniqueDest, renameOrCopy)
import Core.Hash (sha256File)
import Core.Scanner (listFilesRecursive)

dedupeTests :: [TestSpec]
dedupeTests =
  [ TestSpec "dedupe: creates deleteme directory" ["duplicates"] False dedupeCreatesDirTest
  , TestSpec "dedupe: moves duplicate files into deleteme" ["duplicates"] False dedupeMovesDuplicatesTest
  , TestSpec "dedupe: groupByHash groups identical files together" ["duplicates"] False groupByHashTest
  , TestSpec "dedupe: removeOriginals leaves no duplicate groups in root" ["duplicates"] False removeOriginalsTest
  , TestSpec "dedupe: uniqueDest avoids overwriting an existing file" ["duplicates", "nested"] False uniqueDestTest
  , TestSpec "dedupe: renameOrCopy places file at destination" ["duplicates"] False renameOrCopyTest
  ]

dedupeCreatesDirTest :: FilePath -> IO TestResult
dedupeCreatesDirTest root = do
  dedupe root False
  exists <- doesDirectoryExist (root </> "deleteme")
  return $ if exists
    then Pass
    else Fail "deleteme directory was not created by dedupe"

dedupeMovesDuplicatesTest :: FilePath -> IO TestResult
dedupeMovesDuplicatesTest root = do
  dedupe root False
  files <- listDirectory (root </> "deleteme")
  return $ if not (null files)
    then Pass
    else Fail "no files were moved into the deleteme directory"

groupByHashTest :: FilePath -> IO TestResult
groupByHashTest root = do
  files  <- listFilesRecursive root
  hashed <- mapM (\f -> do h <- sha256File f; return (f, h)) files
  let groups = groupByHash hashed
  return $ if not (null groups)
    then Pass
    else Fail "groupByHash found no duplicate groups (expected at least one)"

removeOriginalsTest :: FilePath -> IO TestResult
removeOriginalsTest root = do
  dedupe root True
  allFiles <- listFilesRecursive root
  let deletemeDir   = root </> "deleteme"
      rootOnlyFiles = filter (not . isPrefixOf deletemeDir) allFiles
  hashed <- mapM (\f -> do h <- sha256File f; return (f, h)) rootOnlyFiles
  let groups = groupByHash hashed
  return $ if null groups
    then Pass
    else Fail $ "duplicate groups still present after removeOriginals=True: "
      ++ show (map (map fst) groups)

uniqueDestTest :: FilePath -> IO TestResult
uniqueDestTest root = do
  dest <- uniqueDest root "hello.txt"
  let expected = root </> "hello-1.txt"
  return $ if dest == expected
    then Pass
    else Fail $ "expected uniqueDest to return '" ++ expected
      ++ "' but got '" ++ dest ++ "'"

renameOrCopyTest :: FilePath -> IO TestResult
renameOrCopyTest root = do
  let src = root </> "readonly.txt"
      dst = root </> "deleteme" </> "readonly.txt"

  createDirectoryIfMissing True (root </> "deleteme")

  perms <- getPermissions src
  setPermissions src (setOwnerWritable False perms)

  outcome <- try (renameOrCopy src dst) :: IO (Either SomeException Bool)

  srcStillExists <- doesFileExist src
  when srcStillExists $ setPermissions src (setOwnerWritable True perms)

  case outcome of
    Left e  -> return $ Fail $ "renameOrCopy threw an exception: " ++ show e
    Right _ -> do
      exists <- doesFileExist dst
      return $ if exists
        then Pass
        else Fail "file did not appear at destination after renameOrCopy"