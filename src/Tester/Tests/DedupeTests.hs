-- | Tests for Core.Dedupe: dedupe, groupByHash, uniqueDest, renameOrCopy,
--   and the removeOriginals flag.

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
  [ TestSpec
      { testName      = "dedupe: creates deleteme directory"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = dedupeCreatesDirTest
      }

  , TestSpec
      { testName      = "dedupe: moves duplicate files into deleteme"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = dedupeMovesDuplicatesTest
      }

  , TestSpec
      { testName      = "dedupe: groupByHash groups identical files together"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = groupByHashTest
      }

  , TestSpec
      { testName      = "dedupe: removeOriginals leaves no duplicate groups in root"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = removeOriginalsTest
      }

  , TestSpec
      { testName      = "dedupe: uniqueDest avoids overwriting an existing file"
      , testScenarios = ["duplicates", "nested"]
      , testVary      = False
      , testRun       = uniqueDestTest
      }

  , TestSpec
      { testName      = "dedupe: renameOrCopy places file at destination"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = renameOrCopyTest
      }
  ]

-- ─── Test functions ──────────────────────────────────────────────────────────

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

-- | groupByHash should find at least one group of identical files.
groupByHashTest :: FilePath -> IO TestResult
groupByHashTest root = do
  files  <- listFilesRecursive root
  hashed <- mapM (\f -> do h <- sha256File f; return (f, h)) files
  let groups = groupByHash hashed
  return $ if not (null groups)
    then Pass
    else Fail "groupByHash found no duplicate groups (expected at least one)"

-- | After dedupe with removeOriginals=True, re-scanning the root (excluding
--   the deleteme folder) should reveal no remaining duplicate groups.
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

-- | uniqueDest should append "-1" (then "-2", etc.) when the target exists.
uniqueDestTest :: FilePath -> IO TestResult
uniqueDestTest root = do
  dest <- uniqueDest root "hello.txt"
  let expected = root </> "hello-1.txt"
  return $ if dest == expected
    then Pass
    else Fail $ "expected uniqueDest to return '" ++ expected
      ++ "' but got '" ++ dest ++ "'"

-- | renameOrCopy must place the file at the destination regardless of
--   whether it renamed or fell back to copying.
--   Always restores the writable bit so the next buildScenario can
--   wipe test-root cleanly on Windows.
renameOrCopyTest :: FilePath -> IO TestResult
renameOrCopyTest root = do
  let src = root </> "readonly.txt"
      dst = root </> "deleteme" </> "readonly.txt"

  createDirectoryIfMissing True (root </> "deleteme")

  perms <- getPermissions src
  setPermissions src (setOwnerWritable False perms)

  outcome <- try (renameOrCopy src dst) :: IO (Either SomeException Bool)

  -- Always restore writable so the next test's buildScenario can delete
  -- test-root without a permission-denied error on Windows.
  srcStillExists <- doesFileExist src
  when srcStillExists $ setPermissions src (setOwnerWritable True perms)

  case outcome of
    Left e  -> return $ Fail $ "renameOrCopy threw an exception: " ++ show e
    Right _ -> do
      exists <- doesFileExist dst
      return $ if exists
        then Pass
        else Fail "file did not appear at destination after renameOrCopy"