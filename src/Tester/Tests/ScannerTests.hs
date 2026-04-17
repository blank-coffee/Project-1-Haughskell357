-- | Tests for Core.Scanner.listFilesRecursive.

module Tester.Tests.ScannerTests (scannerTests) where

import Data.List (isSuffixOf, isPrefixOf)
import System.FilePath ((</>))
import Tester.TestTypes
import Core.Scanner (listFilesRecursive)

scannerTests :: [TestSpec]
scannerTests =
  [ TestSpec
      { testName      = "scanner: finds inner.txt in nested sub-directory"
      , testScenarios = ["nested"]
      , testVary      = False
      , testRun       = scannerNestedTest
      }

  , TestSpec
      { testName      = "scanner: finds hello(1).txt at top level"
      , testScenarios = ["nested", "duplicates"]
      , testVary      = False
      , testRun       = scannerTopLevelTest
      }

  , TestSpec
      { testName      = "scanner: all returned paths are rooted under root"
      , testScenarios = ["nested", "duplicates"]
      , testVary      = False
      , testRun       = scannerPrefixTest
      }

  , TestSpec
      { testName      = "scanner: returns at least one file"
      , testScenarios = ["nested", "duplicates"]
      , testVary      = False
      , testRun       = scannerNonEmptyTest
      }
  ]

-- ─── Test functions ──────────────────────────────────────────────────────────

scannerNestedTest :: FilePath -> IO TestResult
scannerNestedTest root = do
  files <- listFilesRecursive root
  let found = any (isSuffixOf "inner.txt") files
  return $ if found
    then Pass
    else Fail $ "inner.txt not found; paths returned: " ++ show files

scannerTopLevelTest :: FilePath -> IO TestResult
scannerTopLevelTest root = do
  files <- listFilesRecursive root
  let found = any (isSuffixOf "hello(1).txt") files
  return $ if found
    then Pass
    else Fail $ "hello(1).txt not found; paths returned: " ++ show files

-- | Every path returned must begin with root so callers never see
--   paths that escaped the directory they asked to scan.
scannerPrefixTest :: FilePath -> IO TestResult
scannerPrefixTest root = do
  files <- listFilesRecursive root
  let offenders = filter (not . isPrefixOf root) files
  return $ if null offenders
    then Pass
    else Fail $ "paths outside root returned: " ++ show offenders

scannerNonEmptyTest :: FilePath -> IO TestResult
scannerNonEmptyTest root = do
  files <- listFilesRecursive root
  return $ if not (null files)
    then Pass
    else Fail "listFilesRecursive returned an empty list for a non-empty directory"
