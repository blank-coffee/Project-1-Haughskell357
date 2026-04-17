-- | Tests for Core.Hash.sha256File.
--
--   Adding a hash test: write a function  :: FilePath -> IO TestResult,
--   then add a TestSpec entry to hashTests.

module Tester.Tests.HashTests (hashTests) where

import System.FilePath ((</>))
import Tester.TestTypes
import Core.Hash (sha256File)

hashTests :: [TestSpec]
hashTests =
  [ TestSpec
      { testName      = "hash: identical files produce the same digest"
      , testScenarios = ["duplicates"]
      , testVary      = False
      , testRun       = hashIdenticalTest
      }

  , TestSpec
      { testName      = "hash: distinct files produce different digests"
      , testScenarios = ["duplicates", "nested"]
      , testVary      = False
      , testRun       = hashDistinctTest
      }
  ]

-- ─── Test functions ──────────────────────────────────────────────────────────

hashIdenticalTest :: FilePath -> IO TestResult
hashIdenticalTest root = do
  h1 <- sha256File (root </> "hello_copy-1.txt")
  h2 <- sha256File (root </> "hello(1).txt")
  return $ if h1 == h2
    then Pass
    else Fail $ "expected equal hashes, got:\n  " ++ h1 ++ "\n  " ++ h2

hashDistinctTest :: FilePath -> IO TestResult
hashDistinctTest root = do
  h1 <- sha256File (root </> "hello.txt")
  h2 <- sha256File (root </> "different.txt")
  return $ if h1 /= h2
    then Pass
    else Fail "expected different hashes for different-content files, but got the same value"
