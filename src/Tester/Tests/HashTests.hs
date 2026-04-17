module Tester.Tests.HashTests (hashTests) where

import System.FilePath ((</>))
import Tester.TestTypes
import Core.Hash (sha256File)

hashTests :: [TestSpec]
hashTests =
  [ TestSpec "hash: identical files produce the same digest" ["duplicates"] False hashIdenticalTest
  , TestSpec "hash: distinct files produce different digests" ["duplicates", "nested"] False hashDistinctTest
  ]

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