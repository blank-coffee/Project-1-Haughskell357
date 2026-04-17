module Tester.Tests.ScannerTests (scannerTests) where

import Data.List (isSuffixOf, isPrefixOf)
import Tester.TestTypes
import Core.Scanner (listFilesRecursive)

scannerTests :: [TestSpec]
scannerTests =
  [ TestSpec "scanner: finds inner.txt in nested sub-directory" ["nested"] False scannerNestedTest
  , TestSpec "scanner: finds hello(1).txt at top level" ["nested", "duplicates"] False scannerTopLevelTest
  , TestSpec "scanner: all returned paths are rooted under root" ["nested", "duplicates"] False scannerPrefixTest
  , TestSpec "scanner: returns at least one file" ["nested", "duplicates"] False scannerNonEmptyTest
  ]

scannerNestedTest :: FilePath -> IO TestResult
scannerNestedTest root = do
  files <- listFilesRecursive root
  return $ if any (isSuffixOf "inner.txt") files
    then Pass
    else Fail $ "inner.txt not found; paths returned: " ++ show files

scannerTopLevelTest :: FilePath -> IO TestResult
scannerTopLevelTest root = do
  files <- listFilesRecursive root
  return $ if any (isSuffixOf "hello(1).txt") files
    then Pass
    else Fail $ "hello(1).txt not found; paths returned: " ++ show files

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