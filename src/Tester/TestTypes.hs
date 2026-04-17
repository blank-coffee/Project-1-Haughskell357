module Tester.TestTypes
  ( TestResult(..)
  , TestSpec(..)
  ) where

data TestResult
  = Pass
  | Fail String
  deriving (Show, Eq)

data TestSpec = TestSpec
  { testName      :: String
  , testScenarios :: [String]
  , testVary      :: Bool
  , testRun       :: FilePath -> IO TestResult
  }