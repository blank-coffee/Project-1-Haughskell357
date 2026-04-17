module Tester.TestTypes
  ( TestResult(..)
  , TestSpec(..)
  ) where

-- | The outcome of running a single test against a single scenario.
data TestResult
  = Pass
  | Fail String   -- human-readable failure reason
  deriving (Show, Eq)

-- | A self-contained test.  Each spec declares which scenarios it runs
--   against; the runner builds each scenario fresh before calling testRun.
--
--   Adding a new test:
--     1. Write a function  :: FilePath -> IO TestResult
--     2. Add a TestSpec to Tester.TestRegistry.allTests
--   Nothing else needs to change.
data TestSpec = TestSpec
  { testName      :: String
    -- ^ Shown in menus and result output.

  , testScenarios :: [String]
    -- ^ Scenario keys (must match a scenarioName in Tester.TestScenarios).
    --   The test is executed once per listed scenario.

  , testVary      :: Bool
    -- ^ When True the runner will randomise filenames (via varyTestRoot)
    --   after building the scenario.  Use for structural tests that should
    --   pass regardless of naming convention.  Most tests want False.

  , testRun       :: FilePath -> IO TestResult
    -- ^ Receives the path to the built test-root directory.
  }
