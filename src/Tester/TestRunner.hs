-- | Runs TestSpecs against their declared scenarios and collects outcomes.
--
--   Each run:
--     1. Looks up the scenario by name (IO — may include preset-loaded ones).
--     2. Calls buildScenario to wipe and recreate test-root.
--     3. If testVary = True, randomises filenames via varyTestRoot.
--     4. Calls testRun with the root path.
--     5. Wraps any exception as a Fail result so the runner never crashes.

module Tester.TestRunner
  ( TestOutcome(..)
  , runAllTests
  , runSpec
  , runByScenario
  , passCount
  , failCount
  ) where

import Control.Exception (try, SomeException)
-- NOTE: Data.List (filter) intentionally not imported here — it shadowed
-- Prelude.filter and forced every call site to write Prelude.filter.

import Tester.TestTypes
import Tester.TestScenarios (findScenario, buildScenario)
import Tester.Build         (testRoot, varyTestRoot)
import Tester.Templates     (loadOptions)

-- ─── Result type ────────────────────────────────────────────────────────────

data TestOutcome = TestOutcome
  { outTestName :: String
  , outScenario :: String
  , outResult   :: TestResult
  } deriving (Show)

-- ─── Runners ────────────────────────────────────────────────────────────────

-- | Run every spec against all its declared scenarios.
runAllTests :: [TestSpec] -> IO [TestOutcome]
runAllTests = fmap concat . mapM runSpec

-- | Run one spec against all its declared scenarios.
runSpec :: TestSpec -> IO [TestOutcome]
runSpec spec = mapM (runAgainst spec) (testScenarios spec)

-- | Run every test that references a given scenario, against that scenario only.
runByScenario :: String -> [TestSpec] -> IO [TestOutcome]
runByScenario sname specs =
  let matching = filter (elem sname . testScenarios) specs
  in  mapM (\sp -> runAgainst sp sname) matching

-- ─── Internal ───────────────────────────────────────────────────────────────

runAgainst :: TestSpec -> String -> IO TestOutcome
runAgainst spec sname = do
  mScenario <- findScenario sname       -- IO: covers both static and preset-loaded
  case mScenario of
    Nothing ->
      return TestOutcome
        { outTestName = testName spec
        , outScenario = sname
        , outResult   = Fail $ "unknown scenario '" ++ sname ++ "'"
        }
    Just scenario -> do
      buildScenario testRoot scenario
      -- Honour testVary: randomise filenames after building the scenario.
      -- Useful for structural tests that should pass regardless of naming
      -- convention.  Most tests leave this False.
      if testVary spec
        then loadOptions >>= varyTestRoot
        else return ()
      result <- try (testRun spec testRoot) :: IO (Either SomeException TestResult)
      return TestOutcome
        { outTestName = testName spec
        , outScenario = sname
        , outResult   = case result of
            Right r -> r
            Left  e -> Fail $ "unhandled exception: " ++ show e
        }

-- ─── Summary helpers ────────────────────────────────────────────────────────

passCount :: [TestOutcome] -> Int
passCount = length . filter (\o -> outResult o == Pass)

failCount :: [TestOutcome] -> Int
failCount = length . filter (\o -> outResult o /= Pass)
