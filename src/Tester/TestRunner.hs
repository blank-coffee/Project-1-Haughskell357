-- | Runs TestSpecs against their declared scenarios and collects outcomes.

module Tester.TestRunner
  ( TestOutcome(..)
  , runAllTests
  , runSpec
  , runByScenario
  , passCount
  , failCount
  ) where

import Control.Exception (try, SomeException)

import Tester.TestTypes
import Tester.TestScenarios (findScenario, buildScenario)
import Tester.Build         (testRoot, varyTestRoot)
import Tester.Templates     (loadOptions)

data TestOutcome = TestOutcome
  { outTestName :: String
  , outScenario :: String
  , outResult   :: TestResult
  } deriving (Show)

runAllTests :: [TestSpec] -> IO [TestOutcome]
runAllTests = fmap concat . mapM runSpec

runSpec :: TestSpec -> IO [TestOutcome]
runSpec spec = mapM (runAgainst spec) (testScenarios spec)

runByScenario :: String -> [TestSpec] -> IO [TestOutcome]
runByScenario sname specs =
  let matching = [sp | sp <- specs, sname `elem` testScenarios sp]
  in  mapM (`runAgainst` sname) matching

runAgainst :: TestSpec -> String -> IO TestOutcome
runAgainst spec sname = do
  mScenario <- findScenario sname
  case mScenario of
    Nothing ->
      return TestOutcome
        { outTestName = testName spec
        , outScenario = sname
        , outResult   = Fail $ "unknown scenario '" ++ sname ++ "'"
        }
    Just scenario -> do
      buildScenario testRoot scenario
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

passCount :: [TestOutcome] -> Int
passCount = length . filter (\o -> outResult o == Pass)

failCount :: [TestOutcome] -> Int
failCount = length . filter (\o -> outResult o /= Pass)