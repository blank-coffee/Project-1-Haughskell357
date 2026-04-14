module FileOrganizerSpec (spec) where

import Test.Hspec
import System.Process (readProcess)
import Data.List (isInfixOf)
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "finds nested inner.txt and top-level files in test-data" $ do
    out <- readProcess "stack" ["run", "--", "test-data", "1000"] ""
    out `shouldSatisfy` isInfixOf "inner.txt"
    out `shouldSatisfy` isInfixOf "hello.txt"
