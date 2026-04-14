module DedupeSpec (spec) where

import Test.Hspec
import System.Directory
import Core.Dedupe (dedupe)
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "creates deleteme directory" $ do
    dedupe "test-data" False
    exists <- doesDirectoryExist "test-data/deleteme"
    exists `shouldBe` True

  it "moves duplicates into deleteme" $ do
    dedupe "test-data" False
    files <- listDirectory "test-data/deleteme"
    length files `shouldSatisfy` (> 0)
