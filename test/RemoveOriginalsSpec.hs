module RemoveOriginalsSpec (spec) where

import Test.Hspec
import Core.Dedupe (dedupe)
import System.Directory
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "removes originals when removeOriginals=True" $ do
    dedupe "test-data" True

    exists <- doesFileExist "test-data/hello_copy-1.txt"
    exists `shouldBe` False
