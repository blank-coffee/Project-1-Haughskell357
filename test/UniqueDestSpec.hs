module UniqueDestSpec (spec) where

import Test.Hspec
import Core.Dedupe (uniqueDest)
import System.FilePath ((</>))
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "generates a new filename when the target already exists" $ do
    let base = "test-data"
    dest <- uniqueDest base "hello.txt"
    dest `shouldBe` (base </> "hello-1.txt")
