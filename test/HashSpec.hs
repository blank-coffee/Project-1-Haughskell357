module HashSpec (spec) where

import Test.Hspec
import Core.Hash (sha256File)
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "hashes identical files to the same value" $ do
    h1 <- sha256File "test-data/hello_copy-1.txt"
    h2 <- sha256File "test-data/hello(1).txt"
    h1 `shouldBe` h2

  it "hashes different files to different values" $ do
    h1 <- sha256File "test-data/hello_copy-1.txt"
    h2 <- sha256File "test-data/nested/inner.txt"
    h1 `shouldNotBe` h2
