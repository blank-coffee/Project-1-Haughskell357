module ScannerSpec (spec) where

import Test.Hspec
import Core.Scanner (listFilesRecursive)
import Data.List (isSuffixOf)
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "finds inner.txt in nested directories" $ do
    files <- listFilesRecursive "test-data"
    any (isSuffixOf "nested\\inner.txt") files `shouldBe` True

  it "finds hello(1).txt in top-level directory" $ do
    files <- listFilesRecursive "test-data"
    any (isSuffixOf "hello(1).txt") files `shouldBe` True
