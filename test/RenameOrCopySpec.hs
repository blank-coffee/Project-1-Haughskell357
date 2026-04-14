module RenameOrCopySpec (spec) where

import Test.Hspec
import Core.Dedupe (renameOrCopy)
import System.Directory
import System.FilePath
import TestUtils (resetTestData)

spec :: Spec
spec = before_ resetTestData $ do

  it "falls back to copy when rename fails" $ do
    let src = "test-data" </> "readonly.txt"
    let dst = "test-data" </> "deleteme" </> "readonly.txt"

    -- Make file read-only so rename will fail
    setPermissions src =<< fmap (\p -> p { readable = True, writable = False }) (getPermissions src)

    renameOrCopy src dst

    exists <- doesFileExist dst
    exists `shouldBe` True
