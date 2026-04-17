-- | Tests for Core.Organizer.organizeByType

module Tester.Tests.OrganizerTests (organizerTests) where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.List (isPrefixOf, isSuffixOf)

import Tester.TestTypes
import Core.Organizer (organizeByType)
import Core.Scanner (listFilesRecursive)

organizerTests :: [TestSpec]
organizerTests =
  [ TestSpec
      { testName      = "organizer: sorts text and markdown into text/"
      , testScenarios = ["original-data"]
      , testVary      = False
      , testRun       = textSortingTest
      }

  , TestSpec
      { testName      = "organizer: sorts jpg images into images/"
      , testScenarios = ["original-data"]
      , testVary      = False
      , testRun       = imageSortingTest
      }

  , TestSpec
      { testName      = "organizer: original un-sorted files are removed"
      , testScenarios = ["original-data"]
      , testVary      = False
      , testRun       = originalRemovalTest
      }
  ]

-- ─── Test functions ──────────────────────────────────────────────────────────

textSortingTest :: FilePath -> IO TestResult
textSortingTest root = do
  files <- listFilesRecursive root
  organizeByType root files
  
  newFiles <- listFilesRecursive root
  let textExts = [".txt", ".md"]
      txtFiles = filter (\f -> any (`isSuffixOf` f) textExts) newFiles
      textDir  = root </> "text"
      offenders = filter (\f -> not (textDir `isPrefixOf` f)) txtFiles

  return $ if null offenders && not (null txtFiles)
    then Pass
    else Fail $ "Text files not in text/ directory: " ++ show offenders

imageSortingTest :: FilePath -> IO TestResult
imageSortingTest root = do
  files <- listFilesRecursive root
  organizeByType root files
  
  newFiles <- listFilesRecursive root
  let imgFiles = filter (\f -> ".jpg" `isSuffixOf` f) newFiles
      imgDir   = root </> "images"
      offenders = filter (\f -> not (imgDir `isPrefixOf` f)) imgFiles

  return $ if null offenders && not (null imgFiles)
    then Pass
    else Fail $ "Image files not in images/ directory: " ++ show offenders

originalRemovalTest :: FilePath -> IO TestResult
originalRemovalTest root = do
  files <- listFilesRecursive root
  organizeByType root files
  
  -- Since they move to root/text or root/images, the original 
  -- scattered paths should no longer exist.
  stillExists <- mapM doesFileExist files
  let remaining = [f | (f, ex) <- zip files stillExists, ex]
  
  return $ if null remaining
    then Pass
    else Fail $ "Original file paths were not cleaned up: " ++ show remaining