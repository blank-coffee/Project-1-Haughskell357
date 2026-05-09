module Tester.Tests.OrganizerTests (organizerTests) where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.List (isPrefixOf, isSuffixOf)

import Tester.TestTypes
import Core.Logger (withRunLog)
import Core.Organizer (organizeByType)
import Core.Scanner (listFilesRecursive)

organizerTests :: [TestSpec]
organizerTests =
  [ TestSpec "organizer: sorts text and markdown into text/" ["original-data"] False textSortingTest
  , TestSpec "organizer: sorts jpg images into images/" ["original-data"] False imageSortingTest
  , TestSpec "organizer: original un-sorted files are removed" ["original-data"] False originalRemovalTest
  ]

textSortingTest :: FilePath -> IO TestResult
textSortingTest root = withRunLog root $ \h -> do
  files <- listFilesRecursive h root
  organizeByType root h files
  newFiles <- listFilesRecursive h root
  let txtFiles  = filter (\f -> any (`isSuffixOf` f) [".txt", ".md"]) newFiles
      textDir   = root </> "text"
      offenders = filter (not . isPrefixOf textDir) txtFiles
  return $ if null offenders && not (null txtFiles)
    then Pass
    else Fail $ "Text files not in text/ directory: " ++ show offenders

imageSortingTest :: FilePath -> IO TestResult
imageSortingTest root = withRunLog root $ \h -> do
  files <- listFilesRecursive h root
  organizeByType root h files
  newFiles <- listFilesRecursive h root
  let imgFiles  = filter (".jpg" `isSuffixOf`) newFiles
      imgDir    = root </> "images"
      offenders = filter (not . isPrefixOf imgDir) imgFiles
  return $ if null offenders && not (null imgFiles)
    then Pass
    else Fail $ "Image files not in images/ directory: " ++ show offenders

originalRemovalTest :: FilePath -> IO TestResult
originalRemovalTest root = withRunLog root $ \h -> do
  files <- listFilesRecursive h root
  organizeByType root h files
  stillExists <- mapM doesFileExist files
  let remaining = [f | (f, ex) <- zip files stillExists, ex]
  return $ if null remaining
    then Pass
    else Fail $ "Original file paths were not cleaned up: " ++ show remaining
