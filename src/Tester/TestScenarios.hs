module Tester.TestScenarios
  ( TestFile(..)
  , TestScenario(..)
  , allScenarios
  , staticScenarios
  , scenarioNames
  , findScenario
  , buildScenario
  , scenarioFromPreset
  , scenariosDir
  ) where

import System.Directory (createDirectoryIfMissing, removePathForcibly, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.Mem (performMajorGC)
import Control.Monad (forM_)
import Data.List (find)

import Tester.Presets   (loadPreset, presetsDir)
import Tester.Templates (loadOptions)
import Tester.Types     (Preset(..), FileOption(..), FolderEntry(..), FileEntry(..))

data TestFile = TestFile
  { tfRelPath :: FilePath
  , tfContent :: String
  } deriving (Show)

data TestScenario = TestScenario
  { scenarioName  :: String
  , scenarioDesc  :: String
  , scenarioFiles :: [TestFile]
  } deriving (Show)

scenariosDir :: FilePath
scenariosDir = presetsDir </> "scenarios"

allScenarios :: IO [TestScenario]
allScenarios = do
  loaded <- loadPresetScenarios
  let staticNames = map scenarioName staticScenarios
      newOnes     = filter (\s -> scenarioName s `notElem` staticNames) loaded
  return (staticScenarios ++ newOnes)

staticScenarios :: [TestScenario]
staticScenarios = [duplicatesScenario, nestedScenario, originalDataScenario, stdCollisionScenario]

stdCollisionScenario :: TestScenario
stdCollisionScenario = TestScenario
  { scenarioName  = "std-collision"
  , scenarioDesc  = "Files that will collide during standardization"
  , scenarioFiles =
      [ TestFile "fileA_1.txt" "content A"
      , TestFile "fileB_1.txt" "content B"
      , TestFile "fileC_1.txt" "content C"
      ]
  }

scenarioNames :: IO [String]
scenarioNames = map scenarioName <$> allScenarios

findScenario :: String -> IO (Maybe TestScenario)
findScenario name = find (\s -> scenarioName s == name) <$> allScenarios

presetContentMap :: [(String, String)]
presetContentMap = []

loadPresetScenarios :: IO [TestScenario]
loadPresetScenarios = do
  exists <- doesDirectoryExist scenariosDir
  if not exists
    then return []
    else do
      files <- listDirectory scenariosDir
      opts  <- loadOptions
      let jsons = filter (\f -> takeExtension f == ".json") files
      results <- mapM (tryLoad opts) jsons
      return [ s | Just s <- results ]
  where
    tryLoad opts f = do
      result <- loadPreset (scenariosDir </> f)
      case result of
        Left  _      -> return Nothing
        Right preset -> return $ Just $ scenarioFromPreset preset opts presetContentMap

scenarioFromPreset :: Preset -> [FileOption] -> [(String, String)] -> TestScenario
scenarioFromPreset preset opts contentMap = TestScenario
  { scenarioName  = presetName preset
  , scenarioDesc  = "Preset: " ++ presetName preset
  , scenarioFiles = concatMap toFiles (presetStructure preset)
  }
  where
    toFiles folder =
      concatMap (entryFiles (folderPath folder)) (folderFiles folder)

    entryFiles dir entry =
      case find (\o -> optionName o == entryOption entry) opts of
        Nothing  -> []
        Just opt ->
          let base    = optionName opt
              ext     = optionExt  opt
              n       = entryCount entry
              content = maybe "" id (lookup base contentMap)
              mkPath name = if dir == "." then name else dir </> name
          in if n == 1
               then [ TestFile (mkPath (base ++ ext)) content ]
               else [ TestFile (mkPath (base ++ "_" ++ show i ++ ext)) content | i <- [1..n] ]

duplicatesScenario :: TestScenario
duplicatesScenario = TestScenario
  { scenarioName  = "duplicates"
  , scenarioDesc  = "Three identical files, one distinct file, one readonly candidate"
  , scenarioFiles =
      [ TestFile "hello.txt"        "hello world\n"
      , TestFile "hello_copy-1.txt" "hello world\n"
      , TestFile "hello(1).txt"     "hello world\n"
      , TestFile "different.txt"    "different content\n"
      , TestFile "readonly.txt"     "readonly content\n"
      ]
  }

nestedScenario :: TestScenario
nestedScenario = TestScenario
  { scenarioName  = "nested"
  , scenarioDesc  = "Root with a nested sub-directory containing inner.txt"
  , scenarioFiles =
      [ TestFile "hello.txt"        "hello world\n"
      , TestFile "hello(1).txt"     "hello world\n"
      , TestFile "different.txt"    "different content\n"
      , TestFile "nested/inner.txt" "inner file content\n"
      ]
  }

originalDataScenario :: TestScenario
originalDataScenario = TestScenario
  { scenarioName  = "original-data"
  , scenarioDesc  = "The group's original test data structure"
  , scenarioFiles =
      [ TestFile "hello(1).txt"        ""
      , TestFile "hello.txt"           ""
      , TestFile "hello_copy-1.txt"    ""
      , TestFile "image.jpg"           ""
      , TestFile "image_copy.jpg"      ""
      , TestFile "README.md"           ""
      , TestFile "README_copy.md"      ""
      , TestFile "readonly.txt"        ""
      , TestFile "salad-1.jpg"         ""
      , TestFile "salad.jpg"           ""
      , TestFile "nested/inner.txt"    ""
      , TestFile "nested/inner_copy.txt" ""
      ]
  }

buildScenario :: FilePath -> TestScenario -> IO ()
buildScenario root scenario = do
  performMajorGC
  removePathForcibly root
  createDirectoryIfMissing True root
  forM_ (scenarioFiles scenario) $ \tf -> do
    let fullPath = root </> tfRelPath tf
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath (tfContent tf)