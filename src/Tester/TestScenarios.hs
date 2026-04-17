-- | Test scenarios.
--
--   A scenario is a precise, named filesystem state: a list of files with
--   exact paths (relative to the test root) and exact content.
--
--   TWO KINDS OF SCENARIO
--   ──────────────────────
--   1. Static (hardcoded here)
--      Define a TestScenario value directly.  Use this when tests depend on
--      specific filenames or file content (e.g. hash/dedupe tests that look
--      for "hello_copy-1.txt" or expect "hello world\n").
--
--   2. Preset-derived (loaded from presets/scenarios/*.json at runtime)
--      Create the structure via the tester menu and save it to
--      presets/scenarios/<name>.json.  The file is picked up automatically
--      and is also loadable from menu option 1 for interactive exploration.
--      Use this when a test only cares about structure — file counts, folder
--      layout — and empty content is acceptable (scanner tests, for example).
--
--   ADDING A STATIC SCENARIO
--   ─────────────────────────
--     1. Write a TestScenario value below (see duplicatesScenario).
--     2. Add it to staticScenarios.
--
--   ADDING A PRESET-DERIVED SCENARIO
--   ──────────────────────────────────
--     1. Build the preset via the tester menu, save to
--        presets/scenarios/<name>.json.
--     2. If specific file content is needed, add an entry to
--        presetContentMap (optionName -> content string).
--     3. Write your TestSpec in src/Tester/Tests/, reference the preset
--        name as the scenario string.  No other changes required.

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

import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.Mem (performMajorGC)
import Control.Monad (forM_)
import Data.List (find)

import Tester.Presets   (loadPreset, presetsDir)
import Tester.Templates (loadOptions)
import Tester.Types     (Preset(..), FileOption(..), FolderEntry(..), FileEntry(..))

-- ─── Types ──────────────────────────────────────────────────────────────────

data TestFile = TestFile
  { tfRelPath :: FilePath   -- relative to the scenario root
  , tfContent :: String
  } deriving (Show)

data TestScenario = TestScenario
  { scenarioName  :: String
  , scenarioDesc  :: String
  , scenarioFiles :: [TestFile]
  } deriving (Show)

-- ─── Scenario preset directory ──────────────────────────────────────────────

-- | Preset JSON files in this directory are automatically loaded as
--   test scenarios and are also available from the tester menu.
scenariosDir :: FilePath
scenariosDir = presetsDir </> "scenarios"

-- ─── Registry ───────────────────────────────────────────────────────────────

-- | All available scenarios: static definitions first, then any loaded from
--   presets/scenarios/*.json.  If a preset name clashes with a static
--   scenario name the static version wins (preset is silently skipped).
allScenarios :: IO [TestScenario]
allScenarios = do
  loaded <- loadPresetScenarios
  let staticNames = map scenarioName staticScenarios
      newOnes     = filter (\s -> scenarioName s `notElem` staticNames) loaded
  return (staticScenarios ++ newOnes)

-- | The hardcoded scenarios.  These are the authoritative source for tests
--   that depend on specific filenames or file content.
staticScenarios :: [TestScenario]
staticScenarios = [duplicatesScenario, nestedScenario, originalDataScenario]

scenarioNames :: IO [String]
scenarioNames = map scenarioName <$> allScenarios

findScenario :: String -> IO (Maybe TestScenario)
findScenario name = find (\s -> scenarioName s == name) <$> allScenarios

-- ─── Preset-derived scenario loading ────────────────────────────────────────

-- | Override file content for specific option names when loading preset-
--   derived scenarios.  Options absent from this list get empty content,
--   which is correct for structural tests (scanner, folder layout, etc.).
--   Add entries here when a preset-derived scenario needs hashed or
--   otherwise meaningful content.
--
--   Example:
--     presetContentMap = [("report", "report body\n"), ("notes", "notes\n")]
presetContentMap :: [(String, String)]
presetContentMap = []

-- | Scan scenariosDir for .json preset files and convert each to a
--   TestScenario.  Silently skips files that fail to parse or whose
--   required file options are missing from options.json.
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

-- ─── Preset → Scenario conversion ───────────────────────────────────────────

-- | Derive a TestScenario from a Preset.
--
--   File naming follows the same convention as buildPreset:
--     count = 1  →  optionName ++ ext           (e.g. "report.pdf")
--     count > 1  →  optionName ++ "_N" ++ ext   (e.g. "report_1.pdf")
--
--   File content comes from the supplied content map; option names absent
--   from the map receive empty content ("").
--
--   IMPORTANT: because preset builds use base option names (not variants),
--   tests written against preset-derived scenarios must reference those base
--   names (e.g. root </> "report_1.pdf"), not variant-renamed forms.  If a
--   test needs variant filenames define a static scenario instead.
scenarioFromPreset
  :: Preset
  -> [FileOption]       -- needed for file extensions
  -> [(String, String)] -- optionName -> file content
  -> TestScenario
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
               else [ TestFile (mkPath (base ++ "_" ++ show i ++ ext)) content
                    | i <- [1..n] ]

-- ─── Static scenario definitions ────────────────────────────────────────────

-- | Root with three files sharing identical content (duplicate group),
--   one distinct file, and one file used as a readonly rename/copy target.
--
--   Covers: hash, dedupe, groupByHash, removeOriginals, uniqueDest,
--           renameOrCopy tests.
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

-- | Root with a nested sub-directory.
--
--   Covers: scanner recursive traversal, path-prefix correctness tests.
--   Also includes a duplicate pair so hash/distinct tests can run here too.
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


-- | A scenario representing the group's original test data structure.
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

-- ─── Builder ────────────────────────────────────────────────────────────────

-- | Wipe root and recreate it from the scenario's file list.
--   performMajorGC runs first to flush any residual file handles (major GC
--   is more thorough than minor GC for finalizing lazy handles on Windows).
--   With strict ByteString in Core.Hash this is belt-and-suspenders, but
--   it guards against future lazy IO introduced elsewhere.
buildScenario :: FilePath -> TestScenario -> IO ()
buildScenario root scenario = do
  performMajorGC
  removePathForcibly root
  createDirectoryIfMissing True root
  forM_ (scenarioFiles scenario) $ \tf -> do
    let fullPath = root </> tfRelPath tf
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath (tfContent tf)
