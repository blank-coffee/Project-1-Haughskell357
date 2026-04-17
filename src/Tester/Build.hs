module Tester.Build
  ( testRoot
  , buildPreset
  , varyTestRoot
  , clearTestRoot
  , fullReset
  ) where

import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  , renameFile
  )
import System.FilePath ((</>), takeDirectory, takeFileName, takeExtension)
import Control.Monad (forM, forM_, when)
import Data.List (find)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)

import Tester.Types
import Tester.Scramble (applyVariant, pickVariant, enabledVariants)

-- ─── Constants ──────────────────────────────────────────────────────────────

testRoot :: FilePath
testRoot = "test-root"

-- | Sits beside test-root, not inside it, so the organizer never sees it.
--   Survives clearTestRoot (intentional — vary still works after a clear),
--   but is removed by clearTestRoot since a cleared root makes it stale.
manifestPath :: FilePath
manifestPath = "tester-manifest.json"

-- ─── Build ──────────────────────────────────────────────────────────────────

-- | Resolve a preset to disk, writing empty files at base names.
--   Saves a manifest so vary knows what to rename later.
buildPreset :: [FileOption] -> Preset -> IO ()
buildPreset options preset = do
  clearTestRoot
  createDirectoryIfMissing True testRoot
  entries <- fmap concat $ forM (presetStructure preset) $ \folder -> do
    let dir = if folderPath folder == "."
              then testRoot
              else testRoot </> folderPath folder
    createDirectoryIfMissing True dir
    fmap concat $ forM (folderFiles folder) $ \entry ->
      case find (\o -> optionName o == entryOption entry) options of
        Nothing -> do
          putStrLn $ "  Warning: no file option '"
            ++ entryOption entry ++ "' -- skipping."
          return []
        Just opt -> do
          let base = optionName opt
              ext  = optionExt  opt
              n    = entryCount entry
          if n == 1
            then do
              let fp = dir </> (base ++ ext)
              writeFile fp ""
              putStrLn $ "  created: " ++ fp
              return [ManifestEntry fp (optionName opt) Nothing]
            else forM [1..n] $ \i -> do
              let fp = dir </> (base ++ "_" ++ show i ++ ext)
              writeFile fp ""
              putStrLn $ "  created: " ++ fp
              return (ManifestEntry fp (optionName opt) (Just i))
  encodeFile manifestPath entries
  putStrLn $ "\nBuilt " ++ show (length entries) ++ " file(s).  Manifest saved."

-- ─── Vary ───────────────────────────────────────────────────────────────────

-- | Rename every file in test-root to a randomly chosen enabled variant.
--   Reads the manifest to know option + index per file, then updates it.
varyTestRoot :: [FileOption] -> IO ()
varyTestRoot options = do
  exists <- doesFileExist manifestPath
  if not exists
    then putStrLn "No manifest found — build a preset first."
    else do
      result <- eitherDecodeFileStrict manifestPath
        :: IO (Either String [ManifestEntry])
      case result of
        Left err ->
          putStrLn $ "Manifest parse error: " ++ err
        Right entries -> do
          updated <- forM entries $ \me ->
            case find (\o -> optionName o == mOptionName me) options of
              Nothing -> do
                putStrLn $ "  Skipping — option '"
                  ++ mOptionName me ++ "' not found."
                return me
              Just opt ->
                let vs = variants opt in
                if null (enabledVariants vs)
                  then do
                    putStrLn $ "  Skipping '"
                      ++ takeFileName (mCurrentPath me)
                      ++ "' — no enabled variants."
                    return me
                  else do
                    v <- pickVariant vs
                    let newName = applyVariant (optionExt opt) v (mIndex me)
                        oldPath = mCurrentPath me
                        newPath = takeDirectory oldPath </> newName
                    when (oldPath /= newPath) $ do
                      fileEx <- doesFileExist oldPath
                      if fileEx
                        then do
                          renameFile oldPath newPath
                          putStrLn $ "  varied: "
                            ++ takeFileName oldPath
                            ++ "  ->  " ++ newName
                        else
                          putStrLn $ "  Warning: file not found: " ++ oldPath
                    return me { mCurrentPath = newPath }
          encodeFile manifestPath updated
          putStrLn "\nVariation complete.  Manifest updated."

-- ─── Reset helpers ──────────────────────────────────────────────────────────

-- | Remove test-root and the manifest (which is now stale without a root).
clearTestRoot :: IO ()
clearTestRoot = do
  removePathForcibly testRoot
  removePathForcibly manifestPath
  putStrLn $ "Cleared " ++ testRoot ++ "/"

-- | Wipe test-root, the manifest, and all user-created presets/options.
--   Preserves presets/scenarios/ so committed scenario preset files survive.
fullReset :: IO ()
fullReset = do
  removePathForcibly testRoot
  removePathForcibly manifestPath
  -- Remove options.json and any user preset .json files, but leave
  -- the scenarios/ sub-directory untouched (those are version-controlled).
  presetsExists <- doesDirectoryExist "presets"
  when presetsExists $ do
    entries <- listDirectory "presets"
    forM_ entries $ \e ->
      when (takeExtension e == ".json") $
        removePathForcibly ("presets" </> e)
  putStrLn "Full reset complete."
  putStrLn "  Removed: test-root, manifest, options.json, user preset files."
  putStrLn "  Preserved: presets/scenarios/"
