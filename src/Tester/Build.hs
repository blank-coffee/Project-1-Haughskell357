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
import Control.Monad (forM, forM_, when, void)
import Data.List (find)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import System.Info (os)
import System.Process (spawnCommand)
import Control.Exception (try, SomeException)

import Tester.Types
import Tester.Scramble (applyVariant, pickVariant, enabledVariants)

testRoot :: FilePath
testRoot = "test-root"

manifestPath :: FilePath
manifestPath = "tester-manifest.json"

-- ─── OS Explorer Hooks ──────────────────────────────────────────────────────

openExplorer :: IO ()
openExplorer = do
  let script | os == "mingw32" = "explorer.exe \"test-root\""
             | os == "darwin"  = "open \"test-root\""
             | otherwise       = "xdg-open \"test-root\""
  void $ (try (void (spawnCommand script)) :: IO (Either SomeException ()))

closeExplorer :: IO ()
closeExplorer = do
  let script | os == "mingw32" = "powershell -Command \"(New-Object -comObject Shell.Application).Windows() | ? { $_.LocationName -eq 'test-root' } | % { $_.Quit() }\""
             | otherwise       = "wmctrl -c \"test-root\""
  void $ (try (void (spawnCommand script)) :: IO (Either SomeException ()))

-- ─── Build ──────────────────────────────────────────────────────────────────

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
          putStrLn $ "  Warning: no file option '" ++ entryOption entry ++ "' -- skipping."
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
  putStrLn $ "\nBuilt " ++ show (length entries) ++ " file(s). Manifest saved."
  openExplorer

-- ─── Vary ───────────────────────────────────────────────────────────────────

varyTestRoot :: [FileOption] -> IO ()
varyTestRoot options = do
  exists <- doesFileExist manifestPath
  if not exists
    then putStrLn "No manifest found — build a preset first."
    else do
      result <- eitherDecodeFileStrict manifestPath :: IO (Either String [ManifestEntry])
      case result of
        Left err -> putStrLn $ "Manifest parse error: " ++ err
        Right entries -> do
          updated <- forM entries $ \me ->
            case find (\o -> optionName o == mOptionName me) options of
              Nothing -> do
                putStrLn $ "  Skipping — option '" ++ mOptionName me ++ "' not found."
                return me
              Just opt ->
                let vs = variants opt in
                if null (enabledVariants vs)
                  then do
                    putStrLn $ "  Skipping '" ++ takeFileName (mCurrentPath me) ++ "' — no enabled variants."
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
                          putStrLn $ "  varied: " ++ takeFileName oldPath ++ "  ->  " ++ newName
                        else putStrLn $ "  Warning: file not found: " ++ oldPath
                    return me { mCurrentPath = newPath }
          encodeFile manifestPath updated
          putStrLn "\nVariation complete. Manifest updated."

-- ─── Reset helpers ──────────────────────────────────────────────────────────

clearTestRoot :: IO ()
clearTestRoot = do
  closeExplorer
  removePathForcibly testRoot
  removePathForcibly manifestPath
  putStrLn $ "Cleared " ++ testRoot ++ "/"

fullReset :: IO ()
fullReset = do
  closeExplorer
  removePathForcibly testRoot
  removePathForcibly manifestPath
  presetsExists <- doesDirectoryExist "presets"
  when presetsExists $ do
    entries <- listDirectory "presets"
    forM_ entries $ \e ->
      when (takeExtension e == ".json") $
        removePathForcibly ("presets" </> e)
  putStrLn "Full reset complete."
  putStrLn "  Removed: test-root, manifest, options.json, user preset files."
  putStrLn "  Preserved: presets/scenarios/"