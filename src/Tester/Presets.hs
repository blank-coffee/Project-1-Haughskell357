module Tester.Presets
  ( loadPreset
  , savePreset
  , deletePreset
  , listPresets
  , presetsDir
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, listDirectory, removeFile)
import Control.Monad (when)
import System.FilePath ((</>), takeExtension)
import Tester.Types

presetsDir :: FilePath
presetsDir = "presets"

loadPreset :: FilePath -> IO (Either String Preset)
loadPreset path = doesFileExist path >>= \ex -> if not ex then return (Left $ "File not found: " ++ path) else eitherDecodeFileStrict path

savePreset :: Preset -> FilePath -> IO ()
savePreset preset path = createDirectoryIfMissing True presetsDir >> encodeFile path preset

deletePreset :: FilePath -> IO ()
deletePreset path = doesFileExist path >>= \ex -> when ex (removeFile path)

listPresets :: IO [FilePath]
listPresets = do
  createDirectoryIfMissing True presetsDir
  rootFiles <- listDirectory presetsDir
  let rootPresets = [ presetsDir </> f | f <- rootFiles, takeExtension f == ".json", f /= "options.json" ]
  
  let scDir = presetsDir </> "scenarios"
  scExists <- doesDirectoryExist scDir
  scPresets <- if scExists
    then do
      scFiles <- listDirectory scDir
      return [ scDir </> f | f <- scFiles, takeExtension f == ".json", f /= "options.json" ]
    else return []

  return (rootPresets ++ scPresets)