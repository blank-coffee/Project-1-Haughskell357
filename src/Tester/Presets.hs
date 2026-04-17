module Tester.Presets
  ( loadPreset
  , savePreset
  , listPresets
  , presetsDir
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory
  ( doesFileExist
  , doesDirectoryExist
  , createDirectoryIfMissing
  , listDirectory
  )
import System.FilePath ((</>), takeExtension)
import Tester.Types

presetsDir :: FilePath
presetsDir = "presets"

-- | Returns Left on missing file or parse error
loadPreset :: FilePath -> IO (Either String Preset)
loadPreset path = do
  exists <- doesFileExist path
  if not exists
    then return (Left $ "File not found: " ++ path)
    else eitherDecodeFileStrict path

savePreset :: Preset -> FilePath -> IO ()
savePreset preset path = do
  createDirectoryIfMissing True presetsDir
  encodeFile path preset

-- | All .json files in presetsDir and presets/scenarios except options.json
listPresets :: IO [FilePath]
listPresets = do
  createDirectoryIfMissing True presetsDir
  files <- listDirectory presetsDir
  let rootPresets =
        [ presetsDir </> f
        | f <- files
        , takeExtension f == ".json"
        , f /= "options.json"
        ]
  
  let scDir = presetsDir </> "scenarios"
  scExists <- doesDirectoryExist scDir
  scPresets <- if scExists
    then do
      scFiles <- listDirectory scDir
      return
        [ scDir </> f
        | f <- scFiles
        , takeExtension f == ".json"
        , f /= "options.json"
        ]
    else return []

  return (rootPresets ++ scPresets)