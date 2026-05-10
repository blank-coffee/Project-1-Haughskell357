module Tester.NameMap
  ( loadNameMapConfig
  , saveNameMapConfig
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Tester.Types

configPath :: FilePath
configPath = "presets" </> "namemaps.json"

emptyConfig :: NameMapConfig
emptyConfig = NameMapConfig [] []

loadNameMapConfig :: IO NameMapConfig
loadNameMapConfig = do
  ex <- doesFileExist configPath
  if not ex then return emptyConfig else do
    res <- eitherDecodeFileStrict configPath
    case res of
      Left _ -> return emptyConfig
      Right c -> return c

saveNameMapConfig :: NameMapConfig -> IO ()
saveNameMapConfig cfg = do
  createDirectoryIfMissing True (takeDirectory configPath)
  encodeFile configPath cfg