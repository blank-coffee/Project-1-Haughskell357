module Tester.Templates
  ( loadOptions
  , saveOptions
  , upsertOption
  , removeOption
  , optionsFile
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import Tester.Types

optionsFile :: FilePath
optionsFile = "presets/options.json"

staticOptionsFile :: FilePath
staticOptionsFile = "presets" </> "scenarios" </> "options.json"

loadOptions :: IO [FileOption]
loadOptions = do
  userOpts <- loadOptsFromFile optionsFile
  staticOpts <- loadOptsFromFile staticOptionsFile
  -- Combine them. User options will overwrite static ones if names clash.
  return $ foldl (flip upsertOption) staticOpts userOpts

loadOptsFromFile :: FilePath -> IO [FileOption]
loadOptsFromFile path = do
  exists <- doesFileExist path
  if not exists
    then return []
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> do
          putStrLn $ "Warning: could not parse options file " ++ path ++ ": " ++ err
          return []
        Right os -> return os

saveOptions :: [FileOption] -> IO ()
saveOptions os = do
  createDirectoryIfMissing True "presets"
  encodeFile optionsFile os

-- | Insert or replace by optionName
upsertOption :: FileOption -> [FileOption] -> [FileOption]
upsertOption o os = o : filter (\x -> optionName x /= optionName o) os

removeOption :: String -> [FileOption] -> [FileOption]
removeOption name = filter (\o -> optionName o /= name)