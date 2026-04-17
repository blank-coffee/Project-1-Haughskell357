{-# LANGUAGE LambdaCase #-}
module Tester.Templates
  ( loadOptions
  , loadUserOptions
  , loadStaticOptions
  , saveUserOptions
  , saveStaticOptions
  , upsertOption
  , removeOption
  ) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Tester.Types

optionsFile, staticOptionsFile :: FilePath
optionsFile = "presets" </> "options.json"
staticOptionsFile = "presets" </> "scenarios" </> "options.json"

loadOptions :: IO [FileOption]
loadOptions = do
  u <- loadUserOptions
  s <- loadStaticOptions
  return $ foldl (flip upsertOption) s u

loadUserOptions, loadStaticOptions :: IO [FileOption]
loadUserOptions   = loadOptsFromFile optionsFile
loadStaticOptions = loadOptsFromFile staticOptionsFile

loadOptsFromFile :: FilePath -> IO [FileOption]
loadOptsFromFile path = doesFileExist path >>= \ex -> if not ex then return [] else eitherDecodeFileStrict path >>= \case
  Left _   -> return []
  Right os -> return os

saveUserOptions, saveStaticOptions :: [FileOption] -> IO ()
saveUserOptions os   = createDirectoryIfMissing True (takeDirectory optionsFile) >> encodeFile optionsFile os
saveStaticOptions os = createDirectoryIfMissing True (takeDirectory staticOptionsFile) >> encodeFile staticOptionsFile os

upsertOption :: FileOption -> [FileOption] -> [FileOption]
upsertOption o os = o : filter (\x -> optionName x /= optionName o) os

removeOption :: String -> [FileOption] -> [FileOption]
removeOption name = filter (\o -> optionName o /= name)