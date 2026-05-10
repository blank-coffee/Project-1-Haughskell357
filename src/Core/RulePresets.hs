{-# LANGUAGE DeriveGeneric #-}
module Core.RulePresets
  ( CustomRule(..)
  , RulePreset(..)
  , rulePresetsDir
  , listRulePresets
  , loadRulePreset
  , saveRulePreset
  , deleteRulePreset
  ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, encodeFile)
import GHC.Generics (Generic)
import System.Directory
  ( doesFileExist
  , createDirectoryIfMissing
  , listDirectory
  , removeFile
  )
import Control.Monad (when)
import System.FilePath ((</>), takeExtension)

data CustomRule = CustomRule
  { ruleKeyword :: String
  , ruleFolder  :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CustomRule
instance FromJSON CustomRule

data RulePreset = RulePreset
  { rulePresetName  :: String
  , rulePresetRules :: [CustomRule]
  } deriving (Show, Generic)

instance ToJSON RulePreset
instance FromJSON RulePreset

rulePresetsDir :: FilePath
rulePresetsDir = "presets" </> "rules"

listRulePresets :: IO [FilePath]
listRulePresets = do
  createDirectoryIfMissing True rulePresetsDir
  files <- listDirectory rulePresetsDir
  return [ rulePresetsDir </> f | f <- files, takeExtension f == ".json" ]

loadRulePreset :: FilePath -> IO (Either String RulePreset)
loadRulePreset path = do
  ex <- doesFileExist path
  if not ex
    then return (Left $ "File not found: " ++ path)
    else eitherDecodeFileStrict path

saveRulePreset :: RulePreset -> FilePath -> IO ()
saveRulePreset preset path = do
  createDirectoryIfMissing True rulePresetsDir
  encodeFile path preset

deleteRulePreset :: FilePath -> IO ()
deleteRulePreset path = doesFileExist path >>= \ex -> when ex (removeFile path)
