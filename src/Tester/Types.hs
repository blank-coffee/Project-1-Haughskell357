{-# LANGUAGE DeriveGeneric #-}
module Tester.Types
  ( Variant(..)
  , FileOption(..)
  , FileEntry(..)
  , FolderEntry(..)
  , Preset(..)
  , ManifestEntry(..)
  , NameStandard(..)
  , ShapeRule(..)
  , NameMapConfig(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as M

data Variant = Variant
  { variantLabel    :: String
  , singlePattern   :: String
  , numberedPattern :: String
  , variantEnabled  :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON   Variant
instance FromJSON Variant

data FileOption = FileOption
  { optionName :: String
  , optionExt  :: String
  , variants   :: [Variant]
  } deriving (Show, Eq, Generic)

instance ToJSON   FileOption
instance FromJSON FileOption

data FileEntry = FileEntry
  { entryOption :: String
  , entryCount  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON   FileEntry
instance FromJSON FileEntry

data FolderEntry = FolderEntry
  { folderPath  :: String
  , folderFiles :: [FileEntry]
  } deriving (Show, Eq, Generic)

instance ToJSON   FolderEntry
instance FromJSON FolderEntry

data Preset = Preset
  { presetName      :: String
  , presetStructure :: [FolderEntry]
  } deriving (Show, Eq, Generic)

instance ToJSON   Preset
instance FromJSON Preset

data ManifestEntry = ManifestEntry
  { mCurrentPath :: FilePath
  , mOptionName  :: String
  , mIndex       :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON   ManifestEntry
instance FromJSON ManifestEntry

-- --- New Standardization Types ---

data NameStandard = NameStandard
  { stdId      :: String
  , stdPattern :: String
  } deriving (Show, Eq, Generic)

instance ToJSON   NameStandard
instance FromJSON NameStandard

data ShapeRule = ShapeRule
  { ruleId      :: String
  , shapeTokens :: [String]
  , tokenMap    :: [(Int, String)]
  , targetStd   :: String
  , dictMap     :: M.Map String (M.Map String String)
  } deriving (Show, Eq, Generic)

instance ToJSON   ShapeRule
instance FromJSON ShapeRule

data NameMapConfig = NameMapConfig
  { standards  :: [NameStandard]
  , shapeRules :: [ShapeRule]
  } deriving (Show, Eq, Generic)

instance ToJSON   NameMapConfig
instance FromJSON NameMapConfig