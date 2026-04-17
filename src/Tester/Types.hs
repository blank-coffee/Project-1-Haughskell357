{-# LANGUAGE DeriveGeneric #-}
module Tester.Types
  ( Variant(..)
  , FileOption(..)
  , FileEntry(..)
  , FolderEntry(..)
  , Preset(..)
  , ManifestEntry(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

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