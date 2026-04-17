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

-- | A single naming variant for a file option.
--   singlePattern    : used when the file count is 1  (no number suffix)
--   numberedPattern  : used when count > 1;  {N} is replaced with the index
--
--   Example — "lowercase + dot separator":
--     singlePattern   = "assignment"
--     numberedPattern = "assignment.{N}"
data Variant = Variant
  { variantLabel    :: String  -- shown in the manage menu
  , singlePattern   :: String  -- e.g. "assignment"
  , numberedPattern :: String  -- e.g. "assignment.{N}"
  , variantEnabled  :: Bool    -- False = stored but excluded from vary
  } deriving (Show, Eq, Generic)

instance ToJSON   Variant
instance FromJSON Variant

-- | A file option (formerly "template").
--   Variants are auto-generated on creation; the user can then
--   add custom ones or toggle/remove existing ones in manage mode.
data FileOption = FileOption
  { optionName :: String     -- base name,  e.g. "Assignment"
  , optionExt  :: String     -- extension,  e.g. ".txt"
  , variants   :: [Variant]
  } deriving (Show, Eq, Generic)

instance ToJSON   FileOption
instance FromJSON FileOption

-- | One file-option entry inside a preset folder
data FileEntry = FileEntry
  { entryOption :: String  -- must match an optionName
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

-- | Written to disk after every build/vary so we know what to rename
data ManifestEntry = ManifestEntry
  { mCurrentPath :: FilePath   -- updated in-place after each vary
  , mOptionName  :: String
  , mIndex       :: Maybe Int  -- Nothing = count was 1, Just n = nth file
  } deriving (Show, Eq, Generic)

instance ToJSON   ManifestEntry
instance FromJSON ManifestEntry