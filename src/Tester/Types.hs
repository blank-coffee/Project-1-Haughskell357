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

-- ─── Standardization Types ───────────────────────────────────────────────────

-- | A naming standard: a unique human-readable ID and a pattern string such
--   as "{class}-{number}: {type} {counter}".
data NameStandard = NameStandard
  { stdId      :: String
  , stdPattern :: String
  } deriving (Show, Eq, Generic)

instance ToJSON   NameStandard
instance FromJSON NameStandard

-- | A rule that maps a particular token shape (and optionally a dominant
--   delimiter character) to a naming standard.
--
--   'ruleName'    — user-supplied unique key; used for upserts and display.
--   'shapeTokens' — ordered list of "Alpha" / "Num" strings describing the
--                   token shape; the primary matching criterion.
--   'ruleDelim'   — dominant non-alphanumeric character detected from the
--                   file used to build the rule (e.g. Just '-' for dash-
--                   separated names).  Nothing means the rule matches any
--                   delimiter style.  When multiple rules share the same
--                   shape, the one whose delimiter matches the file under
--                   inspection is preferred.
--   'tokenMap'    — list of (1-based token index, variable name) pairs that
--                   describe how token positions map to pattern variables.
--   'targetStd'   — stdId of the NameStandard to apply.
--   'dictMap'     — per-variable translation tables:
--                   variable name -> (raw token value -> translated value).
data ShapeRule = ShapeRule
  { ruleName    :: String
  , shapeTokens :: [String]
  , ruleDelim   :: Maybe Char
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