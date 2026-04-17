module Tester.Scramble
  ( autoVariants
  , applyVariant
  , pickVariant
  , enabledVariants
  ) where

import Data.Char (toLower, toUpper)
import System.Random (randomRIO)
import Tester.Types

-- ─── Auto-generation ────────────────────────────────────────────────────────

-- | Build the 12 auto variants (3 cases × 4 separators) for a base name.
--   All start enabled.  The user can disable or remove them later.
--
--   Example base = "Assignment":
--     original  + underscore  ->  single="Assignment"   numbered="Assignment_{N}"
--     lowercase + dot         ->  single="assignment"   numbered="assignment.{N}"
--     uppercase + space       ->  single="ASSIGNMENT"   numbered="ASSIGNMENT {N}"
--     ... (12 total)
autoVariants :: String -> [Variant]
autoVariants base =
  [ Variant
      (cLabel ++ " + " ++ sLabel)
      cBase
      (cBase ++ sep ++ "{N}")
      True
  | (cBase, cLabel) <- cases
  , (sep,   sLabel) <- seps
  ]
  where
    cases = [ (base,             "original")
            , (map toLower base, "lowercase")
            , (map toUpper base, "uppercase")
            ]
    seps  = [ ("_", "underscore")
            , (".", "dot")
            , ("-", "dash")
            , (" ", "space")
            ]

-- ─── Pattern application ────────────────────────────────────────────────────

-- | Turn a Variant into a concrete filename (extension included).
--   index = Nothing  ->  use singlePattern   (count = 1)
--   index = Just n   ->  substitute {N} in numberedPattern
applyVariant :: String -> Variant -> Maybe Int -> String
applyVariant ext v Nothing  = singlePattern v ++ ext
applyVariant ext v (Just n) = subN (numberedPattern v) ++ ext
  where
    subN []                 = []
    subN ('{':'N':'}':rest) = show n ++ subN rest
    subN (c:rest)           = c : subN rest

-- ─── Selection helpers ──────────────────────────────────────────────────────

enabledVariants :: [Variant] -> [Variant]
enabledVariants = filter variantEnabled

-- | Pick a random enabled variant.
--   Falls back to the first variant (regardless of enabled state) if none
--   are enabled, so vary never crashes.
pickVariant :: [Variant] -> IO Variant
pickVariant [] = error "Tester.Scramble.pickVariant: option has no variants at all"
pickVariant vs =
  let pool = enabledVariants vs
      actual = if null pool then [head vs] else pool
  in (actual !!) <$> randomRIO (0, length actual - 1)