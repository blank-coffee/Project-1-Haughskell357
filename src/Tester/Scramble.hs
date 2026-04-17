module Tester.Scramble
  ( autoVariants
  , applyVariant
  , pickVariant
  , enabledVariants
  ) where

import Data.Char (toLower, toUpper)
import System.Random (randomRIO)
import Tester.Types

-- | Build the 12 auto variants (3 cases × 4 separators) for a base name.
autoVariants :: String -> [Variant]
autoVariants base =
  [ Variant (cLabel ++ " + " ++ sLabel) cBase (cBase ++ sep ++ "{N}") True
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

applyVariant :: String -> Variant -> Maybe Int -> String
applyVariant ext v Nothing  = singlePattern v ++ ext
applyVariant ext v (Just n) = subN (numberedPattern v) ++ ext
  where
    subN []                 = []
    subN ('{':'N':'}':rest) = show n ++ subN rest
    subN (c:rest)           = c : subN rest

enabledVariants :: [Variant] -> [Variant]
enabledVariants = filter variantEnabled

pickVariant :: [Variant] -> IO Variant
pickVariant [] = error "Tester.Scramble.pickVariant: option has no variants at all"
pickVariant vs =
  let pool = enabledVariants vs
      actual = if null pool then [head vs] else pool
  in (actual !!) <$> randomRIO (0, length actual - 1)