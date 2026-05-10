{-# LANGUAGE LambdaCase #-}
module Core.Standardize
  ( Token(..)
  , tokenize
  , shapeOf
  , extractTokens
  , applyStandard
  , sanitizeFileName
  ) where

import Data.Char (isAlpha, isDigit, toLower)
import qualified Data.Map.Strict as M

-- Lexical analysis chunk
data Token = Alpha String | Num Int deriving (Show, Eq, Ord)

-- Groups letters and numbers, entirely discarding delimiters
tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c:cs)
  | isAlpha c = let (a, rest) = span isAlpha s in Alpha (map toLower a) : tokenize rest
  | isDigit c = let (d, rest) = span isDigit s in Num (read d) : tokenize rest
  | otherwise = tokenize cs

-- Identifies the semantic shape regardless of actual content
shapeOf :: [Token] -> [String]
shapeOf = map (\case Alpha _ -> "Alpha"; Num _ -> "Num")

-- Uses the mapped indices to extract variables and runs them through semantic dictionaries
extractTokens :: M.Map Int String -> M.Map String (M.Map String String) -> [Token] -> M.Map String String
extractTokens tMap dMap tokens = M.fromList $ do
  (idx, tok) <- zip [1..] tokens
  case M.lookup idx tMap of
    Nothing -> []
    Just varName -> do
      let rawVal = case tok of Alpha a -> a; Num n -> show n
      -- Consult the specific dictionary for this variable, otherwise fallback to raw value
      let translated = case M.lookup varName dMap >>= M.lookup rawVal of
                         Just v  -> v
                         Nothing -> rawVal
      return (varName, translated)

-- Replaces standard '{var}' templates with extracted values
applyStandard :: String -> M.Map String String -> String
applyStandard [] _ = []
applyStandard ('{':rest) vals =
  let (var, after) = break (== '}') rest
  in case after of
       ('}':tailStr) -> case M.lookup var vals of
                          Just val -> val ++ applyStandard tailStr vals
                          Nothing  -> "{" ++ var ++ "}" ++ applyStandard tailStr vals
       _ -> '{' : applyStandard rest vals
applyStandard (c:rest) vals = c : applyStandard rest vals

-- Replaces OS-restricted characters with a hyphen to prevent filesystem crashes
sanitizeFileName :: String -> String
sanitizeFileName = map (\c -> if c `elem` "<>:\"/\\|?*" then '-' else c)