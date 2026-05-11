{-# LANGUAGE LambdaCase #-}
module Core.Standardize
  ( Token(..)
  , tokenize
  , shapeOf
  , dominantDelim
  , extractPatternVars
  , extractTokens
  , hasUnresolved
  , applyStandard
  , sanitizeFileName
  ) where

import Data.Char (isAlpha, isDigit, isAlphaNum, toLower)
import Data.List (group, maximumBy, sort)
import Data.Ord  (comparing)
import qualified Data.Map.Strict as M

data Token = Alpha String String | Num Int deriving (Show, Eq, Ord)

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c:_)
  | isAlpha c = let (a, rest) = span isAlpha s in Alpha (map toLower a) a : tokenize rest
  | isDigit c = let (d, rest) = span isDigit s in Num (read d)            : tokenize rest
  | otherwise = tokenize (tail s)

shapeOf :: [Token] -> [String]
shapeOf = map (\case Alpha _ _ -> "Alpha"; Num _ -> "Num")

dominantDelim :: String -> Maybe Char
dominantDelim s =
  let ds = filter (not . isAlphaNum) s
  in if null ds then Nothing else Just . head . maximumBy (comparing length) . group . sort $ ds

extractPatternVars :: String -> [String]
extractPatternVars [] = []
extractPatternVars ('{':rest) =
  let (var, after) = break (== '}') rest
  in case after of
       ('}':tail') -> var : extractPatternVars tail'
       _           -> extractPatternVars rest
extractPatternVars (_:rest) = extractPatternVars rest

extractTokens 
  :: M.Map Int String                    -- tMap
  -> M.Map String (M.Map String String)  -- dMap (Local Rule)
  -> M.Map String (M.Map String String)  -- globalDict
  -> [Token] 
  -> M.Map String String
extractTokens tMap dMap globalDict tokens =
  let tokenEvals = do
        (idx, tok) <- zip [1..] tokens
        case M.lookup idx tMap of
          Nothing -> []
          Just varName -> do
            let (normVal, origVal) = case tok of Alpha n o -> (n, o); Num num -> (show num, show num)
                localDict   = M.findWithDefault M.empty varName dMap
                hasLocal    = M.member normVal localDict
                globalDictV = M.findWithDefault M.empty varName globalDict
                hasGlobal   = M.member normVal globalDictV
                
                isAnchored  = any (\v -> '@' `elem` take 2 v) (M.elems localDict)
                -- It is a "miss" if the rule has local knowledge for this variable, 
                -- but the current token isn't in it (and isn't in global).
                isMiss      = not (M.null localDict) && not hasLocal && not hasGlobal
                
            return (varName, normVal, origVal, localDict, globalDictV, isAnchored, isMiss)
      
      -- If any token misses on an anchored variable, the rule hard-fails.
      hasAnchorFail = any (\(_, _, _, _, _, isAnchored, isMiss) -> isAnchored && isMiss) tokenEvals
      
      -- If any token misses (but isn't anchored), trigger Sibling Mode.
      isSiblingMode = any (\(_, _, _, _, _, _, isMiss) -> isMiss) tokenEvals

  in if hasAnchorFail 
     then M.empty 
     else M.fromList $ do
       (varName, normVal, origVal, localDict, globalDictV, _, _) <- tokenEvals
       let globalTrans = M.lookup normVal globalDictV
       
       if isSiblingMode
       then -- SIBLING MODE: Ignore all local translations. Only use globals or pass-through.
         case globalTrans of
           Just gt -> return (varName, stripDecorations gt)
           Nothing -> return (varName, origVal)
           
       else -- STRICT MODE: Perfect match. Apply local translations, then global, then pass-through.
         let localTrans = M.lookup normVal localDict
         in case localTrans of
              Just t  -> return (varName, stripDecorations t)
              Nothing -> case globalTrans of
                           Just gt -> return (varName, stripDecorations gt)
                           Nothing -> return (varName, origVal)
  where
    stripDecorations ('*':rest) = stripDecorations rest
    stripDecorations ('@':rest) = stripDecorations rest
    stripDecorations s = s

hasUnresolved :: String -> Bool
hasUnresolved = ('{' `elem`)

applyStandard :: String -> M.Map String String -> String
applyStandard [] _ = []
applyStandard ('{':rest) vals =
  let (var, after) = break (== '}') rest
  in case after of
       ('}':tailStr) -> case M.lookup var vals of
                          Just val -> val ++ applyStandard tailStr vals
                          Nothing  -> "{" ++ var ++ "}" ++ applyStandard tailStr vals
       _             -> '{' : applyStandard rest vals
applyStandard (c:rest) vals = c : applyStandard rest vals

sanitizeFileName :: String -> String
sanitizeFileName = map (\c -> if c `elem` "<>:\"/\\|?*" then '-' else c)