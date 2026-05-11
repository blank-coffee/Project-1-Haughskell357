module Tester.StandardizeRunner (runStandardizeBatch, findMatchingRules) where

import Control.Monad (foldM)
import Data.Char (toLower)
import Data.List (find, partition)
import System.FilePath ((</>), takeFileName, takeDirectory, dropExtension, takeExtension)
import System.Directory (doesFileExist, renameFile)
import System.IO (Handle)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Tester.Types (NameMapConfig(..), ShapeRule(..), NameStandard(..))
import Core.Standardize (tokenize, shapeOf, dominantDelim, extractTokens, applyStandard, hasUnresolved, sanitizeFileName)
import Core.Logger (logStandardize, logStdSkip)

findMatchingRules :: [String] -> Maybe Char -> [ShapeRule] -> [ShapeRule]
findMatchingRules shape delim rules =
  let shapeMatches = filter (\r -> shapeTokens r == shape) rules
      (delimMatches, otherMatches) = partition (\r -> ruleDelim r == delim) shapeMatches
  in  delimMatches ++ otherMatches

runStandardizeBatch :: Handle -> Bool -> NameMapConfig -> [FilePath] -> IO (S.Set String)
runStandardizeBatch h isDryRun cfg files = foldM processFile S.empty files
  where
    processFile claimed fp = do
      let base   = dropExtension (takeFileName fp)
          ext    = takeExtension fp
          dir    = takeDirectory fp
          tokens = tokenize base
          shape  = shapeOf tokens
          delim  = dominantDelim base
          rules  = findMatchingRules shape delim (shapeRules cfg)

      let stripDec ('*':xs) = stripDec xs
          stripDec ('@':xs) = stripDec xs
          stripDec xs       = xs

      let globalDict = M.fromListWith M.union $ do
            r <- shapeRules cfg
            (var, dict) <- M.toList (dictMap r)
            let globalPairs = [ (k, v) | (k, v) <- M.toList dict, '*' `elem` take 2 v ]
                autoPairs   = [ (map toLower (stripDec tgt), stripDec tgt) | (_, tgt) <- globalPairs ]
            return (var, M.fromList (globalPairs ++ autoPairs))

      let tryRules [] lastErr = do
            case lastErr of
              Just err -> do
                putStrLn $ "[std-skip] " ++ base ++ ext ++ ": " ++ err
                logStdSkip h fp err
              Nothing -> return ()
            return claimed

          tryRules (rule:rest) lastErr =
            case find (\s -> stdId s == targetStd rule) (standards cfg) of
              Nothing  -> tryRules rest (Just $ "unknown standard '" ++ targetStd rule ++ "'")
              Just std -> do
                let extracted = extractTokens (M.fromList (tokenMap rule)) (dictMap rule) globalDict tokens
                    rawBase   = applyStandard (stdPattern std) extracted
                if hasUnresolved rawBase
                  then tryRules rest (Just $ "unresolved variables -> " ++ rawBase)
                  else do
                    let baseDest = sanitizeFileName rawBase
                    
                    let findUnique n = do
                          let suffix = if n == 0 then "" else " (" ++ show n ++ ")"
                              testName = baseDest ++ suffix
                              testFp   = dir </> (testName ++ ext)
                              testFpLower = map toLower testFp
                          exists <- doesFileExist testFp
                          let isCaseChangeOnly = map toLower testFp == map toLower fp
                              isClaimed = S.member testFpLower claimed
                          if (exists && not isCaseChangeOnly) || isClaimed
                            then findUnique (n + 1)
                            else return (testName, testFp, testFpLower)

                    (finalBase, newFp, newFpLower) <- findUnique 0

                    if newFp == fp
                      then return claimed
                      else if isDryRun
                        then do
                          putStrLn $ "[dry-run]  " ++ base ++ ext ++ " -> " ++ finalBase ++ ext
                          return (S.insert newFpLower claimed)
                        else do
                          renameFile fp newFp
                          putStrLn $ "[std]      " ++ base ++ ext ++ " -> " ++ finalBase ++ ext
                          logStandardize h fp newFp
                          return (S.insert newFpLower claimed)

      tryRules rules Nothing