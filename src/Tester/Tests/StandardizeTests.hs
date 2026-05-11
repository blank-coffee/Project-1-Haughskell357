module Tester.Tests.StandardizeTests (standardizeTests) where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import qualified Data.Map.Strict as M

import Tester.TestTypes
import Tester.Types
-- Notice we ONLY import StandardizeRunner here, NOT Menu
import Tester.StandardizeRunner (runStandardizeBatch)
import Core.Logger (withRunLog)

standardizeTests :: [TestSpec]
standardizeTests =
  [ TestSpec "standardize: handles collisions with (1), (2) suffixes" ["std-collision"] False collisionTest
  ]

collisionTest :: FilePath -> IO TestResult
collisionTest root = withRunLog root $ \h -> do
  let cfg = NameMapConfig
        { standards = [ NameStandard "TestStd" "Target" ]
        , shapeRules = 
            [ ShapeRule "RuleA" ["Alpha", "Num"] (Just '_') [(1, "var1"), (2, "var2")] "TestStd" M.empty
            ]
        }
      files = [ root </> "fileA_1.txt", root </> "fileB_1.txt", root </> "fileC_1.txt" ]
  
  _ <- runStandardizeBatch h False cfg files

  ex0 <- doesFileExist (root </> "Target.txt")
  ex1 <- doesFileExist (root </> "Target (1).txt")
  ex2 <- doesFileExist (root </> "Target (2).txt")

  return $ if ex0 && ex1 && ex2
    then Pass
    else Fail $ "Expected Target.txt, Target (1).txt, Target (2).txt. Got: " 
                ++ show [ex0, ex1, ex2]