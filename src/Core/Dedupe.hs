{-# LANGUAGE TupleSections #-}





module Core.Dedupe


  ( dedupe


  , groupByHash


  , uniqueDest


  , renameOrCopy


  ) where





import System.Directory


  ( createDirectoryIfMissing


  , renameFile


  , copyFile


  , removeFile


  , getPermissions


  , setPermissions


  , Permissions(..)


  , doesFileExist


  )


import System.FilePath


  ( takeDirectory


  , takeFileName


  , dropExtension


  , takeExtension


  , (</>)


  )


import Control.Exception (try, IOException)


import Control.Monad


import Data.List


import qualified Data.Map.Strict as M





import Core.Scanner (listFilesRecursive)


import Core.Hash (sha256File)





dedupe :: FilePath -> Bool -> IO ()


dedupe root removeOriginals = do


    files <- listFilesRecursive root


    hashed <- mapM (\f -> (f,) <$> sha256File f) files





    let groups = groupByHash hashed


    let deleteme = root </> "deleteme"





    createDirectoryIfMissing True deleteme





    forM_ groups $ \grp -> case grp of


        [] -> pure ()


        (keeper:rest) -> do


            putStrLn $ "Keeper: " ++ fst keeper


            forM_ rest $ \(src, _) -> do


                let name = takeFileName src


                dest <- uniqueDest deleteme name
                moved <- renameOrCopy src dest
                when (removeOriginals && not moved) $
                    removeFile src





groupByHash :: [(FilePath, String)] -> [[(FilePath, String)]]


groupByHash xs =


    let mp = M.fromListWith (++) [(h, [(p,h)]) | (p,h) <- xs]


    in filter (\g -> length g > 1) (M.elems mp)





uniqueDest :: FilePath -> FilePath -> IO FilePath


uniqueDest dir name = go 0


  where


    base = dropExtension name


    ext  = takeExtension name


    go n = do


        let candidate =


                if n == 0


                then dir </> name


                else dir </> (base ++ "-" ++ show n ++ ext)


        exists <- doesFileExist candidate


        if exists then go (n+1) else pure candidate





renameOrCopy :: FilePath -> FilePath -> IO Bool
renameOrCopy src dest = do
    result <- try (renameFile src dest) :: IO (Either IOException ())
    case result of
        Right _ -> pure True
        Left _  -> do
            -- Defensive: ensure destination parent directory exists
            createDirectoryIfMissing True (takeDirectory dest)
            copyFile src dest
            pure False

