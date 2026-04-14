module Main where

import Core.Scanner (listFilesRecursive)
import Core.Detect (detectType)
import Core.Hash (sha256File)
import System.Environment (getArgs)
import Control.Exception (try, SomeException)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let dir = if null args then "." else head args
      limit = case args of
                (_:n:_) -> readMaybe n :: Maybe Int
                _       -> Nothing
  putStrLn $ "Dry-run: scanning directory " ++ dir
  files <- listFilesRecursive dir
  let filesToShow = maybe files (`take` files) limit
  mapM_ showInfo filesToShow

showInfo :: FilePath -> IO ()
showInfo f = do
  isFile <- doesFileExist f
  if not isFile
    then putStrLn $ f ++ " | skipped (not a regular file)"
    else do
      et <- try (detectType f) :: IO (Either SomeException String)
      eh <- try (sha256File f) :: IO (Either SomeException String)
      case (et, eh) of
        (Right t, Right h) -> putStrLn $ f ++ " | " ++ t ++ " | " ++ take 16 h ++ " ..."
        (Left e, _)        -> putStrLn $ f ++ " | detect error: " ++ show e
        (_, Left e)        -> putStrLn $ f ++ " | hash error: " ++ show e