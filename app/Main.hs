module Main where

import Core.Logger (withRunLog)
import Core.Scanner (listFilesRecursive)
import Core.Organizer (organizeByType)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let dir = if null args then "." else head args
  putStrLn $ "Organizing files in: " ++ dir
  withRunLog dir $ \h -> do
    files <- listFilesRecursive h dir
    organizeByType dir h files
  putStrLn "Done."
