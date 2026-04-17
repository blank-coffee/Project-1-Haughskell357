module Main where

import Core.Scanner (listFilesRecursive)
import Core.Organizer (organizeByType)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let dir = if null args then "." else head args
  putStrLn $ "Organizing files in: " ++ dir
  files <- listFilesRecursive dir
  organizeByType dir files
  putStrLn "Done."
