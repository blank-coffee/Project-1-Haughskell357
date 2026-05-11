module Main where

import System.Environment (getArgs)
import System.IO (Handle, hFlush, stdout, hIsTerminalDevice, stdin)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Control.Monad (unless, when)
import System.FilePath (makeRelative)
import System.Exit (exitFailure)

import Core.Logger (withRunLog)
import Core.Scanner (listFilesRecursive)
import Core.RulePresets (CustomRule(..))
import Core.Organizer
  ( OrganizeOptions(..)
  , organizeByTypeWith
  )
import Core.Detect (detectType)
import Core.Backup
  ( createBackup
  , restoreBackupFullReplace
  , cleanupBackup
  )

parseRule :: String -> Maybe CustomRule
parseRule s = case stripPrefix "--rule=" s of
  Nothing  -> Nothing
  Just val -> case break (== ':') val of
    (kw, ':':folder) | not (null kw) && not (null folder) -> Just (CustomRule kw folder)
    _ -> Nothing

parseFlags :: [String] -> (OrganizeOptions, Bool, Bool, Bool, Bool, FilePath)
parseFlags args =
  let (flags, rest) = span ("--" `isPrefixOf`) args
      opts = OrganizeOptions
        { optDryRun      = "--dry-run" `elem` flags
        , optVerbose     = "--verbose" `elem` flags
        , optCustomRules = mapMaybe parseRule flags
        }
      undoMode    = "--undo"      `elem` flags
      cleanupMode = "--cleanup"   `elem` flags
      scanMode    = "--scan"      `elem` flags
      noPrompt    = "--no-prompt" `elem` flags
      dir = case rest of
              []    -> "<NO_ROOT>"
              (d:_) -> d
  in (opts, undoMode, cleanupMode, scanMode, noPrompt, dir)

main :: IO ()
main = do
  args <- getArgs
  let (opts, undoMode, cleanupMode, scanMode, noPrompt, root) = parseFlags args

  when (root == "<NO_ROOT>") $ do
    putStrLn "Error: No folder provided."
    putStrLn "Usage: file-organizer [FLAGS] <folder>"
    putStrLn "Example: file-organizer --dry-run C:\\Users\\you\\Downloads"
    exitFailure

  if scanMode
    then withRunLog root $ \h -> runScan opts root h
    else if undoMode
      then do
        putStrLn "[progress] undoing 1/1"
        restoreBackupFullReplace root
        putStrLn "Undo complete."
      else if cleanupMode
        then do
          putStrLn "[progress] cleaning 1/1"
          cleanupBackup root
          putStrLn "Backup removed."
        else
          withRunLog root $ \h -> runNormal opts noPrompt root h

runScan :: OrganizeOptions -> FilePath -> Handle -> IO ()
runScan opts root h = do
  putStrLn $ "-- scanning " ++ root ++ "..."
  files <- listFilesRecursive h root
  let total = length files
  mapM_ (\(i, fp) -> printScanInfo opts root i total fp) (zip [1..] files)
  putStrLn "-- scan complete."

printScanInfo :: OrganizeOptions -> FilePath -> Int -> Int -> FilePath -> IO ()
printScanInfo opts root i total fp = do
  let rel = makeRelative root fp
  putStrLn $ "[progress] scanning " ++ show i ++ "/" ++ show total
  if optVerbose opts
    then do
      mime <- detectType fp
      putStrLn $ "[verbose] " ++ rel ++ " classified as " ++ mime
    else pure ()
  putStrLn rel

runNormal :: OrganizeOptions -> Bool -> FilePath -> Handle -> IO ()
runNormal opts noPrompt root h = do
  putStrLn $
    "Organizing files in: " ++ root
    ++ (if optDryRun opts then " (dry run)" else "")

  unless (optDryRun opts) $
    createBackup root

  files <- listFilesRecursive h root
  organizeByTypeWith opts root h files

  putStrLn "Sorting complete."

  if optDryRun opts
    then putStrLn "Dry run: no backup created, no undo available."
    else if noPrompt
      then putStrLn "Non-interactive run: backup preserved for external undo."
      else do
        isTTY <- hIsTerminalDevice stdin
        if isTTY
          then do
            putStr   "Undo last sort? (y/n): "
            hFlush stdout
            ans <- getLine
            if ans `elem` ["y","Y","yes","YES"]
              then do
                putStrLn "[progress] undoing 1/1"
                restoreBackupFullReplace root
                putStrLn "Undo complete."
              else do
                putStrLn "[progress] cleaning 1/1"
                cleanupBackup root
                putStrLn "Backup removed."
          else do
            putStrLn "[progress] cleaning 1/1"
            cleanupBackup root
            putStrLn "Backup removed."
