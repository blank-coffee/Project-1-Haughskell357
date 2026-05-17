{-# LANGUAGE LambdaCase #-}
module Tester.Menu (runTester, runTesterSimple) where

import System.Console.Haskeline
import Control.Monad (forM_, when)
import Control.Exception (try, SomeException)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, find, nub)
import System.FilePath ((</>), takeFileName, takeDirectory, dropExtension, takeExtension)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IO (hFlush, stdout, stdin, hReady, hSetBuffering, BufferMode(..))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M

import Tester.Types
import Tester.Templates
import Tester.Presets
import Tester.NameMap
import Tester.Scramble (autoVariants)
import Tester.Build
import Tester.TestRegistry (allTests)
import Tester.TestScenarios (scenarioNames)
import Tester.TestRunner ( TestOutcome(..)
                         , runAllTests
                         , runByScenario
                         , passCount
                         , failCount
                         )
import Tester.TestTypes (TestResult(..))


import Control.Monad.IO.Class (liftIO)
import Tester.StandardizeRunner (runStandardizeBatch)

import Core.Standardize
import Core.Logger (withRunLog)
import Core.Scanner (listFilesRecursive)
import Core.Detect (detectType)
import Core.Hash (sha256File)
import Core.Dedupe (dedupe)
import Core.Organizer (OrganizeOptions(..), organizeByTypeWith)
import Core.RulePresets
  ( CustomRule(..), RulePreset(..)
  , rulePresetsDir
  , listRulePresets, loadRulePreset, saveRulePreset, deleteRulePreset
  )

colorHeader :: String -> String
colorHeader s = "\ESC[1;36m" ++ s ++ "\ESC[0m"

colorPass :: String -> String
colorPass s = "\ESC[1;32m" ++ s ++ "\ESC[0m"

colorFail :: String -> String
colorFail s = "\ESC[1;31m" ++ s ++ "\ESC[0m"

colorWarn :: String -> String
colorWarn s = "\ESC[1;33m" ++ s ++ "\ESC[0m"

colorInfo :: String -> String
colorInfo s = "\ESC[1;34m" ++ s ++ "\ESC[0m"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

clearStdin :: IO ()
clearStdin = do
  ready <- hReady stdin
  when ready $ do
    _ <- getChar
    clearStdin

askSimple :: String -> IO String
askSimple p = do
  clearStdin
  putStrLn p
  hFlush stdout
  line <- getLine
  return (trim line)

yesNoSimple :: String -> IO Bool
yesNoSimple p = do
  r <- askSimple (p ++ " [y/n]: \n")
  return $ map toLower r `elem` ["y", "yes"]

numbered :: [String] -> IO ()
numbered xs = forM_ (zip [1 :: Int ..] xs) $ \(i, x) ->
  putStrLn $ "  " ++ show i ++ ") " ++ x

parseInts :: String -> [Int]
parseInts = foldr (\w acc -> case readMaybe w of { Just n -> n:acc; Nothing -> acc }) [] . words

selectByIndices :: [Int] -> [a] -> [a]
selectByIndices idxs xs = [xs !! (i-1) | i <- idxs, i >= 1, i <= length xs]

normalizeExt :: String -> String
normalizeExt ""        = ".txt"
normalizeExt s@('.':_) = s
normalizeExt s         = '.' : s

showTok :: Token -> String
showTok (Alpha _ o) = "Alpha(\"" ++ o ++ "\")"
showTok (Num   n)   = "Num("    ++ show n ++ ")"

withTestRoot :: IO () -> IO ()
withTestRoot action = do
  exists <- doesDirectoryExist testRoot
  if exists then action else putStrLn (colorFail "  Error: test-root does not exist. Please build a preset first.")

runTester :: IO ()
runTester = runInputT defaultSettings (liftIO runTesterSimple)

runTesterSimple :: IO ()
runTesterSimple = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  mainMenu

mainMenu :: IO ()
mainMenu = do
  putStrLn $ colorHeader "\n===============================\n    File Organizer Tester\n==============================="
  putStrLn "  1) Load and build a preset"
  putStrLn "  2) Manage Presets"
  putStrLn "  3) Manage File Options"
  putStrLn "  4) Vary test-root files"
  putStrLn "  5) Clear test-root"
  putStrLn "  6) Full reset"
  putStrLn "  7) Run organizer"
  putStrLn "  8) Run tests"
  putStrLn "  9) Manage Sort Rules"
  putStrLn "  0) Quit"
  putStrLn "==============================="
  hFlush stdout
  choice <- askSimple "Choice: \n"
  case choice of
    "1" -> loadPresetMenu >> mainMenu
    "2" -> managePresetsMenu >> mainMenu
    "3" -> manageOptionsMenu >> mainMenu
    "4" -> withTestRoot (loadOptions >>= varyTestRoot) >> mainMenu
    "5" -> yesNoSimple "Clear test-root?" >>= \ok -> when ok clearTestRoot >> mainMenu
    "6" -> yesNoSimple "Remove test-root, manifest, options, and user presets?" >>= \ok -> when ok fullReset >> mainMenu
    "7" -> runOrganizerMenu >> mainMenu
    "8" -> runTestsMenu >> mainMenu
    "9" -> manageSortRulesMenu >> mainMenu
    "0" -> putStrLn "Goodbye!"
    _   -> putStrLn (colorWarn "Invalid choice.") >> mainMenu

loadPresetMenu :: IO ()
loadPresetMenu = do
  presets <- listPresets
  if null presets then putStrLn "No presets saved yet." else do
    putStrLn "\n-- Load Preset --"
    hFlush stdout
    numbered $ map (\p -> (if takeFileName (takeDirectory p) == "scenarios" then "[Static] " else "[User]   ") ++ takeFileName p) presets
    putStrLn "  0) Back"
    choice <- askSimple "Choice: \n"
    case readMaybe choice :: Maybe Int of
      Just 0 -> return ()
      Just n | n >= 1 && n <= length presets -> do
        res <- loadPreset (presets !! (n-1))
        case res of
          Left err -> putStrLn $ colorFail ("Error: " ++ err)
          Right p  -> do
            opts <- loadOptions
            buildPreset opts p
            putStrLn (colorInfo "Done!")
            vary <- yesNoSimple "Apply vary now?\n"
            when vary (varyTestRoot opts)
      _ -> putStrLn (colorWarn "Invalid choice.")

managePresetsMenu :: IO ()
managePresetsMenu = do
  putStrLn "\n-- Manage Presets --"
  putStrLn "  1) Create new"
  putStrLn "  2) Edit existing"
  putStrLn "  3) Delete"
  putStrLn "  0) Back"
  askSimple "Choice: \n" >>= \case
    "1" -> createPresetMenu >> managePresetsMenu
    "2" -> editPresetMenu >> managePresetsMenu
    "3" -> deletePresetMenu >> managePresetsMenu
    "0" -> return ()
    _   -> putStrLn (colorWarn "Invalid.") >> managePresetsMenu

createPresetMenu :: IO ()
createPresetMenu = do
  name <- askSimple "\nPreset name: \n"
  if null name then return () else do
    opts <- loadOptions
    if null opts then putStrLn "No file options yet." else do
      folders <- collectFolders opts []
      if null folders then putStrLn "Discarded." else do
        let preset = Preset name folders
        save <- yesNoSimple "Save?"
        when save $ do
          isStatic <- yesNoSimple "Save as static?"
          let path = (if isStatic then presetsDir </> "scenarios" else presetsDir)
                     </> map (\c -> if isSpace c then '-' else c) name ++ ".json"
          createDirectoryIfMissing True (takeDirectory path)
          savePreset preset path
          putStrLn $ colorInfo ("Saved to " ++ path)
        build <- yesNoSimple "Build test-root now?"
        when build $ do
          buildPreset opts preset
          yesNoSimple "Apply vary?" >>= \v -> when v (varyTestRoot opts)

editPresetMenu :: IO ()
editPresetMenu = do
  presets <- listPresets
  if null presets then putStrLn "No presets." else do
    putStrLn "\n-- Edit Preset --"
    numbered $ map (\p -> (if takeFileName (takeDirectory p) == "scenarios" then "[Static] " else "[User]   ") ++ takeFileName p) presets
    choice <- askSimple "Choice (0 to Cancel): \n"
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        let path = presets !! (n-1)
        res <- loadPreset path
        case res of
          Left e -> putStrLn $ colorFail ("Error: " ++ e)
          Right p -> do
            newNameRaw <- askSimple $ "New preset name (empty to keep '" ++ presetName p ++ "'): \n"
            let newName = if null newNameRaw then presetName p else newNameRaw
            opts <- loadOptions
            finalFolders <- modifyFolders opts (presetStructure p)
            isStatic <- yesNoSimple "Save as static?"
            let newPath = (if isStatic then presetsDir </> "scenarios" else presetsDir)
                           </> map (\c -> if isSpace c then '-' else c) newName ++ ".json"
            when (path /= newPath) $ deletePreset path
            createDirectoryIfMissing True (takeDirectory newPath)
            savePreset (p { presetName = newName, presetStructure = finalFolders }) newPath
            putStrLn $ colorInfo ("Saved to " ++ newPath)
      _ -> return ()

deletePresetMenu :: IO ()
deletePresetMenu = do
  presets <- listPresets
  if null presets then putStrLn "No presets." else do
    numbered $ map takeFileName presets
    choice <- askSimple "Delete which number (0 to Cancel)? \n"
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        let path = presets !! (n-1)
        ok <- yesNoSimple $ "Delete " ++ takeFileName path ++ "?"
        when ok $ deletePreset path >> putStrLn (colorInfo "Deleted.")
      _ -> return ()

modifyFolders :: [FileOption] -> [FolderEntry] -> IO [FolderEntry]
modifyFolders opts folders = do
  putStrLn "\n-- Current Folders --"
  if null folders
    then putStrLn "  (No folders)"
    else numbered (map (\f -> folderPath f ++ " (" ++ show (sum $ map entryCount $ folderFiles f) ++ " files)") folders)
  putStrLn "\n  a) Add folder"
  putStrLn "  d) Delete (e.g. d 1)"
  putStrLn "  e) Edit (e.g. e 1)"
  putStrLn "  b) Done"
  raw <- askSimple "Action: \n"
  let (cmd, restStr) = break isSpace raw
      rest = parseInts restStr
  case cmd of
    "a" -> do
      path <- askSimple "Folder path: \n"
      if null path
        then modifyFolders opts folders
        else do
          fs <- collectFiles opts []
          modifyFolders opts (folders ++ [FolderEntry path fs])
    "d" | not (null rest) -> do
        let idx = head rest
        if idx >= 1 && idx <= length folders
          then modifyFolders opts (take (idx - 1) folders ++ drop idx folders)
          else modifyFolders opts folders
    "e" | not (null rest) -> do
        let idx = head rest
        if idx >= 1 && idx <= length folders
          then do
            fs <- modifyFiles opts (folderPath (folders !! (idx-1))) (folderFiles (folders !! (idx-1)))
            modifyFolders opts (take (idx - 1) folders ++ [(folders !! (idx-1)) { folderFiles = fs }] ++ drop idx folders)
          else modifyFolders opts folders
    "b" -> return folders
    _   -> modifyFolders opts folders

modifyFiles :: [FileOption] -> String -> [FileEntry] -> IO [FileEntry]
modifyFiles opts path files = do
  putStrLn $ "\n-- Editing Files in '" ++ path ++ "' --"
  if null files
    then putStrLn "  (No files)"
    else numbered (map (\f -> show (entryCount f) ++ "x " ++ entryOption f) files)
  putStrLn "\n  a) Add files"
  putStrLn "  d) Delete (e.g. d 1)"
  putStrLn "  b) Done"
  raw <- askSimple "Action: \n"
  let (cmd, restStr) = break isSpace raw
      rest = parseInts restStr
  case cmd of
    "a" -> collectFiles opts [] >>= \fs -> modifyFiles opts path (files ++ fs)
    "d" | not (null rest) -> do
        let idx = head rest
        if idx >= 1 && idx <= length files
          then modifyFiles opts path (take (idx - 1) files ++ drop idx files)
          else modifyFiles opts path files
    "b" -> return files
    _   -> modifyFiles opts path files

collectFolders :: [FileOption] -> [FolderEntry] -> IO [FolderEntry]
collectFolders opts acc = do
  path <- askSimple "Folder path (empty to finish): \n"
  if null path
    then return acc
    else do
      fs <- collectFiles opts []
      collectFolders opts (acc ++ [FolderEntry path fs])

collectFiles :: [FileOption] -> [FileEntry] -> IO [FileEntry]
collectFiles opts acc = do
  numbered (map (\o -> optionName o ++ optionExt o) opts)
  choice <- askSimple "Select option (0 to Done): \n"
  case readMaybe choice :: Maybe Int of
    Just 0 -> return acc
    Just n | n >= 1 && n <= length opts -> do
      cstr <- askSimple $ "How many " ++ optionName (opts !! (n-1)) ++ " files? \n"
      case readMaybe cstr :: Maybe Int of
        Just c | c >= 1 -> collectFiles opts (acc ++ [FileEntry (optionName (opts !! (n-1))) c])
        _ -> putStrLn (colorWarn "Need >= 1.") >> collectFiles opts acc
    _ -> collectFiles opts acc

manageOptionsMenu :: IO ()
manageOptionsMenu = do
  uOpts <- loadUserOptions
  sOpts <- loadStaticOptions
  let refs = map (\o -> (o, True)) sOpts ++ map (\o -> (o, False)) uOpts
  putStrLn "\n-- File Options --"
  if null refs
    then putStrLn "  (none yet)"
    else numbered (map (\(o, isS) -> (if isS then "[Static] " else "[User]   ") ++ optionName o ++ optionExt o) refs)
  putStrLn "\n  1) Add new"
  putStrLn "  2) Edit/Manage variants"
  putStrLn "  3) Delete"
  putStrLn "  0) Back"
  askSimple "Choice: \n" >>= \case
    "1" -> addOptionMenu uOpts sOpts >> manageOptionsMenu
    "2" -> editOptionMenu refs uOpts sOpts >> manageOptionsMenu
    "3" -> deleteOptionMenu refs uOpts sOpts >> manageOptionsMenu
    "0" -> return ()
    _   -> putStrLn (colorWarn "Invalid.") >> manageOptionsMenu

addOptionMenu :: [FileOption] -> [FileOption] -> IO ()
addOptionMenu uOpts sOpts = do
  name <- askSimple "\nBase name: \n"
  if null name then return () else do
    ext <- normalizeExt <$> askSimple "Extension (e.g. .txt): "
    let opt = FileOption name ext (autoVariants name)
    isStatic <- yesNoSimple "Save as static?"
    if isStatic
      then saveStaticOptions (upsertOption opt sOpts)
      else saveUserOptions (upsertOption opt uOpts)
    putStrLn (colorInfo "Option created.")

editOptionMenu :: [(FileOption, Bool)] -> [FileOption] -> [FileOption] -> IO ()
editOptionMenu [] _ _ = return ()
editOptionMenu refs uOpts sOpts = do
  choice <- askSimple "Edit which number? \n"
  case readMaybe choice :: Maybe Int of
    Just n | n >= 1 && n <= length refs -> do
      let (opt, isStatic) = refs !! (n-1)
      updatedOpt <- manageVariantsMenu opt
      toggle <- yesNoSimple $ "Toggle status? (Currently " ++ (if isStatic then "[Static]" else "[User]") ++ ")"
      let finalStatic = if toggle then not isStatic else isStatic
          cleanU = removeOption (optionName opt) uOpts
          cleanS = removeOption (optionName opt) sOpts
      if finalStatic
        then do
          saveUserOptions cleanU
          saveStaticOptions (upsertOption updatedOpt cleanS)
        else do
          saveStaticOptions cleanS
          saveUserOptions (upsertOption updatedOpt cleanU)
      putStrLn (colorInfo "Updated.")
    _ -> return ()

deleteOptionMenu :: [(FileOption, Bool)] -> [FileOption] -> [FileOption] -> IO ()
deleteOptionMenu [] _ _ = return ()
deleteOptionMenu refs uOpts sOpts = do
  choice <- askSimple "Delete which number? \n"
  case readMaybe choice :: Maybe Int of
    Just n | n >= 1 && n <= length refs -> do
      let (opt, isStatic) = refs !! (n-1)
      ok <- yesNoSimple $ "Delete '" ++ optionName opt ++ "'?"
      when ok $
        if isStatic
          then saveStaticOptions (removeOption (optionName opt) sOpts)
          else saveUserOptions (removeOption (optionName opt) uOpts)
      putStrLn (colorInfo "Deleted.")
    _ -> return ()

manageVariantsMenu :: FileOption -> IO FileOption
manageVariantsMenu opt = do
  let vs = variants opt
  putStrLn $ "\n-- Variants for " ++ optionName opt ++ optionExt opt ++ " --"
  forM_ (zip [1 :: Int ..] vs) $ \(i, v) ->
    putStrLn $ "  " ++ show i ++ ") " ++ (if variantEnabled v then "[ON] " else "[OFF]") ++ " " ++ variantLabel v
  putStrLn "\n  a) Add custom"
  putStrLn "  r) Remove (e.g. r 1)"
  putStrLn "  e) Enable (e.g. e 1)"
  putStrLn "  d) Disable (e.g. d 1)"
  putStrLn "  b) Done"
  raw <- askSimple "Action: \n"
  let (cmd, restStr) = break isSpace raw
      rest = parseInts restStr
  case cmd of
    "a" -> do
      b <- askSimple "Custom base name: \n"
      if null b
        then manageVariantsMenu opt
        else manageVariantsMenu (opt { variants = vs ++ [Variant ("custom: " ++ b) b (b ++ "_{N}") True] })
    "r" | not (null rest) ->
      manageVariantsMenu (opt { variants = filter (\v -> variantLabel v `notElem` map variantLabel (selectByIndices rest vs)) vs })
    "e" | not (null rest) ->
      manageVariantsMenu (opt { variants = [ if i `elem` rest then v { variantEnabled = True } else v | (i, v) <- zip [1..] vs ] })
    "d" | not (null rest) ->
      manageVariantsMenu (opt { variants = [ if i `elem` rest then v { variantEnabled = False } else v | (i, v) <- zip [1..] vs ] })
    "b" -> return opt
    _   -> manageVariantsMenu opt

runOrganizerMenu :: IO ()
runOrganizerMenu = do
  putStrLn "\n-- Run Organizer --"
  putStrLn "  1) Dry-run scan"
  putStrLn "  2) Dedupe"
  putStrLn "  3) Full Organize"
  putStrLn "  4) Standardize Names"
  putStrLn "  0) Back"
  askSimple "Choice: \n" >>= \case
    "1" -> withTestRoot runDryScan >> runOrganizerMenu
    "2" -> withTestRoot runDedupeMenu >> runOrganizerMenu
    "3" -> withTestRoot pickRulesAndOrganize >> runOrganizerMenu
    "4" -> withTestRoot standardizeMenu >> runOrganizerMenu
    "0" -> return ()
    _   -> runOrganizerMenu

pickRulesAndOrganize :: IO ()
pickRulesAndOrganize = do
  rules <- pickRulePreset
  runFullOrganize rules

pickRulePreset :: IO [CustomRule]
pickRulePreset = do
  presets <- listRulePresets
  if null presets then return [] else do
    useRules <- yesNoSimple "Apply custom sort rules?"
    if not useRules then return [] else do
      putStrLn "\n-- Select Rule Preset --"
      numbered (map takeFileName presets)
      putStrLn "  0) None (type-based only)"
      choice <- askSimple "Choice: "
      case readMaybe choice :: Maybe Int of
        Just 0 -> return []
        Just n | n >= 1 && n <= length presets -> do
          res <- loadRulePreset (presets !! (n-1))
          case res of
            Left err -> putStrLn (colorFail ("Error: " ++ err)) >> return []
            Right p  -> do
              putStrLn $ colorInfo ("Using " ++ show (length (rulePresetRules p)) ++ " rule(s).")
              return (rulePresetRules p)
        _ -> return []

runFullOrganize :: [CustomRule] -> IO ()
runFullOrganize rules = withRunLog testRoot $ \h -> do
  files <- listFilesRecursive h testRoot
  if null files then putStrLn "  test-root is empty." else do
    putStrLn $ "\nOrganizing " ++ show (length files) ++ " file(s)..."
    let opts = OrganizeOptions { optDryRun = False, optVerbose = False, optCustomRules = rules }
    organizeByTypeWith opts testRoot h files
    putStrLn "Organization complete."

standardizeMenu :: IO ()
standardizeMenu = do
  putStrLn "\n-- Standardize Names --"
  putStrLn "  1) Dry run (preview renames)"
  putStrLn "  2) Apply standardization"
  putStrLn "  3) Rule Builder"
  putStrLn "  4) Manage Rules"
  putStrLn "  0) Back"
  askSimple "Choice: \n" >>= \case
    "1" -> runStandardize True  >> standardizeMenu
    "2" -> runStandardize False >> standardizeMenu
    "3" -> ruleBuilderMenu >> standardizeMenu
    "4" -> manageRulesMenu >> standardizeMenu
    "0" -> return ()
    _   -> standardizeMenu

ruleBuilderMenu :: IO ()
ruleBuilderMenu = do
  files <- withRunLog testRoot $ \h -> listFilesRecursive h testRoot
  let withExt = filter (\f -> takeExtension f /= "") files
  if null withExt
    then putStrLn "  No files with extensions found in test-root."
    else do
      putStrLn "\n-- Rule Builder: pick a file to model --"
      numbered withExt
      putStrLn "  0) Cancel"
      choice <- askSimple "Choice: \n"
      case readMaybe choice :: Maybe Int of
        Just 0 -> return ()
        Just n | n >= 1 && n <= length withExt -> buildRuleFor (withExt !! (n - 1))
        _ -> putStrLn "  Invalid choice."

buildRuleFor :: FilePath -> IO ()
buildRuleFor fp = do
  let file   = takeFileName fp
      base   = dropExtension file
      tokens = tokenize base
      shape  = shapeOf tokens
      delim  = dominantDelim base
  putStrLn $ "\n  File:      " ++ file
  putStrLn $ "  Tokens:    " ++ intercalate "  " [ show i ++ ":" ++ showTok t | (i, t) <- zip [1 :: Int ..] tokens ]
  putStrLn $ "  Shape:     " ++ unwords shape
  putStrLn $ "  Delimiter: " ++ maybe "(none detected)" (:[]) delim
  cfg <- loadNameMapConfig
  rName <- askSimple "\nRule name (unique identifier, empty to cancel): \n"
  if null rName
    then putStrLn "  Cancelled."
    else do
      case find (\r -> ruleName r == rName) (shapeRules cfg) of
        Just _  -> putStrLn $ "  Note: existing rule '" ++ rName ++ "' will be overwritten."
        Nothing -> putStrLn $ "  Creating new rule '" ++ rName ++ "'."
      putStrLn "\n-- Select or create a naming standard --"
      let stds = standards cfg
      if null stds
        then putStrLn "  (no standards saved yet)"
        else numbered [ stdId s ++ "  ->  " ++ stdPattern s | s <- stds ]
      putStrLn "  0) Create new standard"
      stdChoice <- askSimple "Choice: \n"
      mStd <- case readMaybe stdChoice :: Maybe Int of
        Just 0 -> createStandardMenu cfg
        Just m | m >= 1 && m <= length stds -> return (Just (stds !! (m - 1)))
        _ -> putStrLn "  Invalid, cancelled." >> return Nothing
      case mStd of
        Nothing  -> return ()
        Just std -> do
          let patVars = extractPatternVars (stdPattern std)
          putStrLn $ "\n  Standard:  " ++ stdId std
          putStrLn $ "  Pattern:   " ++ stdPattern std
          putStrLn $ "  Variables: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") patVars)
          putStrLn "\n-- Map token positions to pattern variables --"
          putStrLn "  Enter a variable name, 'x' to skip, '!' to abort."
          tmap <- mapTokens 1 tokens []
          when (not (null tmap)) $ do
            let mappedVars = map snd tmap
                dups = nub [ v | v <- mappedVars, length (filter (== v) mappedVars) > 1 ]
            when (not (null dups)) $
              putStrLn $ "  Warning: variable(s) mapped to multiple tokens: " ++ intercalate ", " dups
            let unmapped = filter (`notElem` mappedVars) patVars
            when (not (null unmapped)) $
              putStrLn $ "  Warning: pattern variable(s) with no token assigned: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") unmapped)
            putStrLn "\n-- Translation dictionaries --"
            putStrLn "  For each mapped token, optionally provide a translation."
            putStrLn "  (Enter=keep, !=abort, *=global map, @=anchor)"
            (finalDict, aborted) <- buildDicts tmap tokens M.empty
            when (not aborted) $ do
              cfg' <- loadNameMapConfig
              let newRule = ShapeRule rName shape delim tmap (stdId std) finalDict
                  others  = filter (\r -> ruleName r /= rName) (shapeRules cfg')
              saveNameMapConfig (cfg' { shapeRules = others ++ [newRule] })
              putStrLn $ "\n  Rule '" ++ rName ++ "' saved."

createStandardMenu :: NameMapConfig -> IO (Maybe NameStandard)
createStandardMenu cfg = do
  putStrLn "\n-- Create New Standard --"
  sid <- askSimple "Standard ID (e.g. univ-assignment): \n"
  if null sid
    then return Nothing
    else do
      pat <- askSimple "Pattern   (e.g. {class}-{number}_{type} {counter}): "
      if null pat
        then return Nothing
        else do
          let vars = extractPatternVars pat
          if null vars
            then do
              putStrLn "  Warning: no {variables} found in pattern."
              ok <- yesNoSimple "  Save anyway?"
              if ok then persist sid pat else return Nothing
            else do
              putStrLn $ "  Variables detected: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") vars)
              ok <- yesNoSimple "  Save this standard?"
              if ok then persist sid pat else return Nothing
  where
    persist sid pat = do
      let std = NameStandard sid pat
      saveNameMapConfig (cfg { standards = standards cfg ++ [std] })
      putStrLn $ "  Standard '" ++ sid ++ "' saved."
      return (Just std)

manageRulesMenu :: IO ()
manageRulesMenu = do
  cfg <- loadNameMapConfig
  let rules = shapeRules cfg
  putStrLn "\n-- Manage Rules --"
  if null rules
    then putStrLn "  (no rules saved yet)"
    else forM_ (zip [1 :: Int ..] rules) $ \(i, r) -> do
      putStrLn $ "\n  " ++ show i ++ ") " ++ ruleName r
      putStrLn $ "     Shape:    " ++ unwords (shapeTokens r)
      putStrLn $ "     Delim:    " ++ maybe "(any)" (:[]) (ruleDelim r)
      putStrLn $ "     Standard: " ++ targetStd r
      putStrLn $ "     Tokens:   " ++ intercalate ", " [ show pos ++ "->{" ++ v ++ "}" | (pos, v) <- tokenMap r ]
  putStrLn "\n  d <n>) Delete a rule"
  putStrLn "  0) Back"
  raw <- askSimple "Action: \n"
  let (cmd, restStr) = break isSpace raw
      idxs = parseInts restStr
  case cmd of
    "d" | not (null idxs) -> do
      let idx = head idxs
      if idx >= 1 && idx <= length rules
        then do
          let r = rules !! (idx - 1)
          ok <- yesNoSimple $ "Delete rule '" ++ ruleName r ++ "'?"
          when ok $ do
            saveNameMapConfig (cfg { shapeRules = filter (\x -> ruleName x /= ruleName r) rules })
            putStrLn "  Deleted."
          manageRulesMenu
        else putStrLn "  Index out of range." >> manageRulesMenu
    "0" -> return ()
    _   -> manageRulesMenu

mapTokens :: Int -> [Token] -> [(Int, String)] -> IO [(Int, String)]
mapTokens idx tokens acc
  | idx > length tokens = return acc
  | otherwise = do
      let tok = tokens !! (idx - 1)
      res <- askSimple $ "  Token " ++ show idx ++ " " ++ showTok tok ++ " -> variable (x=skip, !=abort): "
      case res of
        "!" -> return []
        "x" -> mapTokens (idx + 1) tokens acc
        ""  -> mapTokens idx tokens acc
        var -> mapTokens (idx + 1) tokens (acc ++ [(idx, var)])

buildDicts :: [(Int, String)] -> [Token] -> M.Map String (M.Map String String)
           -> IO (M.Map String (M.Map String String), Bool)
buildDicts [] _ acc = return (acc, False)
buildDicts ((idx, var):ts) tokens acc = do
  if map toLower var == "counter"
    then buildDicts ts tokens acc
    else do
      let tok = tokens !! (idx - 1)
          raw = case tok of Alpha n _ -> n; Num num -> show num
      res <- askSimple $ "  Translate '" ++ raw ++ "' for {" ++ var ++ "}? (Enter=keep, !=abort, *=global, @=anchor): "
      case res of
        "!" -> return (acc, True)
        ""  -> buildDicts ts tokens acc
        trans -> do
          let innerMap = M.findWithDefault M.empty var acc
              newInner = M.insert raw trans innerMap
          buildDicts ts tokens (M.insert var newInner acc)

runStandardize :: Bool -> IO ()
runStandardize isDryRun = withRunLog testRoot $ \h -> do
  files <- listFilesRecursive h testRoot
  cfg   <- loadNameMapConfig
  let label = if isDryRun then "Dry-run preview" else "Standardizing"
  putStrLn $ "\n" ++ label ++ ": " ++ show (length files) ++ " file(s) in scope."
  _ <- runStandardizeBatch h isDryRun cfg files
  putStrLn "Done."

runDryScan :: IO ()
runDryScan = withRunLog testRoot $ \h -> do
  files <- listFilesRecursive h testRoot
  if null files
    then putStrLn "  test-root is empty."
    else do
      putStrLn $ "\nScanning " ++ show (length files) ++ " file(s):\n"
      mapM_ showFileInfo files
      putStrLn ""

showFileInfo :: FilePath -> IO ()
showFileInfo f = do
  et <- try (detectType f) :: IO (Either SomeException String)
  eh <- try (sha256File f) :: IO (Either SomeException String)
  case (et, eh) of
    (Right t, Right h) -> putStrLn $ "  " ++ f ++ " | " ++ t ++ " | " ++ take 16 h ++ "..."
    (Left e, _)        -> putStrLn $ "  " ++ f ++ " | detect err: " ++ show e
    (_, Left e)        -> putStrLn $ "  " ++ f ++ " | hash err: " ++ show e

runDedupeMenu :: IO ()
runDedupeMenu = do
  removeOrig <- yesNoSimple "\nDelete originals after moving?"
  ok <- yesNoSimple "Proceed?"
  when ok $ do
    withRunLog testRoot $ \h -> dedupe testRoot removeOrig h
    putStrLn (colorInfo "Dedupe complete.")

manageSortRulesMenu :: IO ()
manageSortRulesMenu = do
  putStrLn "\n-- Manage Sort Rules --"
  putStrLn "  1) List presets"
  putStrLn "  2) Create preset"
  putStrLn "  3) Delete preset"
  putStrLn "  0) Back"
  askSimple "Choice: \n" >>= \case
    "1" -> listRulePresetsMenu >> manageSortRulesMenu
    "2" -> createRulePresetMenu >> manageSortRulesMenu
    "3" -> deleteRulePresetMenu >> manageSortRulesMenu
    "0" -> return ()
    _   -> putStrLn (colorWarn "Invalid.") >> manageSortRulesMenu

listRulePresetsMenu :: IO ()
listRulePresetsMenu = do
  presets <- listRulePresets
  if null presets
    then putStrLn "  No rule presets saved."
    else forM_ presets $ \path -> do
      res <- loadRulePreset path
      case res of
        Left _  -> putStrLn $ "  " ++ takeFileName path ++ " (error loading)"
        Right p -> do
          putStrLn $ "\n  " ++ rulePresetName p ++ ":"
          forM_ (rulePresetRules p) $ \r ->
            putStrLn $ "    \"" ++ ruleKeyword r ++ "\" -> " ++ ruleFolder r ++ "/"

createRulePresetMenu :: IO ()
createRulePresetMenu = do
  name <- askSimple "\nPreset name: \n"
  if null name then return () else do
    rules <- collectRules []
    if null rules
      then putStrLn "  No rules added, discarded."
      else do
        let preset = RulePreset name rules
            path   = rulePresetsDir </> map (\c -> if isSpace c then '-' else c) name ++ ".json"
        saveRulePreset preset path
        putStrLn $ "Saved " ++ show (length rules) ++ " rule(s) to " ++ path

collectRules :: [CustomRule] -> IO [CustomRule]
collectRules acc = do
  putStrLn $ "  (" ++ show (length acc) ++ " rule(s) so far)"
  kw <- askSimple "Keyword to match (empty to finish): \n"
  if null kw then return acc else do
    folder <- askSimple "Sort into folder name: \n"
    if null folder then return acc else
      collectRules (acc ++ [CustomRule kw folder])

deleteRulePresetMenu :: IO ()
deleteRulePresetMenu = do
  presets <- listRulePresets
  if null presets then putStrLn "  No rule presets." else do
    numbered (map takeFileName presets)
    choice <- askSimple "Delete which number (0 to cancel)? \n"
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        let path = presets !! (n-1)
        ok <- yesNoSimple $ "Delete " ++ takeFileName path ++ "?"
        when ok $ deleteRulePreset path >> putStrLn "  Deleted."
      _ -> return ()

printOutcome :: TestOutcome -> IO ()
printOutcome o =
  case outResult o of
    Pass ->
      putStrLn $
        colorPass ("PASS ") ++ outTestName o ++ " [" ++ outScenario o ++ "]"

    Fail msg ->
      putStrLn $
        colorFail ("FAIL ") ++ outTestName o ++ " [" ++ outScenario o ++ "] - " ++ msg

printSummary :: [TestOutcome] -> IO ()
printSummary outcomes = do
  let p = passCount outcomes
      f = failCount outcomes
      t = length outcomes
  putStrLn ""
  if f == 0
    then putStrLn $ colorPass ("All tests passed (" ++ show p ++ "/" ++ show t ++ ")")
    else do
      putStrLn $ colorPass ("Passed: " ++ show p ++ "/" ++ show t)
      putStrLn $ colorFail ("Failed: " ++ show f)


runTestsMenu :: IO ()
runTestsMenu = do
  putStrLn $ colorHeader "\n-- Run Tests --"
  putStrLn "  1) Run all tests"
  putStrLn "  2) Run by scenario"
  putStrLn "  0) Back"
  choice <- askSimple "Choice:"
  case choice of
    "1" -> do
      putStrLn $ colorInfo "\nRunning all tests..."
      outcomes <- runAllTests allTests
      mapM_ printOutcome outcomes
      printSummary outcomes
      runTestsMenu

    "2" -> do
      outcomes <- runByScenarioMenu
      mapM_ printOutcome outcomes
      printSummary outcomes
      runTestsMenu

    "0" -> return ()
    _   -> putStrLn (colorWarn "Invalid choice.") >> runTestsMenu


runByScenarioMenu :: IO [TestOutcome]
runByScenarioMenu = do
  putStrLn $ colorHeader "\nAvailable scenarios:"
  names <- scenarioNames
  mapM_ putStrLn names
  s <- askSimple "Scenario name:"
  runByScenario s allTests
