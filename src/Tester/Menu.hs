{-# LANGUAGE LambdaCase #-}
module Tester.Menu (runTester) where

import System.Console.Haskeline
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, find, nub, partition)
import System.FilePath ((</>), takeFileName, takeDirectory, dropExtension, takeExtension)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IO (Handle)
import qualified Data.Map.Strict as M

import Tester.Types
import Tester.Templates
import Tester.Presets
import Tester.NameMap
import Tester.Scramble (autoVariants)
import Tester.Build
import Tester.TestRegistry  (allTests)
import Tester.TestRunner
import Tester.TestScenarios (allScenarios, scenarioName, scenarioDesc)
import Tester.TestTypes
import Tester.StandardizeRunner (runStandardizeBatch)

import Core.Standardize
import Core.Logger     (withRunLog)
import Core.Scanner    (listFilesRecursive)
import Core.Detect     (detectType)
import Core.Hash       (sha256File)
import Core.Dedupe     (dedupe)
import Core.Organizer  (OrganizeOptions(..), organizeByType, organizeByTypeWith)
import Core.RulePresets
  ( CustomRule(..), RulePreset(..)
  , rulePresetsDir
  , listRulePresets, loadRulePreset, saveRulePreset, deleteRulePreset
  )

runTester :: IO ()
runTester = runInputT defaultSettings mainMenu

ask :: String -> InputT IO String
ask p = do
  mi <- getInputLine p
  return $ maybe "" (dropWhile isSpace . reverse . dropWhile isSpace . reverse) mi

yesNo :: String -> InputT IO Bool
yesNo p = do
  r <- ask (p ++ " [y/n]: ")
  return $ map toLower r `elem` ["y", "yes"]

numbered :: [String] -> InputT IO ()
numbered xs = forM_ (zip [1 :: Int ..] xs) $ \(i, x) -> outputStrLn $ "  " ++ show i ++ ") " ++ x

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

withTestRoot :: InputT IO () -> InputT IO ()
withTestRoot action = do
  exists <- liftIO $ doesDirectoryExist testRoot
  if exists then action else outputStrLn "  Error: test-root does not exist. Please build a preset first."

mainMenu :: InputT IO ()
mainMenu = do
  outputStrLn "\n===============================\n    File Organizer Tester\n==============================="
  outputStrLn "  1) Load and build a preset\n  2) Manage Presets\n  3) Manage File Options"
  outputStrLn "  4) Vary test-root files\n  5) Clear test-root\n  6) Full reset"
  outputStrLn "  7) Run organizer\n  8) Run tests\n  9) Manage Sort Rules\n  0) Quit\n==============================="
  choice <- ask "Choice: "
  case choice of
    "1" -> loadPresetMenu >> mainMenu
    "2" -> managePresetsMenu >> mainMenu
    "3" -> manageOptionsMenu >> mainMenu
    "4" -> withTestRoot (liftIO loadOptions >>= liftIO . varyTestRoot) >> mainMenu
    "5" -> yesNo "Clear test-root?" >>= \ok -> when ok (liftIO clearTestRoot) >> mainMenu
    "6" -> yesNo "Remove test-root, manifest, options, and user presets?" >>= \ok -> when ok (liftIO fullReset) >> mainMenu
    "7" -> runOrganizerMenu >> mainMenu
    "8" -> runTestsMenu >> mainMenu
    "9" -> manageSortRulesMenu >> mainMenu
    "0" -> outputStrLn "Goodbye!"
    _   -> outputStrLn "Invalid choice." >> mainMenu

loadPresetMenu :: InputT IO ()
loadPresetMenu = do
  presets <- liftIO listPresets
  if null presets then outputStrLn "No presets saved yet." else do
    outputStrLn "\n-- Load Preset --"
    numbered $ map (\p -> (if takeFileName (takeDirectory p) == "scenarios" then "[Static] " else "[User]   ") ++ takeFileName p) presets
    outputStrLn "  0) Back"
    choice <- ask "Choice: "
    case readMaybe choice :: Maybe Int of
      Just 0 -> return ()
      Just n | n >= 1 && n <= length presets -> do
        res <- liftIO (loadPreset (presets !! (n-1)))
        case res of
          Left err -> outputStrLn $ "Error: " ++ err
          Right p  -> do
            opts <- liftIO loadOptions
            liftIO (buildPreset opts p)
            outputStrLn "Done!"
            vary <- yesNo "Apply vary now?"
            when vary $ liftIO (varyTestRoot opts)
      _ -> outputStrLn "Invalid choice."

managePresetsMenu :: InputT IO ()
managePresetsMenu = do
  outputStrLn "\n-- Manage Presets --\n  1) Create new\n  2) Edit existing\n  3) Delete\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> createPresetMenu >> managePresetsMenu
    "2" -> editPresetMenu >> managePresetsMenu
    "3" -> deletePresetMenu >> managePresetsMenu
    "0" -> return ()
    _   -> outputStrLn "Invalid." >> managePresetsMenu

createPresetMenu :: InputT IO ()
createPresetMenu = do
  name <- ask "\nPreset name: "
  if null name then return () else do
    opts <- liftIO loadOptions
    if null opts then outputStrLn "No file options yet." else do
      folders <- collectFolders opts []
      if null folders then outputStrLn "Discarded." else do
        let preset = Preset name folders
        save <- yesNo "Save?"
        when save $ do
          isStatic <- yesNo "Save as static?"
          let path = (if isStatic then presetsDir </> "scenarios" else presetsDir) </> map (\c -> if isSpace c then '-' else c) name ++ ".json"
          liftIO $ createDirectoryIfMissing True (takeDirectory path) >> savePreset preset path
          outputStrLn $ "Saved to " ++ path
        build <- yesNo "Build test-root now?"
        when build $ liftIO (buildPreset opts preset) >> yesNo "Apply vary?" >>= \v -> when v (liftIO $ varyTestRoot opts)

editPresetMenu :: InputT IO ()
editPresetMenu = do
  presets <- liftIO listPresets
  if null presets then outputStrLn "No presets." else do
    outputStrLn "\n-- Edit Preset --"
    numbered $ map (\p -> (if takeFileName (takeDirectory p) == "scenarios" then "[Static] " else "[User]   ") ++ takeFileName p) presets
    choice <- ask "Choice (0 to Cancel): "
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        let path = presets !! (n-1)
        res <- liftIO $ loadPreset path
        case res of
          Left e -> outputStrLn $ "Error: " ++ e
          Right p -> do
            newNameRaw <- ask $ "New preset name (empty to keep '" ++ presetName p ++ "'): "
            let newName = if null newNameRaw then presetName p else newNameRaw
            opts <- liftIO loadOptions
            finalFolders <- modifyFolders opts (presetStructure p)
            isStatic <- yesNo "Save as static?"
            let newPath = (if isStatic then presetsDir </> "scenarios" else presetsDir) </> map (\c -> if isSpace c then '-' else c) newName ++ ".json"
            liftIO $ do
              when (path /= newPath) $ deletePreset path
              createDirectoryIfMissing True (takeDirectory newPath)
              savePreset (p { presetName = newName, presetStructure = finalFolders }) newPath
            outputStrLn $ "Saved to " ++ newPath
      _ -> return ()

deletePresetMenu :: InputT IO ()
deletePresetMenu = do
  presets <- liftIO listPresets
  if null presets then outputStrLn "No presets." else do
    numbered $ map takeFileName presets
    choice <- ask "Delete which number (0 to Cancel)? "
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        let path = presets !! (n-1)
        ok <- yesNo $ "Delete " ++ takeFileName path ++ "?"
        when ok $ liftIO (deletePreset path) >> outputStrLn "Deleted."
      _ -> return ()

modifyFolders :: [FileOption] -> [FolderEntry] -> InputT IO [FolderEntry]
modifyFolders opts folders = do
  outputStrLn "\n-- Current Folders --"
  if null folders then outputStrLn "  (No folders)" else numbered (map (\f -> folderPath f ++ " (" ++ show (sum $ map entryCount $ folderFiles f) ++ " files)") folders)
  outputStrLn "\n  a) Add folder\n  d) Delete (e.g. d 1)\n  e) Edit (e.g. e 1)\n  b) Done"
  raw <- ask "Action: "
  let (cmd, restStr) = break isSpace raw
      rest = parseInts restStr
  case cmd of
    "a" -> ask "Folder path: " >>= \path -> if null path then modifyFolders opts folders else collectFiles opts [] >>= \fs -> modifyFolders opts (folders ++ [FolderEntry path fs])
    "d" | not (null rest) -> do
        let idx = head rest
        if idx >= 1 && idx <= length folders
            then modifyFolders opts (take (idx - 1) folders ++ drop idx folders)
            else modifyFolders opts folders
    "e" | not (null rest) -> do
        let idx = head rest
        if idx >= 1 && idx <= length folders
            then modifyFiles opts (folderPath (folders !! (idx-1))) (folderFiles (folders !! (idx-1))) >>= \fs -> modifyFolders opts (take (idx - 1) folders ++ [(folders !! (idx-1)) { folderFiles = fs }] ++ drop idx folders)
            else modifyFolders opts folders
    "b" -> return folders
    _   -> modifyFolders opts folders

modifyFiles :: [FileOption] -> String -> [FileEntry] -> InputT IO [FileEntry]
modifyFiles opts path files = do
  outputStrLn $ "\n-- Editing Files in '" ++ path ++ "' --"
  if null files then outputStrLn "  (No files)" else numbered (map (\f -> show (entryCount f) ++ "x " ++ entryOption f) files)
  outputStrLn "\n  a) Add files\n  d) Delete (e.g. d 1)\n  b) Done"
  raw <- ask "Action: "
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

collectFolders :: [FileOption] -> [FolderEntry] -> InputT IO [FolderEntry]
collectFolders opts acc = ask "Folder path (empty to finish): " >>= \path -> if null path then return acc else collectFiles opts [] >>= \fs -> collectFolders opts (acc ++ [FolderEntry path fs])

collectFiles :: [FileOption] -> [FileEntry] -> InputT IO [FileEntry]
collectFiles opts acc = do
  numbered (map (\o -> optionName o ++ optionExt o) opts)
  choice <- ask "Select option (0 to Done): "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return acc
    Just n | n >= 1 && n <= length opts -> do
      cstr <- ask $ "How many " ++ optionName (opts !! (n-1)) ++ " files? "
      case readMaybe cstr :: Maybe Int of
        Just c | c >= 1 -> collectFiles opts (acc ++ [FileEntry (optionName (opts !! (n-1))) c])
        _ -> outputStrLn "Need >= 1." >> collectFiles opts acc
    _ -> collectFiles opts acc

manageOptionsMenu :: InputT IO ()
manageOptionsMenu = do
  uOpts <- liftIO loadUserOptions
  sOpts <- liftIO loadStaticOptions
  let refs = map (\o -> (o, True)) sOpts ++ map (\o -> (o, False)) uOpts
  outputStrLn "\n-- File Options --"
  if null refs then outputStrLn "  (none yet)" else numbered (map (\(o, isS) -> (if isS then "[Static] " else "[User]   ") ++ optionName o ++ optionExt o) refs)
  outputStrLn "\n  1) Add new\n  2) Edit/Manage variants\n  3) Delete\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> addOptionMenu uOpts sOpts >> manageOptionsMenu
    "2" -> editOptionMenu refs uOpts sOpts >> manageOptionsMenu
    "3" -> deleteOptionMenu refs uOpts sOpts >> manageOptionsMenu
    "0" -> return ()
    _   -> outputStrLn "Invalid." >> manageOptionsMenu

addOptionMenu :: [FileOption] -> [FileOption] -> InputT IO ()
addOptionMenu uOpts sOpts = do
  name <- ask "\nBase name: "
  if null name then return () else do
    ext <- normalizeExt <$> ask "Extension (e.g. .txt): "
    let opt = FileOption name ext (autoVariants name)
    isStatic <- yesNo "Save as static?"
    liftIO $ if isStatic then saveStaticOptions (upsertOption opt sOpts) else saveUserOptions (upsertOption opt uOpts)
    outputStrLn "Option created."

editOptionMenu :: [(FileOption, Bool)] -> [FileOption] -> [FileOption] -> InputT IO ()
editOptionMenu [] _ _ = return ()
editOptionMenu refs uOpts sOpts = do
  choice <- ask "Edit which number? "
  case readMaybe choice :: Maybe Int of
    Just n | n >= 1 && n <= length refs -> do
      let (opt, isStatic) = refs !! (n-1)
      updatedOpt <- manageVariantsMenu opt
      toggle <- yesNo $ "Toggle status? (Currently " ++ (if isStatic then "[Static]" else "[User]") ++ ")"
      let finalStatic = if toggle then not isStatic else isStatic
          cleanU = removeOption (optionName opt) uOpts
          cleanS = removeOption (optionName opt) sOpts
      liftIO $ if finalStatic then saveUserOptions cleanU >> saveStaticOptions (upsertOption updatedOpt cleanS) else saveStaticOptions cleanS >> saveUserOptions (upsertOption updatedOpt cleanU)
    _ -> return ()

deleteOptionMenu :: [(FileOption, Bool)] -> [FileOption] -> [FileOption] -> InputT IO ()
deleteOptionMenu [] _ _ = return ()
deleteOptionMenu refs uOpts sOpts = do
  choice <- ask "Delete which number? "
  case readMaybe choice :: Maybe Int of
    Just n | n >= 1 && n <= length refs -> do
      let (opt, isStatic) = refs !! (n-1)
      ok <- yesNo $ "Delete '" ++ optionName opt ++ "'?"
      when ok $ liftIO $ if isStatic then saveStaticOptions (removeOption (optionName opt) sOpts) else saveUserOptions (removeOption (optionName opt) uOpts)
    _ -> return ()

manageVariantsMenu :: FileOption -> InputT IO FileOption
manageVariantsMenu opt = do
  let vs = variants opt
  outputStrLn $ "\n-- Variants for " ++ optionName opt ++ optionExt opt ++ " --"
  forM_ (zip [1 :: Int ..] vs) $ \(i, v) -> outputStrLn $ "  " ++ show i ++ ") " ++ (if variantEnabled v then "[ON] " else "[OFF]") ++ " " ++ variantLabel v
  outputStrLn "\n  a) Add custom\n  r) Remove (e.g. r 1)\n  e) Enable (e.g. e 1)\n  d) Disable (e.g. d 1)\n  b) Done"
  raw <- ask "Action: "
  let (cmd, restStr) = break isSpace raw
      rest = parseInts restStr
  case cmd of
    "a" -> ask "Custom base name: " >>= \b -> if null b then manageVariantsMenu opt else manageVariantsMenu (opt { variants = vs ++ [Variant ("custom: " ++ b) b (b ++ "_{N}") True] })
    "r" | not (null rest) -> manageVariantsMenu (opt { variants = filter (\v -> variantLabel v `notElem` map variantLabel (selectByIndices rest vs)) vs })
    "e" | not (null rest) -> manageVariantsMenu (opt { variants = [ if i `elem` rest then v { variantEnabled = True } else v | (i, v) <- zip [1..] vs ] })
    "d" | not (null rest) -> manageVariantsMenu (opt { variants = [ if i `elem` rest then v { variantEnabled = False } else v | (i, v) <- zip [1..] vs ] })
    "b" -> return opt
    _   -> manageVariantsMenu opt

runOrganizerMenu :: InputT IO ()
runOrganizerMenu = do
  outputStrLn "\n-- Run Organizer --\n  1) Dry-run scan\n  2) Dedupe\n  3) Full Organize\n  4) Standardize Names\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> withTestRoot (liftIO runDryScan) >> runOrganizerMenu
    "2" -> withTestRoot runDedupeMenu >> runOrganizerMenu
    "3" -> withTestRoot pickRulesAndOrganize >> runOrganizerMenu
    "4" -> withTestRoot standardizeMenu >> runOrganizerMenu
    "0" -> return ()
    _   -> runOrganizerMenu

pickRulesAndOrganize :: InputT IO ()
pickRulesAndOrganize = do
  rules <- pickRulePreset
  liftIO (runFullOrganize rules)

pickRulePreset :: InputT IO [CustomRule]
pickRulePreset = do
  presets <- liftIO listRulePresets
  if null presets then return [] else do
    useRules <- yesNo "Apply custom sort rules?"
    if not useRules then return [] else do
      outputStrLn "\n-- Select Rule Preset --"
      numbered (map takeFileName presets)
      outputStrLn "  0) None (type-based only)"
      choice <- ask "Choice: "
      case readMaybe choice :: Maybe Int of
        Just 0 -> return []
        Just n | n >= 1 && n <= length presets -> do
          res <- liftIO (loadRulePreset (presets !! (n-1)))
          case res of
            Left err -> outputStrLn ("Error: " ++ err) >> return []
            Right p  -> do
              outputStrLn $ "Using " ++ show (length (rulePresetRules p)) ++ " rule(s)."
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

standardizeMenu :: InputT IO ()
standardizeMenu = do
  outputStrLn "\n-- Standardize Names --\n  1) Dry run (preview renames)\n  2) Apply standardization\n  3) Rule Builder\n  4) Manage Rules\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> liftIO (runStandardize True)  >> standardizeMenu
    "2" -> liftIO (runStandardize False) >> standardizeMenu
    "3" -> ruleBuilderMenu >> standardizeMenu
    "4" -> manageRulesMenu >> standardizeMenu
    "0" -> return ()
    _   -> standardizeMenu

ruleBuilderMenu :: InputT IO ()
ruleBuilderMenu = do
  files <- liftIO $ withRunLog testRoot $ \h -> listFilesRecursive h testRoot
  let withExt = filter (\f -> takeExtension f /= "") files
  if null withExt
    then outputStrLn "  No files with extensions found in test-root."
    else do
      outputStrLn "\n-- Rule Builder: pick a file to model --"
      numbered withExt
      outputStrLn "  0) Cancel"
      choice <- ask "Choice: "
      case readMaybe choice :: Maybe Int of
        Just 0 -> return ()
        Just n | n >= 1 && n <= length withExt -> buildRuleFor (withExt !! (n - 1))
        _ -> outputStrLn "  Invalid choice."

buildRuleFor :: FilePath -> InputT IO ()
buildRuleFor fp = do
  let file   = takeFileName fp
      base   = dropExtension file
      tokens = tokenize base
      shape  = shapeOf tokens
      delim  = dominantDelim base

  outputStrLn $ "\n  File:      " ++ file
  outputStrLn $ "  Tokens:    " ++ intercalate "  " [ show i ++ ":" ++ showTok t | (i, t) <- zip [1 :: Int ..] tokens ]
  outputStrLn $ "  Shape:     " ++ unwords shape
  outputStrLn $ "  Delimiter: " ++ maybe "(none detected)" (:[]) delim

  cfg <- liftIO loadNameMapConfig
  rName <- ask "\nRule name (unique identifier, empty to cancel): "
  if null rName then outputStrLn "  Cancelled." else do
    case find (\r -> ruleName r == rName) (shapeRules cfg) of
      Just _  -> outputStrLn $ "  Note: existing rule '" ++ rName ++ "' will be overwritten."
      Nothing -> outputStrLn $ "  Creating new rule '" ++ rName ++ "'."

    outputStrLn "\n-- Select or create a naming standard --"
    let stds = standards cfg
    if null stds then outputStrLn "  (no standards saved yet)" else numbered [ stdId s ++ "  ->  " ++ stdPattern s | s <- stds ]
    outputStrLn "  0) Create new standard"
    stdChoice <- ask "Choice: "
    mStd <- case readMaybe stdChoice :: Maybe Int of
      Just 0                               -> createStandardMenu cfg
      Just m | m >= 1 && m <= length stds  -> return (Just (stds !! (m - 1)))
      _                                    -> outputStrLn "  Invalid, cancelled." >> return Nothing

    case mStd of
      Nothing  -> return ()
      Just std -> do
        let patVars = extractPatternVars (stdPattern std)
        outputStrLn $ "\n  Standard:  " ++ stdId std
        outputStrLn $ "  Pattern:   " ++ stdPattern std
        outputStrLn $ "  Variables: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") patVars)

        outputStrLn "\n-- Map token positions to pattern variables --"
        outputStrLn "  Enter a variable name, 'x' to skip, '!' to abort."
        tmap <- mapTokens 1 tokens []

        when (not (null tmap)) $ do
          let mappedVars = map snd tmap
              dups = nub [ v | v <- mappedVars, length (filter (== v) mappedVars) > 1 ]
          when (not (null dups)) $
            outputStrLn $ "  Warning: variable(s) mapped to multiple tokens: " ++ intercalate ", " dups

          let unmapped = filter (`notElem` mappedVars) patVars
          when (not (null unmapped)) $
            outputStrLn $ "  Warning: pattern variable(s) with no token assigned: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") unmapped)

          outputStrLn "\n-- Translation dictionaries --"
          outputStrLn "  For each mapped token, optionally provide a translation."
          outputStrLn "  (Enter=keep, !=abort, *=global map, @=strict anchor e.g. @357)"
          (finalDict, aborted) <- buildDicts tmap tokens M.empty

          when (not aborted) $ do
            cfg' <- liftIO loadNameMapConfig
            let newRule = ShapeRule rName shape delim tmap (stdId std) finalDict
                others  = filter (\r -> ruleName r /= rName) (shapeRules cfg')
            liftIO $ saveNameMapConfig (cfg' { shapeRules = others ++ [newRule] })
            outputStrLn $ "\n  Rule '" ++ rName ++ "' saved."

createStandardMenu :: NameMapConfig -> InputT IO (Maybe NameStandard)
createStandardMenu cfg = do
  outputStrLn "\n-- Create New Standard --"
  sid <- ask "Standard ID (e.g. univ-assignment): "
  if null sid then return Nothing else do
    pat <- ask "Pattern   (e.g. {class}-{number}_{type} {counter}): "
    if null pat then return Nothing else do
      let vars = extractPatternVars pat
      if null vars
        then outputStrLn "  Warning: no {variables} found in pattern." >> yesNo "  Save anyway?" >>= \ok -> if ok then persist sid pat else return Nothing
        else outputStrLn ("  Variables detected: " ++ intercalate ", " (map (\v -> "{" ++ v ++ "}") vars)) >> yesNo "  Save this standard?" >>= \ok -> if ok then persist sid pat else return Nothing
  where
    persist sid pat = do
      let std = NameStandard sid pat
      liftIO $ saveNameMapConfig (cfg { standards = standards cfg ++ [std] })
      outputStrLn $ "  Standard '" ++ sid ++ "' saved."
      return (Just std)

-- | Manage standardization shape rules (used within Standardize Names menu).
manageRulesMenu :: InputT IO ()
manageRulesMenu = do
  cfg <- liftIO loadNameMapConfig
  let rules = shapeRules cfg
  outputStrLn "\n-- Manage Rules --"
  if null rules
    then outputStrLn "  (no rules saved yet)"
    else forM_ (zip [1 :: Int ..] rules) $ \(i, r) -> do
      outputStrLn $ "\n  " ++ show i ++ ") " ++ ruleName r
      outputStrLn $ "     Shape:    " ++ unwords (shapeTokens r)
      outputStrLn $ "     Delim:    " ++ maybe "(any)" (:[]) (ruleDelim r)
      outputStrLn $ "     Standard: " ++ targetStd r
      outputStrLn $ "     Tokens:   " ++ intercalate ", " [ show pos ++ "->{" ++ v ++ "}" | (pos, v) <- tokenMap r ]
  outputStrLn "\n  d <n>) Delete a rule\n  0) Back"
  raw <- ask "Action: "
  let (cmd, restStr) = break isSpace raw
      idxs = parseInts restStr
  case cmd of
    "d" | not (null idxs) -> do
      let idx = head idxs
      if idx >= 1 && idx <= length rules
        then do
          let r = rules !! (idx - 1)
          ok <- yesNo $ "Delete rule '" ++ ruleName r ++ "'?"
          when ok $ do
            liftIO $ saveNameMapConfig (cfg { shapeRules = filter (\x -> ruleName x /= ruleName r) rules })
            outputStrLn "  Deleted."
          manageRulesMenu
        else outputStrLn "  Index out of range." >> manageRulesMenu
    "0" -> return ()
    _   -> manageRulesMenu

mapTokens :: Int -> [Token] -> [(Int, String)] -> InputT IO [(Int, String)]
mapTokens idx tokens acc
  | idx > length tokens = return acc
  | otherwise = do
      let tok = tokens !! (idx - 1)
      res <- ask $ "  Token " ++ show idx ++ " " ++ showTok tok ++ " -> variable (x=skip, !=abort): "
      case res of
        "!" -> return []
        "x" -> mapTokens (idx + 1) tokens acc
        ""  -> mapTokens idx tokens acc
        var -> mapTokens (idx + 1) tokens (acc ++ [(idx, var)])

buildDicts :: [(Int, String)] -> [Token] -> M.Map String (M.Map String String) -> InputT IO (M.Map String (M.Map String String), Bool)
buildDicts [] _ acc = return (acc, False)
buildDicts ((idx, var):ts) tokens acc = do
  if map toLower var == "counter"
    then buildDicts ts tokens acc
    else do
      let tok = tokens !! (idx - 1)
          raw = case tok of Alpha n _ -> n; Num num -> show num
      res <- ask $ "  Translate '" ++ raw ++ "' for {" ++ var ++ "}? (Enter=keep, !=abort, *=global, @=anchor): "
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
  if null files then putStrLn "  test-root is empty." else do
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

runDedupeMenu :: InputT IO ()
runDedupeMenu = do
  removeOrig <- yesNo "\nDelete originals after moving?"
  ok <- yesNo "Proceed?"
  when ok $ liftIO (withRunLog testRoot $ \h -> dedupe testRoot removeOrig h) >> outputStrLn "Dedupe complete."

-- ─── Sort Rules ─────────────────────────────────────────────────────────────

-- | Manage custom folder-sort rules (keyword -> folder presets). Distinct from
--   manageRulesMenu which handles standardization shape rules.
manageSortRulesMenu :: InputT IO ()
manageSortRulesMenu = do
  outputStrLn "\n-- Manage Sort Rules --\n  1) List presets\n  2) Create preset\n  3) Delete preset\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> listRulePresetsMenu >> manageSortRulesMenu
    "2" -> createRulePresetMenu >> manageSortRulesMenu
    "3" -> deleteRulePresetMenu >> manageSortRulesMenu
    "0" -> return ()
    _   -> outputStrLn "Invalid." >> manageSortRulesMenu

listRulePresetsMenu :: InputT IO ()
listRulePresetsMenu = do
  presets <- liftIO listRulePresets
  if null presets
    then outputStrLn "  No rule presets saved."
    else forM_ presets $ \path -> do
      res <- liftIO (loadRulePreset path)
      case res of
        Left _  -> outputStrLn $ "  " ++ takeFileName path ++ " (error loading)"
        Right p -> do
          outputStrLn $ "\n  " ++ rulePresetName p ++ ":"
          forM_ (rulePresetRules p) $ \r ->
            outputStrLn $ "    \"" ++ ruleKeyword r ++ "\" -> " ++ ruleFolder r ++ "/"

createRulePresetMenu :: InputT IO ()
createRulePresetMenu = do
  name <- ask "\nPreset name: "
  if null name then return () else do
    rules <- collectRules []
    if null rules
      then outputStrLn "  No rules added, discarded."
      else do
        let preset = RulePreset name rules
            path   = rulePresetsDir </> map (\c -> if isSpace c then '-' else c) name ++ ".json"
        liftIO $ saveRulePreset preset path
        outputStrLn $ "Saved " ++ show (length rules) ++ " rule(s) to " ++ path

collectRules :: [CustomRule] -> InputT IO [CustomRule]
collectRules acc = do
  outputStrLn $ "  (" ++ show (length acc) ++ " rule(s) so far)"
  kw <- ask "Keyword to match (empty to finish): "
  if null kw then return acc else do
    folder <- ask "Sort into folder name: "
    if null folder then return acc else
      collectRules (acc ++ [CustomRule kw folder])

deleteRulePresetMenu :: InputT IO ()
deleteRulePresetMenu = do
  presets <- liftIO listRulePresets
  if null presets then outputStrLn "  No rule presets." else do
    numbered (map takeFileName presets)
    choice <- ask "Delete which number (0 to cancel)? "
    case readMaybe choice :: Maybe Int of
      Just n | n >= 1 && n <= length presets -> do
        ok <- yesNo $ "Delete " ++ takeFileName (presets !! (n-1)) ++ "?"
        when ok $ liftIO (deleteRulePreset (presets !! (n-1))) >> outputStrLn "Deleted."
      _ -> return ()

runTestsMenu :: InputT IO ()
runTestsMenu = do
  scenarios <- liftIO allScenarios
  outputStrLn $ "\n-- Run Tests -- (" ++ show (length allTests) ++ " tests, " ++ show (length scenarios) ++ " scenarios)"
  outputStrLn "  1) Run all\n  2) Run one\n  3) Run by scenario\n  4) List tests\n  5) List scenarios\n  0) Back"
  ask "Choice: " >>= \case
    "1" -> liftIO (runAllTests allTests) >>= printOutcomes >> runTestsMenu
    "2" -> numbered (map testName allTests) >> ask "Choice: " >>= \c -> case readMaybe c :: Maybe Int of
             Just n | n >= 1 && n <= length allTests -> liftIO (runSpec (allTests !! (n-1))) >>= printOutcomes >> runTestsMenu
             _ -> runTestsMenu
    "3" -> numbered (map scenarioName scenarios) >> ask "Choice: " >>= \c -> case readMaybe c :: Maybe Int of
             Just n | n >= 1 && n <= length scenarios -> liftIO (runByScenario (scenarioName (scenarios !! (n-1))) allTests) >>= printOutcomes >> runTestsMenu
             _ -> runTestsMenu
    "4" -> forM_ allTests (\s -> outputStrLn $ "  " ++ testName s) >> runTestsMenu
    "5" -> forM_ scenarios (\s -> outputStrLn $ "  " ++ scenarioName s ++ "  — " ++ scenarioDesc s) >> runTestsMenu
    "0" -> return ()
    _   -> runTestsMenu

printOutcomes :: [TestOutcome] -> InputT IO ()
printOutcomes [] = outputStrLn "  (no outcomes)"
printOutcomes outcomes = do
  outputStrLn (replicate 60 '-')
  forM_ outcomes $ \o -> do
    outputStrLn $ (if outResult o == Pass then "PASS" else "FAIL") ++ " [" ++ outScenario o ++ "] " ++ outTestName o
    case outResult o of
      Fail r -> outputStrLn $ "       -> " ++ r
      Pass   -> return ()
  outputStrLn (replicate 60 '-')
  let (p, f, t) = (passCount outcomes, failCount outcomes, length outcomes)
  outputStrLn $ "  Passed: " ++ show p ++ " / " ++ show t ++ (if f > 0 then " (" ++ show f ++ " failed)" else " OK")