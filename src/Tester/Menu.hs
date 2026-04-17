module Tester.Menu (runTester) where

import System.Console.Haskeline
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import System.FilePath ((</>), takeFileName)
import Text.Read (readMaybe)

import Tester.Types
import Tester.Templates
import Tester.Presets
import Tester.Scramble (autoVariants, applyVariant, enabledVariants)
import Tester.Build
import Tester.TestRegistry  (allTests)
import Tester.TestRunner
import Tester.TestScenarios (allScenarios, scenarioName, scenarioDesc)
import Tester.TestTypes

import Core.Scanner (listFilesRecursive)
import Core.Detect  (detectType)
import Core.Hash    (sha256File)
import Core.Dedupe  (dedupe)

-- ─── Entry point ────────────────────────────────────────────────────────────

runTester :: IO ()
runTester = runInputT defaultSettings mainMenu

-- ─── Input helpers ──────────────────────────────────────────────────────────

ask :: String -> InputT IO String
ask p = do
  mi <- getInputLine p
  return $ maybe "" trim mi
  where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

yesNo :: String -> InputT IO Bool
yesNo p = do
  r <- ask (p ++ " [y/n]: ")
  return $ map toLower r `elem` ["y", "yes"]

numbered :: [String] -> InputT IO ()
numbered xs =
  forM_ (zip [1 :: Int ..] xs) $ \(i, x) ->
    outputStrLn $ "  " ++ show i ++ ") " ++ x

-- | Parse a space-separated list of ints, e.g. "1 3 4" -> [1,3,4]
parseInts :: String -> [Int]
parseInts = foldr step [] . words
  where step w acc = case readMaybe w of { Just n -> n:acc; Nothing -> acc }

-- | Pick indices (1-based) from a list, ignoring out-of-range values
selectByIndices :: [Int] -> [a] -> [a]
selectByIndices idxs xs =
  [xs !! (i-1) | i <- idxs, i >= 1, i <= length xs]

normalizeExt :: String -> String
normalizeExt ""        = ".txt"
normalizeExt s@('.':_) = s
normalizeExt s         = '.' : s

-- ─── Main menu ──────────────────────────────────────────────────────────────

mainMenu :: InputT IO ()
mainMenu = do
  outputStrLn ""
  outputStrLn "==============================="
  outputStrLn "    File Organizer Tester"
  outputStrLn "==============================="
  outputStrLn "  1) Load and build a preset"
  outputStrLn "  2) Create new preset"
  outputStrLn "  3) Manage file options"
  outputStrLn "  4) Vary test-root files"
  outputStrLn "  5) Clear test-root"
  outputStrLn "  6) Full reset  [removes everything except presets/scenarios/]"
  outputStrLn "  7) Run organizer"
  outputStrLn "  8) Run tests"
  outputStrLn "  9) Quit"
  outputStrLn "==============================="
  choice <- ask "Choice: "
  case choice of
    "1" -> loadPresetMenu      >> mainMenu
    "2" -> createPresetMenu    >> mainMenu
    "3" -> manageOptionsMenu   >> mainMenu
    "4" -> do
             opts <- liftIO loadOptions
             liftIO (varyTestRoot opts)
             mainMenu
    "5" -> do
             ok <- yesNo "Clear test-root?"
             when ok $ liftIO clearTestRoot
             mainMenu
    "6" -> do
             ok <- yesNo "Remove test-root, manifest, options, and user presets?"
             when ok $ liftIO fullReset
             mainMenu
    "7" -> runOrganizerMenu >> mainMenu
    "8" -> runTestsMenu     >> mainMenu
    "9" -> outputStrLn "Goodbye!"
    _   -> outputStrLn "Invalid choice." >> mainMenu

-- ─── Load preset ────────────────────────────────────────────────────────────

loadPresetMenu :: InputT IO ()
loadPresetMenu = do
  presets <- liftIO listPresets
  if null presets
    then outputStrLn "No presets saved yet -- create one first."
    else do
      outputStrLn "\n-- Load Preset --"
      numbered (map takeFileName presets)
      outputStrLn "  0) Back"
      choice <- ask "Choice: "
      case readMaybe choice :: Maybe Int of
        Just 0 -> return ()
        Just n | n >= 1 && n <= length presets -> do
          result <- liftIO (loadPreset (presets !! (n-1)))
          case result of
            Left err -> outputStrLn $ "Error: " ++ err
            Right preset -> do
              outputStrLn $ "Loaded: " ++ presetName preset
              opts <- liftIO loadOptions
              liftIO (buildPreset opts preset)
              outputStrLn "Done! test-root is ready."
              vary <- yesNo "Apply vary now?"
              when vary $ liftIO (varyTestRoot opts)
        _ -> outputStrLn "Invalid choice."

-- ─── Create preset ──────────────────────────────────────────────────────────

createPresetMenu :: InputT IO ()
createPresetMenu = do
  outputStrLn "\n-- Create Preset --"
  name <- ask "Preset name: "
  if null name then outputStrLn "Cancelled." else do
    opts <- liftIO loadOptions
    if null opts
      then outputStrLn "No file options yet -- add some under Manage File Options first."
      else do
        outputStrLn "Add folders (empty path to finish)."
        folders <- collectFolders opts []
        if null folders
          then outputStrLn "No folders added -- discarded."
          else do
            let preset  = Preset name folders
                total   = sum [entryCount e | f <- folders, e <- folderFiles f]
            outputStrLn $ "\nPreset '" ++ name ++ "': "
              ++ show (length folders) ++ " folder(s), "
              ++ show total ++ " file(s)."
            save <- yesNo "Save?"
            when save $ do
              fname <- ask "Filename (without .json): "
              unless (null fname) $ do
                let path = presetsDir </> fname ++ ".json"
                liftIO (savePreset preset path)
                outputStrLn $ "Saved to " ++ path
            build <- yesNo "Build test-root now?"
            when build $ do
              liftIO (buildPreset opts preset)
              vary <- yesNo "Apply vary?"
              when vary $ liftIO (varyTestRoot opts)

collectFolders :: [FileOption] -> [FolderEntry] -> InputT IO [FolderEntry]
collectFolders opts acc = do
  outputStrLn ""
  path <- ask "Folder path ('.' for root, empty to finish): "
  if null path then return acc else do
    files <- collectFiles opts []
    let folder = FolderEntry path files
        count  = sum (map entryCount files)
    outputStrLn $ "  Added folder '" ++ path
      ++ "' with " ++ show count ++ " file(s)."
    collectFolders opts (acc ++ [folder])

collectFiles :: [FileOption] -> [FileEntry] -> InputT IO [FileEntry]
collectFiles opts acc = do
  outputStrLn "\n  File options:"
  numbered (map (\o -> optionName o ++ optionExt o) opts)
  outputStrLn "  0) Done"
  choice <- ask "  Select option: "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return acc
    Just n | n >= 1 && n <= length opts -> do
      let opt = opts !! (n-1)
      cstr <- ask $ "  How many " ++ optionName opt ++ " files? "
      case readMaybe cstr :: Maybe Int of
        Just c | c >= 1 -> do
          outputStrLn $ "  Added " ++ show c ++ "x " ++ optionName opt ++ "."
          collectFiles opts (acc ++ [FileEntry (optionName opt) c])
        _ -> outputStrLn "  Need a whole number >= 1." >> collectFiles opts acc
    _ -> outputStrLn "  Invalid." >> collectFiles opts acc

-- ─── Manage file options ────────────────────────────────────────────────────

manageOptionsMenu :: InputT IO ()
manageOptionsMenu = do
  opts <- liftIO loadOptions
  outputStrLn "\n-- File Options --"
  if null opts
    then outputStrLn "  (none yet)"
    else numbered (map (\o -> optionName o ++ optionExt o) opts)
  outputStrLn ""
  outputStrLn "  a) Add new    d) Delete    m) Manage variants    b) Back"
  choice <- ask "Choice: "
  case choice of
    "a" -> addOptionMenu    opts >> manageOptionsMenu
    "d" -> deleteOptionMenu opts >> manageOptionsMenu
    "m" -> pickForManage    opts >> manageOptionsMenu
    "b" -> return ()
    _   -> outputStrLn "Invalid." >> manageOptionsMenu

-- ─── Add option ─────────────────────────────────────────────────────────────

addOptionMenu :: [FileOption] -> InputT IO ()
addOptionMenu opts = do
  outputStrLn "\n-- Add File Option --"
  name <- ask "Base name (e.g. Assignment): "
  if null name then outputStrLn "Cancelled." else do
    rawExt <- ask "Extension (e.g. .txt): "
    let ext  = normalizeExt rawExt
        vars = autoVariants name
        opt  = FileOption name ext vars
    liftIO (saveOptions (upsertOption opt opts))
    outputStrLn $ "Created '" ++ name ++ ext ++ "' with "
      ++ show (length vars) ++ " auto-generated variants."
    outputStrLn "(Use Manage Variants to add custom ones or toggle existing ones.)"

-- ─── Delete option ──────────────────────────────────────────────────────────

deleteOptionMenu :: [FileOption] -> InputT IO ()
deleteOptionMenu [] = outputStrLn "Nothing to delete."
deleteOptionMenu opts = do
  outputStrLn "\n-- Delete File Option --"
  numbered (map optionName opts)
  outputStrLn "  0) Cancel"
  choice <- ask "Delete which? "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return ()
    Just n | n >= 1 && n <= length opts -> do
      let opt = opts !! (n-1)
      ok <- yesNo $ "Delete '" ++ optionName opt ++ "'?"
      when ok $ do
        liftIO (saveOptions (removeOption (optionName opt) opts))
        outputStrLn "Deleted."
    _ -> outputStrLn "Invalid."

-- ─── Pick which option to manage variants for ────────────────────────────────

pickForManage :: [FileOption] -> InputT IO ()
pickForManage [] = outputStrLn "No file options yet."
pickForManage opts = do
  outputStrLn "\n-- Manage Variants -- choose file option --"
  numbered (map (\o -> optionName o ++ optionExt o) opts)
  outputStrLn "  0) Back"
  choice <- ask "Choice: "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return ()
    Just n | n >= 1 && n <= length opts -> do
      let opt = opts !! (n-1)
      updated <- manageVariantsMenu opt
      let newOpts = upsertOption updated opts
      liftIO (saveOptions newOpts)
    _ -> outputStrLn "Invalid."

-- ─── Manage variants for one option ─────────────────────────────────────────

manageVariantsMenu :: FileOption -> InputT IO FileOption
manageVariantsMenu opt = do
  let vs   = variants opt
      name = optionName opt ++ optionExt opt
  outputStrLn $ "\n-- Variants for " ++ name ++ " --"
  printVariants vs
  outputStrLn ""
  outputStrLn "  a) Add custom"
  outputStrLn "  r) Remove   (e.g.  r 1 3 5)"
  outputStrLn "  e) Enable   (e.g.  e 2 4)"
  outputStrLn "  d) Disable  (e.g.  d 1)"
  outputStrLn "  b) Done"
  raw <- ask "Action: "
  let (cmd, rest) = break isSpace raw
      idxs        = parseInts rest
  case cmd of
    "a" -> do
      updated <- addCustomVariant opt
      manageVariantsMenu updated
    "r" | not (null idxs) -> do
      let toRemove  = map variantLabel (selectByIndices idxs vs)
          newVs     = filter (\v -> variantLabel v `notElem` toRemove) vs
          updated   = opt { variants = newVs }
      outputStrLn $ "  Removed " ++ show (length idxs) ++ " variant(s)."
      manageVariantsMenu updated
    "e" | not (null idxs) -> do
      let updated = opt { variants = toggleAt True  idxs vs }
      outputStrLn $ "  Enabled " ++ show (length idxs) ++ " variant(s)."
      manageVariantsMenu updated
    "d" | not (null idxs) -> do
      let updated = opt { variants = toggleAt False idxs vs }
      outputStrLn $ "  Disabled " ++ show (length idxs) ++ " variant(s)."
      manageVariantsMenu updated
    "b" -> return opt
    _   -> outputStrLn "Invalid -- use  a / r 1 3 / e 2 / d 4 / b" >> manageVariantsMenu opt

-- | Print the variant list with [ON]/[OFF] tags
printVariants :: [Variant] -> InputT IO ()
printVariants [] = outputStrLn "  (no variants)"
printVariants vs =
  forM_ (zip [1 :: Int ..] vs) $ \(i, v) -> do
    let tag  = if variantEnabled v then "[ON] " else "[OFF]"
        ex1  = singlePattern v
        exN  = let p = numberedPattern v
               in concatMap (\c -> if c == '{' then "" else
                                   if c == 'N' then "1" else
                                   if c == '}' then "" else [c]) p
    outputStrLn $ "  " ++ show i ++ ") " ++ tag ++ "  "
      ++ variantLabel v
      ++ "  [e.g. " ++ ex1 ++ " / " ++ exN ++ "]"

-- | Enable or disable specific 1-based indices in a variant list
toggleAt :: Bool -> [Int] -> [Variant] -> [Variant]
toggleAt flag idxs vs =
  [ if (i `elem` idxs) then v { variantEnabled = flag } else v
  | (i, v) <- zip [1..] vs
  ]

-- ─── Add custom variant ──────────────────────────────────────────────────────

addCustomVariant :: FileOption -> InputT IO FileOption
addCustomVariant opt = do
  outputStrLn "\n  Add Custom Variant"
  outputStrLn "  The base name for numbered files  (e.g.  HW)"
  outputStrLn "  will produce:  HW_{N}.ext   and   HW.ext  (for single files)"
  base <- ask "  Custom base name: "
  if null base
    then do outputStrLn "  Cancelled."; return opt
    else do
      let v = Variant
                ("custom: " ++ base)
                base
                (base ++ "_{N}")
                True
          updated = opt { variants = variants opt ++ [v] }
      outputStrLn $ "  Added '" ++ base ++ "' (numbered: " ++ base ++ "_1, " ++ base ++ "_2, …)"
      return updated

-- ─── Run organizer ──────────────────────────────────────────────────────────

runOrganizerMenu :: InputT IO ()
runOrganizerMenu = do
  outputStrLn "\n-- Run Organizer on test-root --"
  outputStrLn "  1) Dry-run scan   (prints type + hash for every file)"
  outputStrLn "  2) Dedupe         (moves duplicates into deleteme/)"
  outputStrLn "  0) Back"
  choice <- ask "Choice: "
  case choice of
    "1" -> liftIO runDryScan  >> runOrganizerMenu
    "2" -> runDedupeMenu      >> runOrganizerMenu
    "0" -> return ()
    _   -> outputStrLn "Invalid." >> runOrganizerMenu

-- | Scan test-root and print type + truncated hash for every file.
--   Matches the output format of the main executable's dry-run mode.
runDryScan :: IO ()
runDryScan = do
  files <- listFilesRecursive testRoot
  if null files
    then putStrLn "  test-root is empty -- build a preset first."
    else do
      putStrLn $ "\nScanning " ++ show (length files) ++ " file(s) in " ++ testRoot ++ ":\n"
      mapM_ showFileInfo files
      putStrLn ""

showFileInfo :: FilePath -> IO ()
showFileInfo f = do
  et <- try (detectType f) :: IO (Either SomeException String)
  eh <- try (sha256File  f) :: IO (Either SomeException String)
  case (et, eh) of
    (Right t, Right h) -> putStrLn $ "  " ++ f ++ " | " ++ t ++ " | " ++ take 16 h ++ "..."
    (Left  e, _)       -> putStrLn $ "  " ++ f ++ " | detect error: "  ++ show e
    (_,       Left e)  -> putStrLn $ "  " ++ f ++ " | hash error: "    ++ show e

-- | Ask for confirmation and the removeOriginals flag, then run dedupe.
runDedupeMenu :: InputT IO ()
runDedupeMenu = do
  outputStrLn "\n-- Dedupe --"
  outputStrLn "  Duplicate files will be moved to test-root/deleteme/."
  removeOrig <- yesNo "Also delete originals from test-root after moving?"
  ok         <- yesNo "Proceed?"
  if ok
    then do
      liftIO $ dedupe testRoot removeOrig
      outputStrLn "Dedupe complete."
    else outputStrLn "Cancelled."

-- ─── Run tests menu ─────────────────────────────────────────────────────────

runTestsMenu :: InputT IO ()
runTestsMenu = do
  scenarios <- liftIO allScenarios
  outputStrLn "\n-- Run Tests --"
  outputStrLn $ "  " ++ show (length allTests) ++ " test(s) across "
    ++ show (length scenarios) ++ " scenario(s)"
  outputStrLn ""
  outputStrLn "  1) Run all tests"
  outputStrLn "  2) Run one test"
  outputStrLn "  3) Run by scenario"
  outputStrLn "  4) List tests"
  outputStrLn "  5) List scenarios"
  outputStrLn "  0) Back"
  choice <- ask "Choice: "
  case choice of
    "1" -> do
      outputStrLn "\nRunning all tests...\n"
      outcomes <- liftIO (runAllTests allTests)
      printOutcomes outcomes
    "2" -> runOneTestMenu
    "3" -> runByScenarioMenu
    "4" -> listTestsMenu
    "5" -> listScenariosMenu
    "0" -> return ()
    _   -> outputStrLn "Invalid." >> runTestsMenu

-- | Let the user pick a single test by number and run all its scenarios.
runOneTestMenu :: InputT IO ()
runOneTestMenu = do
  outputStrLn "\n-- Run One Test --"
  numbered (map testName allTests)
  outputStrLn "  0) Back"
  choice <- ask "Choice: "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return ()
    Just n | n >= 1 && n <= length allTests -> do
      let spec = allTests !! (n - 1)
      outputStrLn $ "\nRunning: " ++ testName spec ++ "\n"
      outcomes <- liftIO (runSpec spec)
      printOutcomes outcomes
    _ -> outputStrLn "Invalid." >> runOneTestMenu

-- | Let the user pick a scenario and run all tests declared for it.
runByScenarioMenu :: InputT IO ()
runByScenarioMenu = do
  scenarios <- liftIO allScenarios
  outputStrLn "\n-- Run by Scenario --"
  numbered (map (\s -> scenarioName s ++ "  -- " ++ scenarioDesc s) scenarios)
  outputStrLn "  0) Back"
  choice <- ask "Choice: "
  case readMaybe choice :: Maybe Int of
    Just 0 -> return ()
    Just n | n >= 1 && n <= length scenarios -> do
      let sname = scenarioName (scenarios !! (n - 1))
      outputStrLn $ "\nRunning tests for scenario: " ++ sname ++ "\n"
      outcomes <- liftIO (runByScenario sname allTests)
      printOutcomes outcomes
    _ -> outputStrLn "Invalid." >> runByScenarioMenu

-- | Print all registered tests with the scenarios each runs against.
listTestsMenu :: InputT IO ()
listTestsMenu = do
  outputStrLn "\n-- All Tests --"
  forM_ (zip [1 :: Int ..] allTests) $ \(i, spec) -> do
    outputStrLn $ "  " ++ show i ++ ") " ++ testName spec
    outputStrLn $ "       scenarios: " ++ intercalate ", " (testScenarios spec)

-- | Print all registered scenarios with their descriptions.
listScenariosMenu :: InputT IO ()
listScenariosMenu = do
  scenarios <- liftIO allScenarios
  outputStrLn "\n-- All Scenarios --"
  forM_ scenarios $ \s ->
    outputStrLn $ "  " ++ scenarioName s ++ "\n    " ++ scenarioDesc s

-- ─── Result display ─────────────────────────────────────────────────────────

-- | Print a result table followed by a pass/fail summary line.
printOutcomes :: [TestOutcome] -> InputT IO ()
printOutcomes [] = outputStrLn "  (no outcomes)"
printOutcomes outcomes = do
  outputStrLn (replicate 60 '-')
  forM_ outcomes $ \o -> do
    let tag = case outResult o of { Pass -> "PASS"; _ -> "FAIL" }
        line = tag ++ "  [" ++ outScenario o ++ "]  " ++ outTestName o
    outputStrLn line
    case outResult o of
      Fail reason -> outputStrLn $ "       → " ++ reason
      Pass        -> return ()
  outputStrLn (replicate 60 '-')
  let p = passCount outcomes
      f = failCount outcomes
      t = length outcomes
  outputStrLn $ "  Passed: " ++ show p ++ " / " ++ show t
    ++ if f > 0 then "   (" ++ show f ++ " failed)" else "   ✓ all clear"
  outputStrLn ""
