module Core.Logger
  ( withRunLog
  , logMove
  , logSkip
  , logDirSkip
  , logDupeKept
  , logDupeMoved
  , logStandardize
  , logStdSkip
  ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hPutStrLn, hClose, openFile)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)

logDir :: FilePath -> FilePath
logDir root = root </> ".file-organizer-logs"

withRunLog :: FilePath -> (Handle -> IO a) -> IO a
withRunLog root action = do
  createDirectoryIfMissing True (logDir root)
  t <- getZonedTime
  let stamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" t
      path  = logDir root </> stamp ++ ".log"
  h <- openFile path AppendMode
  result <- action h
  hClose h
  pure result

logMove :: Handle -> FilePath -> FilePath -> IO ()
logMove h src dest = hPutStrLn h $ "MOVED        " ++ src ++ " -> " ++ dest

logSkip :: Handle -> FilePath -> String -> IO ()
logSkip h src reason = hPutStrLn h $ "SKIPPED      " ++ src ++ ": " ++ reason

logDirSkip :: Handle -> FilePath -> String -> IO ()
logDirSkip h dir reason = hPutStrLn h $ "DIR-SKIP     " ++ dir ++ ": " ++ reason

logDupeKept :: Handle -> FilePath -> IO ()
logDupeKept h path = hPutStrLn h $ "DUPE-KEPT    " ++ path

logDupeMoved :: Handle -> FilePath -> FilePath -> IO ()
logDupeMoved h src dest = hPutStrLn h $ "DUPE-MOVED   " ++ src ++ " -> " ++ dest

logStandardize :: Handle -> FilePath -> FilePath -> IO ()
logStandardize h src dest = hPutStrLn h $ "STANDARDIZED " ++ src ++ " -> " ++ dest

logStdSkip :: Handle -> FilePath -> String -> IO ()
logStdSkip h src reason = hPutStrLn h $ "STD-SKIP     " ++ src ++ ": " ++ reason