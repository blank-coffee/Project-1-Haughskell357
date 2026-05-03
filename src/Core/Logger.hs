module Core.Logger
  ( withRunLog
  , logMove
  , logSkip
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
logMove h src dest = hPutStrLn h $ "MOVED   " ++ src ++ " -> " ++ dest

logSkip :: Handle -> FilePath -> String -> IO ()
logSkip h src reason = hPutStrLn h $ "SKIPPED " ++ src ++ ": " ++ reason
