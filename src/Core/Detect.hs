module Core.Detect (detectType) where

import System.FilePath (takeExtension)
import Data.Char (toLower)

detectType :: FilePath -> IO String
detectType fp = return $ extToType (map toLower (takeExtension fp))

extToType :: String -> String
extToType ".txt"  = "text/plain"
extToType ".md"   = "text/markdown"
extToType ".hs"   = "text/x-haskell"
extToType ".jpg"  = "image/jpeg"
extToType ".png"  = "image/png"
extToType ""      = "unknown"
extToType _       = "application/octet-stream"
