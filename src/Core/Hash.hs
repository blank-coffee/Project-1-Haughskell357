module Core.Hash (sha256File) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteArray as BA
import Data.Char (toLower)

sha256File :: FilePath -> IO String
sha256File path = do
  bs <- BS.readFile path
  let digest = hash bs :: Digest SHA256
      hexBs  = convertToBase Base16 (BA.convert digest :: BS.ByteString) :: BS.ByteString
      hexStr = BSC.unpack hexBs
  return (map toLower hexStr)