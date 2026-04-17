module Core.Hash (sha256File) where

-- NOTE: We use *strict* ByteString (BS.readFile) deliberately.
-- Lazy ByteString (BL.readFile / hashlazy) holds the file handle open
-- until the GC finalizes the value.  On Windows this causes
-- removePathForcibly to fail with "permission denied" when the next
-- test tries to wipe test-root before the previous handle is released.
-- Strict reading closes the handle the moment the IO action completes.

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteArray as BA
import Data.Char (toLower)

sha256File :: FilePath -> IO String
sha256File path = do
  bs <- BS.readFile path                               -- strict: handle closed here
  let digest = hash bs :: Digest SHA256
      hexBs  = convertToBase Base16 (BA.convert digest :: BS.ByteString) :: BS.ByteString
      hexStr = BSC.unpack hexBs
  return (map toLower hexStr)
