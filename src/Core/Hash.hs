module Core.Hash (sha256File) where

import qualified Data.ByteString.Lazy as BL
import Crypto.Hash (Digest, SHA256, hashlazy)

sha256File :: FilePath -> IO String
sha256File path = do
  bs <- BL.readFile path
  let digest = hashlazy bs :: Digest SHA256
  return (show digest)
