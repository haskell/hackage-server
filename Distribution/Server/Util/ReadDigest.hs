{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Util.ReadDigest (
    readDigestSHA
  , readDigestMD5
  ) where

import Control.DeepSeq
import Data.Binary (Binary)
import Data.Bits
import Data.Digest.Pure.MD5
import Data.Word (Word8)
import qualified Data.Binary          as Binary
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Digest.Pure.SHA as SHA

-- | Read a SHA digest
--
-- When the digest is forced the whole thing is forced.
readDigestSHA :: Binary (SHA.Digest t) => String -> Either String (SHA.Digest t)
readDigestSHA = readDigest

-- | Read an MD5 digest
readDigestMD5 :: String -> Either String MD5Digest
readDigestMD5 = readDigest

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

readDigest :: Binary a => String -> Either String a
readDigest str =
   case Binary.decodeOrFail . force . BS.L.pack . translate $ str of
     Left  (_, _, err) -> Left  err
     Right (_, _, a)   -> Right a

-- | Translate a string of hex chars to the correspondin Word8 values
-- (i.e., the length of the output will be half the length of the input)
translate :: [Char] -> [Word8]
translate []       = []
translate [_]      = error "readSHA256: trailing character"
translate (a:b:xs) = toByte a b : translate xs

-- | Translate a pair of hex chars into their value
toByte :: Char -> Char -> Word8
toByte a b = shift (charValue a) 4 .|. charValue b
  where
    charValue :: Char -> Word8
    charValue '0' = 0
    charValue '1' = 1
    charValue '2' = 2
    charValue '3' = 3
    charValue '4' = 4
    charValue '5' = 5
    charValue '6' = 6
    charValue '7' = 7
    charValue '8' = 8
    charValue '9' = 9
    charValue 'A' = 10
    charValue 'B' = 11
    charValue 'C' = 12
    charValue 'D' = 13
    charValue 'E' = 14
    charValue 'F' = 15
    charValue 'a' = 10
    charValue 'b' = 11
    charValue 'c' = 12
    charValue 'd' = 13
    charValue 'e' = 14
    charValue 'f' = 15
    charValue c   = error $ "readSHA256: invalid character " ++ show c
