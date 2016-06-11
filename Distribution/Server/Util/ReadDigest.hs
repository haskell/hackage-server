{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Distribution.Server.Util.ReadDigest (
    ReadDigest(..)
  , decodeBase16
  , readBase16
  ) where

import Data.Binary (Binary)
import Data.Bits
import Data.Word (Word8)
import qualified Data.Binary          as Binary
import qualified Data.ByteString      as BS.Strict
import qualified Data.ByteString.Lazy as BS.Lazy

class ReadDigest a where
  -- | Read a digest. The default instance uses 'decodeBase16'.
  readDigest :: String -> Either String a
  default readDigest :: Binary a => String -> Either String a
  readDigest = decodeBase16

-- | Read encoding in base-16 format and then use 'Binary' instance to decode
decodeBase16 :: Binary a => String -> Either String a
decodeBase16 str =
    case Binary.decodeOrFail . BS.Lazy.fromStrict . readBase16 $ str of
      Left  (_, _, err) -> Left  err
      Right (_, _, a)   -> Right a

-- | Read base-16 encoded string
--
-- For instance, the string "deadbeef" will be turned into a 4-byte bytestring
-- @[0xDE, 0xAD, 0xBE, 0xEF]@
--
-- TODO: This uses 'error' if the characters out of range. Might be nicer to
-- use @Either String ..@ here too (though less performant).
readBase16 :: String -> BS.Strict.ByteString
readBase16 = BS.Strict.pack . translate

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
