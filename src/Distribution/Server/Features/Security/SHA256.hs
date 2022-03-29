{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- | SHA256 digest
module Distribution.Server.Features.Security.SHA256 (
    SHA256Digest
  , sha256

  , sha256DigestBytes
  ) where

-- stdlibs
import           Distribution.Server.Prelude

import           Control.DeepSeq
import           Data.Aeson                            (ToJSON (toJSON))
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Base16                as B16
import           Data.SafeCopy
import           Data.Serialize                        as Ser
#if MIN_VERSION_binary(0,8,3)
import           Data.ByteString.Builder.Extra         as BS
#endif
import qualified Data.Binary                           as Bin
import qualified Data.Binary.Put                       as Bin
import qualified Data.ByteString.Char8                 as BS.Char8
import qualified Data.ByteString.Lazy                  as BS.Lazy
import qualified Data.Text.Encoding                    as T

-- cryptohash
import qualified Crypto.Hash.SHA256                    as SHA256

-- hackage
import           Distribution.Server.Framework.MemSize
import           Distribution.Server.Util.ReadDigest

-- | SHA256 digest
data SHA256Digest = SHA256Digest {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
                                 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
                  deriving (Eq)

instance NFData SHA256Digest where
  rnf !_ = () -- 'SHA256Digest' has only strict primitive fields, hence WHNF==NF

-- internal convenience helper
-- fails if input has wrong length; callers must ensure correct length
sha256digestFromBS :: BS.ByteString -> SHA256Digest
sha256digestFromBS bs = case runGet getSHA256NoPfx bs of
    Left  e -> error ("sha256digestFromBS: " ++ e)
    Right d -> d

-- | 'Data.Serialize.Get' helper to read a raw 32byte SHA256Digest w/o
-- any length-prefix
getSHA256NoPfx :: Get SHA256Digest
getSHA256NoPfx = SHA256Digest <$> getWord64be
                              <*> getWord64be
                              <*> getWord64be
                              <*> getWord64be

-- | The 'Show' instance for 'SHA256Digest' prints the underlying digest
-- (without showing the newtype wrapper)
--
-- For legacy reasons, this instance emits the base16 encoded digest
-- string without surrounding quotation marks
instance Show SHA256Digest where
  show = BS.Char8.unpack . B16.encode . sha256DigestBytes

-- | Encodes SHA256 hash as lower-case base-16 JSON string; i.e. as 64
-- hex-digits.
instance ToJSON SHA256Digest where
  toJSON = toJSON . T.decodeLatin1 . B16.encode . sha256DigestBytes

instance ReadDigest SHA256Digest where
  -- NOTE: This differs in an important way from the 'Serialize' instance:
  -- the base16-encoded digest doesn't have a length prefix
  readDigest str =
      case B16.decode (BS.Char8.pack str) of
          Right d | BS.length d == 32 -> Right $! sha256digestFromBS d
          _  -> Left $ "Could not decode SHA256 " ++ show str

-- | Compute SHA256 digest
sha256 :: BS.Lazy.ByteString -> SHA256Digest
sha256 = sha256digestFromBS . SHA256.hashlazy

instance MemSize SHA256Digest where
  memSize _ = 5

instance SafeCopy SHA256Digest where
    getCopy = contain get
    putCopy = contain . put
  -- use default Serialize instance

-- For legacy reasons this length-prefixes the serialised digest
instance Serialize SHA256Digest where
  put (SHA256Digest w1 w2 w3 w4) =
      do put (32 :: Int)
         putWord64be w1
         putWord64be w2
         putWord64be w3
         putWord64be w4

  get = do lenpfx <- get
           unless (lenpfx == (32 :: Int)) $
               fail "Bytestring of the wrong length"
           getSHA256NoPfx

-- | Export 'SHA256Digest' as raw 16-byte 'BS.ByteString' digest-value
sha256DigestBytes :: SHA256Digest -> BS.ByteString
sha256DigestBytes =
    toBs . putSHA256Digest
  where
    putSHA256Digest :: SHA256Digest -> Bin.PutM ()
    putSHA256Digest (SHA256Digest w1 w2 w3 w4)
        = Bin.putWord64be w1 >> Bin.putWord64be w2 >>
          Bin.putWord64be w3 >> Bin.putWord64be w4

    toBs :: Bin.Put -> BS.ByteString
#if MIN_VERSION_binary(0,8,3)
    -- with later binary versions we can control the buffer size precisely:
    toBs = BS.Lazy.toStrict
         . BS.toLazyByteStringWith (BS.untrimmedStrategy 32 0) BS.Lazy.empty
         . Bin.execPut
#else
    toBs = BS.Lazy.toStrict . Bin.runPut
#endif
