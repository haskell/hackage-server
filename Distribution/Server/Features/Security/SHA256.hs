-- | SHA256 digest
module Distribution.Server.Features.Security.SHA256 (
    SHA256Digest
  , sha256
  ) where

-- stdlibs
import Control.DeepSeq
import Data.SafeCopy
import Data.Serialize
import qualified Data.ByteString.Lazy as BS.Lazy

-- cryptohash
import Data.Byteable (toBytes)
import qualified Crypto.Hash as Crypto

-- hackage
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.ReadDigest

-- | SHA256 digest
newtype SHA256Digest = SHA256Digest {
    sha256Digest :: Crypto.Digest Crypto.SHA256
  }
  deriving (Eq)

-- | SHA256Digest is a newtype wrapper around 'Crypto.Digest', which in turn
-- is a newtype wrapper around a string bytestring; hence WHNF = NF.
instance NFData SHA256Digest where
  rnf digest = digest `seq` ()

-- | The 'Show' instance for 'SHA256Digest' prints the underlying digest
-- (without showing the newtype wrapper)
instance Show SHA256Digest where
  show = show . sha256Digest

instance ReadDigest SHA256Digest where
  -- NOTE: This differs in an important way from the 'Serialize' instance:
  -- the base16-encoded digest doesn't have a length prefix
  readDigest str = case Crypto.digestFromByteString (readBase16 str) of
                     Nothing -> Left  $ "Could not decode SHA256 " ++ show str
                     Just d  -> Right $ SHA256Digest d

sha256 :: BS.Lazy.ByteString -> SHA256Digest
sha256 = SHA256Digest . Crypto.hashlazy

instance MemSize SHA256Digest where
  memSize _ = 8 -- 8 words @ 32 bits = 256 bits

instance SafeCopy SHA256Digest where
  -- use default Serialize instance

-- For legacy reasons this length-prefixes the digest
instance Serialize SHA256Digest where
  put = put . toBytes . sha256Digest
  get = do bs <- get
           case Crypto.digestFromByteString bs of
             Nothing -> fail "Bytestring of the wrong length"
             Just d  -> return $ SHA256Digest d
