-- | SHA256 digest
module Distribution.Server.Features.Security.SHA256 (
    SHA256Digest
  , sha256
  ) where

-- stdlibs
import Control.DeepSeq
import Control.Monad
import Data.SafeCopy
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as BS.Lazy

-- cryptohash
import qualified Crypto.Hash.SHA256 as SHA256

-- hackage
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.ReadDigest

-- | SHA256 digest.
--
-- Internal invariant: @BS.length . sha256Digest == const 32@
newtype SHA256Digest = SHA256Digest {
    -- TODO: currently takes up 9 words on 64bit and uses pinned memory
    --       could use ShortByteString instead, or
    --
    --  data SHA256Digest = SHA256Digest !Word64 !Word64 !Word64 !Word64
    --
    -- which would get us unpinned memory, and down to the optimum of
    -- 5 Words on 64bit
    sha256Digest :: BS.ByteString
  }
  deriving (Eq)

-- | SHA256Digest is a @newtype@ wrapper around a 'BS.Bytestring' holding
-- the raw 32-byte SHA256 digest value; hence WHNF = NF.
instance NFData SHA256Digest where
  rnf digest = digest `seq` ()

-- | The 'Show' instance for 'SHA256Digest' prints the underlying digest
-- (without showing the newtype wrapper)
--
-- For legacy reasons, this instance emits the base16 encoded digest
-- string without surrounding quotation marks
instance Show SHA256Digest where
  show = BS.Char8.unpack . B16.encode . sha256Digest

instance ReadDigest SHA256Digest where
  -- NOTE: This differs in an important way from the 'Serialize' instance:
  -- the base16-encoded digest doesn't have a length prefix
  readDigest str =
      case B16.decode (BS.Char8.pack str) of
          (d,rest) | BS.null rest
                   , BS.length d == 32 -> Right $ SHA256Digest d
                   | otherwise         -> Left $ "Could not decode SHA256 " ++
                                                 show str

-- | Compute SHA256 digest
sha256 :: BS.Lazy.ByteString -> SHA256Digest
sha256 = SHA256Digest . SHA256.hashlazy

instance MemSize SHA256Digest where
  memSize _ = 9 -- memSize (Data.ByteString.replicate 32 0)

instance SafeCopy SHA256Digest where
  -- use default Serialize instance

-- For legacy reasons this length-prefixes the digest
instance Serialize SHA256Digest where
  put = put . sha256Digest
  get = do bs <- get
           unless (BS.length bs == 32) $
               fail "Bytestring of the wrong length"
           return (SHA256Digest bs)
