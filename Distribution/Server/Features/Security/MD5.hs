{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

-- | MD5 digest
module Distribution.Server.Features.Security.MD5 (
    MD5Digest
  , md5

  , md5DigestBytes

  , lazyMD5
  , ByteStringWithMd5(..)
  ) where

-- stdlibs
import           Distribution.Server.Prelude

import           Control.DeepSeq
import qualified Data.Binary                           as Bin
import qualified Data.Binary.Put                       as Bin
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Base16                as B16
#if MIN_VERSION_binary(0,8,3)
import           Data.ByteString.Builder.Extra         as BS
#endif
import qualified Data.ByteString.Char8                 as BS.Char8
import qualified Data.ByteString.Lazy                  as BS.Lazy
import           Data.SafeCopy
import qualified Data.Serialize                        as Ser

-- cryptohash
import qualified Crypto.Hash.MD5                       as MD5

-- hackage
import           Distribution.Server.Framework.MemSize
import           Distribution.Server.Util.ReadDigest

-- | MD5 digest
data MD5Digest = MD5Digest {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
               deriving (Eq,Ord)

instance NFData MD5Digest where
    rnf !_ = () -- 'MD5Digest' has only strict primitive fields, hence WHNF==NF

-- internal convenience helper
-- fails if input has wrong length; callers must ensure correct length
md5digestFromBS :: BS.ByteString -> MD5Digest
md5digestFromBS bs = case Ser.runGet (Ser.get :: Ser.Get MD5Digest) bs of
    Left  e -> error ("md5digestFromBS: " ++ e)
    Right d -> d

-- | The 'Show' instance for 'MD5Digest' prints the underlying digest
-- (without showing the newtype wrapper)
--
-- For legacy reasons, this instance emits the base16 encoded digest
-- string without surrounding quotation marks
instance Show MD5Digest where
  show = BS.Char8.unpack . B16.encode . md5DigestBytes

instance ReadDigest MD5Digest where
  readDigest str =
      case B16.decode (BS.Char8.pack str) of
          Right d | BS.length d == 16 -> Right $! md5digestFromBS d
          _ -> Left $ "Could not decode MD5 " ++ show str

-- | Compute MD5 digest
md5 :: BS.Lazy.ByteString -> MD5Digest
md5 = md5digestFromBS . MD5.hashlazy

instance MemSize MD5Digest where
    memSize _ = 3

instance SafeCopy MD5Digest where
    getCopy = contain Ser.get
    putCopy = contain . Ser.put
    -- use default Serialize instance

instance Ser.Serialize MD5Digest where
    put (MD5Digest w1 w2) = Ser.putWord64be w1 >> Ser.putWord64be w2
    get = MD5Digest <$> Ser.getWord64be <*> Ser.getWord64be


-- We don't need a 'Binary' instance currently (previously, it was
-- needed for md5DigestBytes), and SHA256Digest doesn't have one either
--
-- instance Bin.Binary MD5Digest where
--     put (MD5Digest w1 w2) = Bin.putWord64be w1 >> Bin.putWord64be w2
--     get = MD5Digest <$> Bin.getWord64be <*> Bin.getWord64be


-- | Compute the MD5 checksum of a lazy bytestring without reading the entire
-- thing into memory
--
-- Example usage:
--
-- > do bs <- BS.Lazy.readFile srcPath
-- >   md5 <- writeFileGetMd5 destPath bs'
-- >   putStrLn $ "MD5 is " ++ show md5
-- > where
-- >   writeFileGetMd5 file content =
-- >       withFile file WriteMode $ \hnd ->
-- >         go hnd (lazyMD5 content)
-- >     where
-- >       go hnd (BsChunk bs cs) = BS.hPut hnd bs >> go hnd cs
-- >       go _   (BsEndMd5 md5)  = return md5
--
-- Note that this lazily reads the file, then lazily writes it again, and
-- finally we get the MD5 checksum of the whole file. This example program
-- will run in constant memory.
--
lazyMD5 :: BS.Lazy.ByteString -> ByteStringWithMd5
lazyMD5 = go MD5.init . BS.Lazy.toChunks
  where
    go :: MD5.Ctx -> [BS.ByteString] -> ByteStringWithMd5
    go !md5ctx [] =
      BsEndMd5 (md5digestFromBS (MD5.finalize md5ctx))
    go !md5ctx (block : blocks') =
      BsChunk block (go (MD5.update md5ctx block) blocks')

-- | See 'lazyMD5'
data ByteStringWithMd5 = BsChunk  !BS.ByteString ByteStringWithMd5
                       | BsEndMd5 !MD5Digest

-- | Export 'MD5Digest' as raw 16-byte 'BS.ByteString' digest-value
md5DigestBytes :: MD5Digest -> BS.ByteString
md5DigestBytes =
    toBs . putMD5Digest
  where
    putMD5Digest :: MD5Digest -> Bin.PutM ()
    putMD5Digest (MD5Digest w1 w2) = Bin.putWord64be w1 >> Bin.putWord64be w2

    toBs :: Bin.Put -> BS.ByteString
#if MIN_VERSION_binary(0,8,3)
    -- with later binary versions we can control the buffer size precisely:
    toBs = BS.Lazy.toStrict
         . BS.toLazyByteStringWith (BS.untrimmedStrategy 16 0) BS.Lazy.empty
         . Bin.execPut
#else
    toBs = BS.Lazy.toStrict . Bin.runPut
#endif
