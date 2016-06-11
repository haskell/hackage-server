{-# LANGUAGE BangPatterns #-}

-- | MD5 digest
module Distribution.Server.Features.Security.MD5 (
    MD5Digest
  , md5

  , lazyMD5
  , ByteStringWithMd5(..)
  ) where

-- stdlibs
import Control.DeepSeq
import Data.SafeCopy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy

-- pureMD5
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context, md5)
import Crypto.Classes (initialCtx, updateCtx, finalize, blockLength)
import Crypto.Types (ByteLength)
import qualified Crypto.Util (for)

-- cryptohash-md5
-- TODO: import qualified Crypto.Hash.MD5 as MD5

-- hackage
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.ReadDigest

-- Orphans!
--
-- Every module which gets access to the MD5Digest type imports this
-- module, so we can place orphan instances here rather than in
-- Distribution.Server.Features.Security.Orphans
--
-- NB: This is only a temporary measure until pureMD5 is replaced with
-- cryptohash-md5, which will only require rewriting *this* module.

instance ReadDigest MD5Digest

instance NFData MD5Digest where
    rnf = rnf . show  --TODO: MD5Digest should be a newtype wrapper and an instance of NFData

instance MemSize MD5Digest where
    memSize _ = 7 --TODO: pureMD5 package wastes 5 words!

instance SafeCopy MD5Digest where
 -- use default Serialize instance (provided by the pureMD5 package)

{------------------------------------------------------------------------------
  Lazy MD5 computation
------------------------------------------------------------------------------}

-- | Adapted from crypto-api
--
-- This function is defined in crypto-api but not exported, and moreover
-- not lazy enough.
--
-- Guaranteed not to return an empty list
makeBlocks :: ByteLength -> BS.Lazy.ByteString -> [BS.ByteString]
makeBlocks len = go . BS.Lazy.toChunks
  where
    go :: [BS.ByteString] -> [BS.ByteString]
    go [] = [BS.empty]
    go (bs:bss)
      | BS.length bs >= len =
          let l = BS.length bs - BS.length bs `rem` len
              (blocks, leftover) = BS.splitAt l bs
          in blocks : go (leftover : bss)
      | otherwise =
          case bss of
            []           -> [bs]
            (bs' : bss') -> go (BS.append bs bs' : bss')

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
lazyMD5 = go initialCtx . makeBlocks blockLen
  where
    blockLen = (blockLength `Crypto.Util.for` (undefined :: MD5Digest)) `div` 8

    go :: MD5Context
       -> [BS.ByteString]
       -> ByteStringWithMd5
    go !md5ctx [lastBlock] =
      BsChunk lastBlock $! (BsEndMd5 (finalize md5ctx lastBlock))
    go !md5ctx (block : blocks') =
      BsChunk block (go (updateCtx md5ctx block) blocks')
    go _ [] = error "impossible"

data ByteStringWithMd5 = BsChunk  !BS.ByteString ByteStringWithMd5
                       | BsEndMd5 !MD5Digest

