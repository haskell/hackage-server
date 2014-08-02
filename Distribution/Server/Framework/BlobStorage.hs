{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeFamilies, BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.BlobStorage
-- Copyright   :  Duncan Coutts <duncan@haskell.org>
--
-- Maintainer  :  Duncan Coutts <duncan@haskell.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Persistent storage for blobs of data.
--
module Distribution.Server.Framework.BlobStorage (
    BlobStorage,
    BlobId,
    blobMd5,
    blobETag,
    open,
    add,
    addWith,
    consumeFile,
    consumeFileWith,
    fetch,
    filepath,
    BlobId_v0,
  ) where

import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Util.Happstack (ETag(..))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Typeable (Typeable)
import Data.Serialize
import System.FilePath ((</>))
import Control.Exception (handle, throwIO, evaluate, bracket)
import Control.Monad
import Control.Applicative
import Data.SafeCopy
import System.Directory
import System.IO
import Data.Aeson

-- For the lazy MD5 computation
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context)
import Crypto.Classes (initialCtx, updateCtx, finalize, blockLength)
import Crypto.Types (ByteLength)
import qualified Crypto.Util (for)
import qualified Data.ByteString as BSS

-- For fsync
import System.Posix.Types (Fd(..))
#ifndef mingw32_HOST_OS
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import System.Posix.IO (
    handleToFd
  , openFd
  , closeFd
  , defaultFileFlags
  , OpenMode(ReadOnly)
  )
#endif

-- | An id for a blob. The content of the blob is stable.
--
newtype BlobId = BlobId MD5Digest
  deriving (Eq, Ord, Show, Typeable, MemSize)

instance ToJSON BlobId where
  toJSON (BlobId md5digest) = toJSON (show md5digest)

blobMd5 :: BlobId -> String
blobMd5 (BlobId digest) = show digest

blobETag :: BlobId -> ETag
blobETag = ETag . blobMd5

instance SafeCopy BlobId where
  version = 2
  kind    = extension
  putCopy (BlobId x) = contain $ put x
  getCopy = contain $ BlobId <$> get

-- | A persistent blob storage area. Blobs can be added and retrieved but
-- not removed or modified.
--
newtype BlobStorage = BlobStorage FilePath -- ^ location of the store

-- | Which directory do we store a blob ID in?
directory :: BlobStorage -> BlobId -> FilePath
directory (BlobStorage storeDir) (BlobId hash) = storeDir </> take 2 str
    where str = show hash

filepath :: BlobStorage -> BlobId -> FilePath
filepath store bid@(BlobId hash) = directory store bid </> str
    where str = show hash

incomingDir :: BlobStorage -> FilePath
incomingDir (BlobStorage storeDir) = storeDir </> "incoming"

-- | Add a blob into the store. The result is a 'BlobId' that can be used
-- later with 'fetch' to retrieve the blob content.
--
-- * This operation is idempotent. That is, adding the same content again
--   gives the same 'BlobId'.
--
add :: BlobStorage -> BSL.ByteString -> IO BlobId
add store content =
  withIncoming store content $ \_ blobId -> return (blobId, True)

-- | Like 'add' but we get another chance to make another pass over the input
-- 'ByteString'.
--
-- What happens is that we stream the input into a temp file in an incoming
-- area. Then we can make a second pass over it to do some validation or
-- processing. If the validator decides to reject then we rollback and the
-- blob is not entered into the store. If it accepts then the blob is added
-- and the 'BlobId' is returned.
--
addWith :: BlobStorage -> BSL.ByteString
        -> (BSL.ByteString -> IO (Either error result))
        -> IO (Either error (result, BlobId))
addWith store content check =
  withIncoming store content $ \file blobId -> do
    content' <- BSL.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

-- | Similar to 'add' but by /moving/ a file into the blob store. So this
-- is a destructive operation. Since it works by renaming the file, the input
-- file must live in the same file system as the blob store.
--
consumeFile :: BlobStorage -> FilePath -> IO BlobId
consumeFile store filePath =
  withIncomingFile store filePath $ \_ blobId -> return (blobId, True)

consumeFileWith :: BlobStorage -> FilePath
                -> (BSL.ByteString -> IO (Either error result))
                -> IO (Either error (result, BlobId))
consumeFileWith store filePath check =
  withIncomingFile store filePath $ \file blobId -> do
    content' <- BSL.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

hBlobId :: Handle -> IO BlobId
hBlobId hnd = evaluate . BlobId . MD5.md5 =<< BSL.hGetContents hnd

fileBlobId :: FilePath -> IO BlobId
fileBlobId file = bracket (openBinaryFile file ReadMode) hClose hBlobId

withIncoming :: BlobStorage -> BSL.ByteString
              -> (FilePath -> BlobId -> IO (a, Bool))
              -> IO a
withIncoming store content action = do
    (file, hnd) <- openBinaryTempFile (incomingDir store) "new"
    handleExceptions file hnd $ do
        md5 <- hPutGetMd5 hnd content
        let blobId = BlobId md5
        fd <- handleToFd hnd -- This closes the handle, see docs for handleToFd
        fsync fd
        closeFd fd
        withIncoming' store file blobId action
  where
    hPutGetMd5 hnd = go . lazyMD5
      where
        go (BsChunk bs cs) = BSS.hPut hnd bs >> go cs
        go (BsEndMd5 md5)  = return md5

    handleExceptions tmpFile tmpHandle =
      handle $ \err -> do
        hClose tmpHandle
        removeFile tmpFile
        throwIO (err :: IOError)

withIncomingFile :: BlobStorage
                 -> FilePath
                 -> (FilePath -> BlobId -> IO (a, Bool))
                 -> IO a
withIncomingFile store file action =
    do blobId <- fileBlobId file
       withIncoming' store file blobId action

withIncoming' :: BlobStorage -> FilePath -> BlobId -> (FilePath -> BlobId -> IO (a, Bool)) -> IO a
withIncoming' store file blobId action = do
        (res, commit) <- action file blobId
        if commit
            then do
#ifndef mingw32_HOST_OS
              fd <- openFd (directory store blobId) ReadOnly Nothing defaultFileFlags
#endif
              -- TODO: if the target already exists then there is no need to overwrite
              -- it since it will have the same content. Checking and then renaming
              -- would give a race condition but that's ok since they have the same
              -- content.
              renameFile file (filepath store blobId)
#ifndef mingw32_HOST_OS
              -- fsync the directory so that the new directory entry becomes 'durable'
              fsync fd
              closeFd fd
#endif
            else removeFile file
        return res


-- | Retrieve a blob from the store given its 'BlobId'.
--
-- * The content corresponding to a given 'BlobId' never changes.
--
-- * The blob must exist in the store or it is an error.
--
fetch :: BlobStorage -> BlobId -> IO BSL.ByteString
fetch store blobid = BSL.readFile (filepath store blobid)

-- | Opens an existing or new blob storage area.
--
open :: FilePath -> IO BlobStorage
open storeDir = do
    let store   = BlobStorage storeDir
        chars   = ['0' .. '9'] ++ ['a' .. 'f']
        subdirs = incomingDir store
                : [storeDir </> [x, y] | x <- chars, y <- chars]

    exists <- doesDirectoryExist storeDir
    if not exists
      then do
        createDirectory storeDir
        forM_ subdirs createDirectory
      else
        forM_ subdirs $ \d -> do
          subdirExists <- doesDirectoryExist d
          unless subdirExists $
            fail $ "Store directory \""
                ++ storeDir
                ++ "\" exists but \""
                ++ d
                ++ "\" does not"
    return store

{------------------------------------------------------------------------------
  Lazy MD5 computation
------------------------------------------------------------------------------}

-- | Adapted from crypto-api
--
-- This function is defined in crypto-api but not exported, and moreover
-- not lazy enough.
--
-- Guaranteed not to return an empty list
makeBlocks :: ByteLength -> BSL.ByteString -> [BSS.ByteString]
makeBlocks len = go . BSL.toChunks
  where
    go :: [BSS.ByteString] -> [BSS.ByteString]
    go [] = [BSS.empty]
    go (bs:bss)
      | BSS.length bs >= len =
          let l = BSS.length bs - BSS.length bs `rem` len
              (blocks, leftover) = BSS.splitAt l bs
          in blocks : go (leftover : bss)
      | otherwise =
          case bss of
            []           -> [bs]
            (bs' : bss') -> go (BSS.append bs bs' : bss')

-- | Compute the MD5 checksum of a lazy bytestring without reading the entire
-- thing into memory
--
-- Example usage:
--
-- > do bs <- BSL.readFile srcPath
-- >   md5 <- writeFileGetMd5 destPath bs'
-- >   putStrLn $ "MD5 is " ++ show md5
-- > where
-- >   writeFileGetMd5 file content =
-- >       withFile file WriteMode $ \hnd ->
-- >         go hnd (lazyMD5 content)
-- >     where
-- >       go hnd (BsChunk bs cs) = BSS.hPut hnd bs >> go hnd cs
-- >       go _   (BsEndMd5 md5)  = return md5
--
-- Note that this lazily reads the file, then lazily writes it again, and
-- finally we get the MD5 checksum of the whole file. This example program
-- will run in constant memory.
--
lazyMD5 :: BSL.ByteString -> ByteStringWithMd5
lazyMD5 = go initialCtx . makeBlocks blockLen
  where
    blockLen = (blockLength `Crypto.Util.for` (undefined :: MD5Digest)) `div` 8

    go :: MD5Context
       -> [BSS.ByteString]
       -> ByteStringWithMd5
    go !md5ctx [lastBlock] =
      BsChunk lastBlock $! (BsEndMd5 (finalize md5ctx lastBlock))
    go !md5ctx (block : blocks') =
      BsChunk block (go (updateCtx md5ctx block) blocks')
    go _ [] = error "impossible"

data ByteStringWithMd5 = BsChunk  !BSS.ByteString ByteStringWithMd5
                       | BsEndMd5 !MD5Digest

{------------------------------------------------------------------------------
  Lazy MD5 computation
------------------------------------------------------------------------------}

-- | Binding to the C @fsync@ function
fsync :: Fd -> IO ()
fsync (Fd fd) =
#ifdef mingw32_HOST_OS
  return ()
#else
 throwErrnoIfMinus1_ "fsync" $ c_fsync fd

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt
#endif

--------------------------
-- Old SafeCopy versions
--

newtype BlobId_v0 = BlobId_v0 MD5Digest deriving Serialize

instance SafeCopy BlobId_v0
instance Migrate BlobId where
    type MigrateFrom BlobId = BlobId_v0
    migrate (BlobId_v0 digest) = BlobId digest

