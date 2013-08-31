{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
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
    open,
    add,
    addWith,
    consumeFile,
    consumeFileWith,
    fetch,
    filepath,
  ) where

import Distribution.Server.Framework.MemSize

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.MD5 as MD5
import Data.Typeable (Typeable)
import Data.Serialize
import System.FilePath ((</>))
import Control.Exception (handle, throwIO, evaluate, bracket)
import Control.Monad
import Data.SafeCopy
import System.Directory
import System.IO

-- For the lazy MD5 computation
import Data.IORef
import Data.Digest.Pure.MD5 (MD5Digest, MD5Context)
import Crypto.Classes (initialCtx, updateCtx, finalize, blockLength)
import Crypto.Types (ByteLength)
import Crypto.Util ((.::.))
import qualified Data.ByteString as BSS
import System.IO.Unsafe (unsafeInterleaveIO)

-- For fsync
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
import System.Posix.Types (Fd(..))
import System.Posix.IO (
    handleToFd
  , openFd
  , closeFd
  , defaultFileFlags
  , OpenMode(ReadOnly)
  )

foreign import ccall "fsync" c_fsync :: CInt -> IO CInt

-- | Binding to the C @fsync@ function
fsync :: Fd -> IO ()
fsync (Fd fd) =  throwErrnoIfMinus1_ "fsync" $ c_fsync fd

-- | An id for a blob. The content of the blob is stable.
--
newtype BlobId = BlobId MD5Digest
  deriving (Eq, Ord, Show, Serialize, Typeable)

blobMd5 :: BlobId -> String
blobMd5 (BlobId digest) = show digest

instance SafeCopy BlobId where
  putCopy = contain . put
  getCopy = contain get

instance MemSize BlobId where
  memSize _ = 7 --TODO: pureMD5 package wastes 5 words!

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
        (content', md5) <- lazyMD5 content
        BSL.hPut hnd content'
        blobId <- evaluate $ BlobId md5
        fd <- handleToFd hnd -- This closes the handle, see docs for handleToFd
        fsync fd
        closeFd fd
        withIncoming' store file blobId action
  where
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
              -- TODO: if the target already exists then there is no need to overwrite
              -- it since it will have the same content. Checking and then renaming
              -- would give a race condition but that's ok since they have the same
              -- content.
              renameFile file (filepath store blobId)

              -- fsync the directory so that we the write becomes 'durable'
              fd <- openFd (directory store blobId) ReadOnly Nothing defaultFileFlags
              fsync fd
              closeFd fd
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
-- >   (bs', md5) <- lazyMD5 bs
-- >   BSL.writeFile destPath bs'
-- >   putStrLn $ "MD5 is " ++ show md5
--
-- Note that this lazily reads the file, then lazily writes it again, and
-- finally we get the MD5 checksum of the whole file. This example program
-- will run in constant memory.
--
-- Important: do not attempt to read the MD5 checksum until you are sure that
-- the entire bytestring has been confumsed (you will get an exception
-- otherwise).
lazyMD5 :: BSL.ByteString -> IO (BSL.ByteString, MD5Digest)
lazyMD5 = \inp -> do
    md5ref <- newIORef (Left initialCtx)
    md5    <- finalMD5 md5ref

    let blockLen = (blockLength .::. md5) `div` 8

    outp <- liftM BSL.fromChunks . go md5ref . makeBlocks blockLen $ inp
    return (outp, md5)
  where
    go :: IORef (Either MD5Context MD5Digest)
       -> [BSS.ByteString]
       -> IO [BSS.ByteString]
    go md5ref blocks = unsafeInterleaveIO $ do
      Left md5ctx <- readIORef md5ref
      case blocks of
        [lastBlock] -> do
          md5ctx' <- evaluate $ finalize md5ctx lastBlock
          writeIORef md5ref (Right md5ctx')
          return [lastBlock]
        (block : blocks') -> do
          md5ctx' <- evaluate $ updateCtx md5ctx block
          writeIORef md5ref (Left md5ctx')
          outp <- go md5ref blocks'
          return (block : outp)
        [] -> error "impossible"

    finalMD5 :: IORef (Either MD5Context MD5Digest) -> IO MD5Digest
    finalMD5 md5ref = unsafeInterleaveIO $ do
      m_md5 <- readIORef md5ref
      case m_md5 of
        Left  _   -> throwIO (userError "lazyMD5: attempt to read MD5 before bytestring was consumed")
        Right md5 -> return md5
