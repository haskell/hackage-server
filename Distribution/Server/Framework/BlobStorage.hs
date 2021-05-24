{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables, TypeFamilies, BangPatterns, CPP,
             RecordWildCards #-}
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
    readBlobId,
    blobETag,
    blobMd5Digest,
    open,
    add,
    addWith,
    consumeFile,
    consumeFileWith,
    fetch,
    filepath,
    BlobId_v0,
    BlobStores(..),
    find,
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Features.Security.MD5
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.CacheControl (ETag(..))
import Distribution.Server.Util.ReadDigest

import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize
import System.FilePath ((</>))
import Control.Exception (handle, throwIO, evaluate, bracket)
import Data.SafeCopy
import System.Directory
import System.IO
import Data.Aeson
import System.Posix.Files as Posix (createLink)

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
  toJSON = toJSON . blobMd5

blobMd5Digest :: BlobId -> MD5Digest
blobMd5Digest (BlobId digest) = digest

blobMd5 :: BlobId -> String
blobMd5 (BlobId digest) = show digest

blobETag :: BlobId -> ETag
blobETag = ETag . blobMd5

readBlobId :: String -> Either String BlobId
readBlobId = either Left (Right . BlobId) . readDigest

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
hBlobId hnd = evaluate . BlobId . md5 =<< BSL.hGetContents hnd

fileBlobId :: FilePath -> IO BlobId
fileBlobId file = bracket (openBinaryFile file ReadMode) hClose hBlobId

withIncoming :: BlobStorage -> BSL.ByteString
              -> (FilePath -> BlobId -> IO (a, Bool))
              -> IO a
withIncoming store content action = do
    (file, hnd) <- openBinaryTempFile (incomingDir store) "new"
    handleExceptions file hnd $ do
        blobId <- BlobId <$> hPutGetMd5 hnd content
        fd <- handleToFd hnd -- This closes the handle, see docs for handleToFd
        fsync fd
        closeFd fd
        withIncoming' store file blobId action
  where
    hPutGetMd5 hnd = go . lazyMD5
      where
        go (BsChunk bs cs) = BSS.hPut hnd bs >> go cs
        go (BsEndMd5 md5val)  = return md5val

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

instance SafeCopy BlobId_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Migrate BlobId where
    type MigrateFrom BlobId = BlobId_v0
    migrate (BlobId_v0 digest) = BlobId digest

{-------------------------------------------------------------------------------
  Multiple blob stores
-------------------------------------------------------------------------------}

data BlobStores = BlobStores {
    -- | Main blob store
    blobStoresMain :: BlobStorage

    -- | Auxiliary blob stores
    --
    -- These are used only when adding new blobs into the main blob store
  , blobStoresAux :: [BlobStorage]
  }

-- | Check if a blob with the specified ID exists in any of the stores, and if
-- exists, hardlink it to the main store.
find :: BlobStores -> BlobId -> IO Bool
find BlobStores{..} blobId = do
     existsInMainStore <- doesFileExist $ filepath blobStoresMain blobId
     if existsInMainStore
       then return True
       else checkAux blobStoresAux
  where
    checkAux :: [BlobStorage] -> IO Bool
    checkAux []             = return False
    checkAux (store:stores) = do
       existsHere <- doesFileExist pathHere
       if existsHere
         then createLink pathHere pathMain >> return True
         else checkAux stores
     where
       pathHere = filepath store          blobId
       pathMain = filepath blobStoresMain blobId
