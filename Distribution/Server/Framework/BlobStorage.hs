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
    addFileWith,
    fetch,
    filepath,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5 (MD5Digest, md5)
import Data.Typeable (Typeable)
import Data.Serialize
import System.FilePath ((</>))
import Control.Exception (handle, throwIO, evaluate)
import Control.Monad
import Data.SafeCopy
import System.Directory
import System.IO

-- | An id for a blob. The content of the blob is stable.
--
newtype BlobId = BlobId MD5Digest
  deriving (Eq, Ord, Show, Serialize, Typeable)

blobMd5 :: BlobId -> String
blobMd5 (BlobId digest) = show digest

instance SafeCopy BlobId where
  putCopy = contain . put
  getCopy = contain get

-- | A persistent blob storage area. Blobs can be added and retrieved but
-- not removed or modified.
--
newtype BlobStorage = BlobStorage FilePath -- ^ location of the store

filepath :: BlobStorage -> BlobId -> FilePath
filepath (BlobStorage storeDir) (BlobId hash)
    = storeDir </> take 2 str </> str
    where str = show hash

incomingDir :: BlobStorage -> FilePath
incomingDir (BlobStorage storeDir) = storeDir </> "incoming"

-- | Add a blob into the store. The result is a 'BlobId' that can be used
-- later with 'fetch' to retrieve the blob content.
--
-- * This operation is idempotent. That is, adding the same content again
--   gives the same 'BlobId'.
--
add :: BlobStorage -> ByteString -> IO BlobId
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
addWith :: BlobStorage -> ByteString
        -> (ByteString -> IO (Either error result))
        -> IO (Either error (result, BlobId))
addWith store content check =
  withIncoming store content $ \file blobId -> do
    content' <- BS.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

addFileWith :: BlobStorage -> FilePath
        -> (ByteString -> IO (Either error result))
        -> IO (Either error (result, BlobId))
addFileWith store filePath check =
  withIncomingFile store filePath $ \file blobId -> do
    content' <- BS.readFile file
    result <- check content'
    case result of
      Left  err -> return (Left  err,          False)
      Right res -> return (Right (res, blobId), True)

hBlobId :: Handle -> IO BlobId
hBlobId hnd = evaluate . BlobId . md5 =<< BS.hGetContents hnd

fpBlobId :: FilePath -> IO BlobId
fpBlobId file =
    do hnd <- openBinaryFile file ReadMode 
       blobId <- hBlobId hnd
       hClose hnd
       return blobId 

withIncoming :: BlobStorage -> ByteString
              -> (FilePath -> BlobId -> IO (a, Bool))
              -> IO a
withIncoming store content action = do
    (file, hnd) <- openBinaryTempFile (incomingDir store) "new"
    handleExceptions file hnd $ do
        -- TODO: calculate the md5 and write to the temp file in one pass:
        BS.hPut hnd content
        hSeek hnd AbsoluteSeek 0
        blobId <- hBlobId hnd
        hClose hnd
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
    do blobId <- fpBlobId file
       withIncoming' store file blobId action

withIncoming' :: BlobStorage -> FilePath -> BlobId -> (FilePath -> BlobId -> IO (a, Bool)) -> IO a
withIncoming' store file blobId action = do
        -- open a new Handle since the old one is closed by hGetContents
        (res, commit) <- action file blobId
        if commit
            --TODO: if the target already exists then there is no need to overwrite
            -- it since it will have the same content. Checking and then renaming
            -- would give a race condition but that's ok since they have the same
            -- content.
            then renameFile file (filepath store blobId)
            else removeFile file
        return res


-- | Retrieve a blob from the store given its 'BlobId'.
--
-- * The content corresponding to a given 'BlobId' never changes.
--
-- * The blob must exist in the store or it is an error.
--
fetch :: BlobStorage -> BlobId -> IO ByteString
fetch store blobid = BS.readFile (filepath store blobid)

-- | Opens an existing or new blob storage area.
--
open :: FilePath -> IO BlobStorage
open storeDir = do
    let store = BlobStorage storeDir

    exists <- doesDirectoryExist storeDir
    unless exists $ do
        let chars = ['0' .. '9'] ++ ['a' .. 'f']
            chars2 = [ [x, y] | x <- chars, y <- chars ]
        createDirectory storeDir
        sequence_ [ createDirectory (storeDir </> x)
                  | x <- chars2 ]
        createDirectory (incomingDir store)

    return store

