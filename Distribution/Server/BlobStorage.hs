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
module Distribution.Server.BlobStorage (
    BlobStorage,
    BlobId,
    open,
    add,
    fetch,
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5
         ( MD5Digest, md5 )
import System.FilePath
         ( (</>) )
import Control.Exception as Exception
import System.Directory
import System.IO

-- | An id for a blob. The content of the blob is stable.
--
newtype BlobId = BlobId MD5Digest

-- | A persistent blob storage area. Blobs can be added and retrieved but
-- not removed or modified.
--
newtype BlobStorage = BlobStorage FilePath -- ^ location of the store

filepath :: BlobStorage -> BlobId -> FilePath
filepath (BlobStorage store) (BlobId hash) = store </> show hash

incomingDir :: BlobStorage -> FilePath
incomingDir (BlobStorage store) = store </> "incoming"

-- | Add a blob into the store. The result is a 'BlobId' that can be used
-- later with 'fetch' to retrieve the blob content.
--
-- * This operation is idempotent. That is, adding the same content again
--   gives the same 'BlobId'.
--
add :: BlobStorage -> ByteString -> IO BlobId
add store content = do
  (file, hnd) <- openBinaryTempFile (incomingDir store) "tmp"
  handleExceptions file hnd $ do
    -- TODO: calculate the md5 and write to the temp file in one pass:
    BS.hPut hnd content
    hSeek hnd AbsoluteSeek 0
    blobId <- evaluate . BlobId . md5 =<< BS.hGetContents hnd
    --TODO: if the target already exists then there is no need to overwrite it
    --      since it will have the same content. Checking and then renaming
    --      would give a race condition but that's ok since they have the same
    --      content.
    renameFile file (filepath store blobId)
    return blobId

  where
    handleExceptions tmpFile tmpHandle =
      Exception.handle $ \err -> do
        hClose tmpHandle
        removeFile tmpFile
        Exception.throwIO err

-- | Retrieve a blob from the store given its 'BlobId'.
--
-- * The content corresponding to a given 'BlobId' never changes.
--
-- * The blob must exist in the store or it is an error.
--
fetch :: BlobStorage -> BlobId -> IO ByteString
fetch store blobid = BS.readFile (filepath store blobid)

-- | Opens an existing blob storage area.
--
-- * The given directory have previously been 'initialise'd.
--
open :: FilePath -> IO BlobStorage
open pkgDir
    = do createDirectoryIfMissing False pkgDir
         let blob = BlobStorage pkgDir
         createDirectoryIfMissing False (incomingDir blob)
         return blob

-- | Initialise a new persistent blob storage area.
--
-- * The given directory must not already exist.
--
initialise :: FilePath -> IO BlobStorage
initialise = undefined
