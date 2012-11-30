{-
Create a tarball with the structured defined by each individual feature.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Framework.BackupDump (
    exportTar,
    csvToBackup,
    blobToBackup,

    stringToBytes,

    testBlobsExist
  ) where

import Distribution.Simple.Utils (toUTF8)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.CSV hiding (csv)

import Distribution.Server.Framework.BackupRestore (BackupEntry(..))
import qualified Distribution.Server.Framework.BlobStorage as Blob

import Distribution.Server.Framework.BlobStorage

--import Distribution.Text

import qualified Data.ByteString.Lazy.Char8 as BS
import Codec.Compression.GZip (compress)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Exception as Exception
import Control.Monad (liftM, forM)
import System.FilePath
import System.Locale
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Maybe (catMaybes)
import Data.Time

exportTar :: BlobStorage -> [(String, IO [BackupEntry])] -> IO BS.ByteString
exportTar store = fmap (compress . Tar.write) . toEntries store

-- this is probably insufficiently lazy. use unsafeInterleaveIO to avoid loading /everything/ into memory
toEntries :: BlobStorage -> [(String, IO [BackupEntry])] -> IO [Tar.Entry]
toEntries store featureMap = do
    baseDir <- mkBaseDir `fmap` getCurrentTime
    let exportEntries :: (String, IO [BackupEntry]) -> IO [Tar.Entry]
        exportEntries (name, ioEntries) = do
            backupEntries <- ioEntries
            tarEntries <- forM backupEntries $ backupToTar store baseDir name
            return (concat tarEntries)
    unsafeInterleaveConcatMap exportEntries featureMap

backupToTar :: BlobStorage -> FilePath -> String -> BackupEntry -> IO [Tar.Entry]
backupToTar _store baseDir featureName (BackupByteString path bs) =
  case Tar.toTarPath False (mkTarPath baseDir featureName path) of
    Right tarPath -> return [Tar.fileEntry tarPath bs]
    Left err -> error $ "Error in export: " ++ err
backupToTar store baseDir featureName (BackupBlob path blobId) = do
  contents <- unsafeInterleaveIO $ Blob.fetch store blobId
  let blobPath = baseDir </> "blobs" </> blobMd5 blobId
      -- go back to the root of the tarball. 'path' here would be something like
      -- a/b/c/filename, which we store in feature/a/b/c/filename; in this
      -- example, we want to go back 4 directories (feature/a/b/c), which is
      -- @length path@ (because 'path' includes the filename)
      blobLink = joinPath (replicate (length path) "..") </> "blobs" </> blobMd5 blobId
  -- Sigh. Lots of annoying tar path conversions
  case Tar.toTarPath False (mkTarPath baseDir featureName path) of
    Left err -> error $ "Error in export: " ++ err
    Right tarPath -> case Tar.toTarPath False blobPath of
      Left err -> error $ "Error in export: " ++ err
      Right blobTarPath -> case Tar.toLinkTarget blobLink of
        Nothing -> error "Could not generate link target"
        Just linkTarget ->
          return [ Tar.fileEntry blobTarPath contents
                 , Tar.simpleEntry tarPath (Tar.SymbolicLink linkTarget)
                 ]

mkTarPath :: FilePath -> String -> [FilePath] -> FilePath
mkTarPath baseDir featureName ps = joinPath $ baseDir : featureName : ps

csvToBackup :: [String] -> CSV -> BackupEntry
csvToBackup fpath csv = BackupByteString fpath $ BS.pack (printCSV csv)

blobToBackup :: [String] -> BlobId -> BackupEntry
blobToBackup = BackupBlob

mkBaseDir :: UTCTime -> FilePath
mkBaseDir time = "export-" ++ formatTime defaultTimeLocale (iso8601DateFormat Nothing) time

{- let's be crazy lazy

   The only non-pure operations we do are reading files
   from the blob-storage, which is already lazy IO.

   So we may as well not force the spine of the tar-ball
   before we need to.
-}
unsafeInterleaveConcatMap :: (a -> IO [b]) -> [a] -> IO [b]
unsafeInterleaveConcatMap f = go
  where
    go [] = return []
    go (x:xs) = do
        ys <- f x
        yss <- unsafeInterleaveIO $ go xs
        return (ys++yss)

-- via UTF8 conversion.
stringToBytes :: String -> BSL.ByteString
stringToBytes = BSL.pack . toUTF8

testBlobsExist :: BlobStorage -> [Blob.BlobId] -> IO [String]
testBlobsExist store blobs
  = liftM catMaybes $ forM blobs $ \blob -> do
       (Blob.fetch store blob >> return Nothing) `Exception.catch`
         \e -> return $ Just $ "Could not open blob " ++ show blob ++ ": " ++ show (e :: IOError)
