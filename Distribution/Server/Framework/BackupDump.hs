{-
Create a tarball with the structured defined by each individual feature.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Framework.BackupDump (
    exportTar,
    ExportEntry,
    readExportBlobs,
    csvToBackup,
    csvToExport,
    blobToExport,

    stringToBytes,

    testRoundtripByQuery,
    testRoundtripByQuery',
    testBlobsExist
  ) where

import Distribution.Simple.Utils (toUTF8)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.CSV hiding (csv)

import Distribution.Server.Framework.BackupRestore (BackupEntry, TestRoundtrip)
import qualified Distribution.Server.Framework.BlobStorage as Blob

import Distribution.Server.Framework.BlobStorage

--import Distribution.Text

import qualified Data.ByteString.Lazy.Char8 as BS
import Codec.Compression.GZip (compress)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Monad (liftM, forM)
import System.FilePath
import System.Locale
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Maybe (catMaybes)
import Data.Time

exportTar :: [(String, IO [BackupEntry])] -> IO BS.ByteString
exportTar = fmap (compress . Tar.write) . toEntries

-- this is probably insufficiently lazy. use unsafeInterleaveIO to avoid loading /everything/ into memory
toEntries :: [(String, IO [BackupEntry])] -> IO [Tar.Entry]
toEntries featureMap = do
    baseDir <- mkBaseDir `fmap` getCurrentTime
    let exportEntries (name, ioEntries) = do
            entries <- ioEntries
            return $ flip map entries $ \(path, export) -> bsToEntry export (joinPath $ baseDir:name:path)
    unsafeInterleaveConcatMap exportEntries featureMap

type ExportEntry = ([FilePath], Either BS.ByteString BlobId)

readExportBlobs :: BlobStorage -> [ExportEntry] -> IO [BackupEntry]
readExportBlobs storage entries = forM entries $ \(path, export) ->
    case export of
        Left bs -> return (path, bs)
        Right blobId -> do 
            contents <- unsafeInterleaveIO $ Blob.fetch storage blobId
            return (path, contents)

-- | Convert a ByteString to a tar entry
bsToEntry :: BS.ByteString -> FilePath -> Tar.Entry
bsToEntry chunk path = case Tar.toTarPath False path of
    Right tarPath -> Tar.fileEntry tarPath chunk
    Left err -> error $ "Error in export: " ++ err

csvToBackup :: [String] -> CSV -> BackupEntry
csvToBackup fpath csv = (fpath, BS.pack (printCSV csv))

csvToExport :: [String] -> CSV -> ExportEntry
csvToExport fpath csv = (fpath, Left $ BS.pack (printCSV csv))

blobToExport :: [String] -> BlobId -> ExportEntry
blobToExport fpath blob = (fpath, Right blob)

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


testRoundtripByQuery :: (Show a, Eq a) => IO a -> TestRoundtrip
testRoundtripByQuery query = testRoundtripByQuery' query $ \_ -> return []

testRoundtripByQuery' :: (Show a, Eq a) => IO a -> (a -> IO [String]) -> TestRoundtrip
testRoundtripByQuery' query k = do
    old <- query
    return $ do
      new <- query
      if old == new
       then return ["Internal state mismatch:\n" ++ difference (show old) (show new)]
       else k new
  where
    difference old_str new_str
        -- = indent 2 old_str ++ "Versus:\n" ++ indent 2 new_str
        = indent 2 (take 80 old_str') ++ "\nVersus:\n" ++ indent 2 (take 80 new_str') ++ "\n(after " ++ show n_common ++ " chars)"
      where (n_common, old_str', new_str') = dropCommonPrefix (0 :: Int) old_str new_str

    indent n = unlines . map (replicate n ' ' ++) . lines
    
    dropCommonPrefix n (x:xs) (y:ys) | x == y = n `seq` dropCommonPrefix (n + 1) xs ys
    dropCommonPrefix n xs ys = (n, xs, ys)

testBlobsExist :: BlobStorage -> [Blob.BlobId] -> IO [String]
testBlobsExist store blobs
  = liftM catMaybes $ forM blobs $ \blob -> do
       (Blob.fetch store blob >> return Nothing) `catch`
         \e -> return $ Just $ "Could not open blob " ++ show blob ++ ": " ++ show (e :: IOError)
