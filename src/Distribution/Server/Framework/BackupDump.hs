{-
Create a tarball with the structured defined by each individual feature.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Framework.BackupDump (
    BackupType (..),
    dumpServerBackup,
    csvToBackup,
    blobToBackup,

    testBlobsExist
  ) where

import Text.CSV hiding (csv)

import Distribution.Server.Framework.BackupRestore (BackupEntry(..))
import qualified Distribution.Server.Framework.BlobStorage as Blob
import Distribution.Server.Framework.BlobStorage (BlobStorage, BlobId)
import Distribution.Server.Framework.Logging
import Distribution.Server.Util.Parse (packUTF8)

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip (compress)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Exception as Exception
import Control.Concurrent.MVar
import qualified Control.Concurrent.Async as Async

import Control.Monad (liftM, forM, unless)
import System.FilePath
import System.Directory
import System.Posix.Files as Posix (createLink)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time

data BackupType = FullBackup | ScrubbedBackup
                               deriving (Eq, Show)

-- Making backups is rather expensive in terms of disk I/O because we have
-- a lot of large static files (like package tarballs and docs).
--
-- So we use backup scheme where we share the static blob files between all
-- the backups. We use a tarball file per backup plus a shared directory of
-- blob files. We don't overwrite the blobs if they already exist. That way,
-- backups only pay the disk I/O cost of new blobs and of the other non-blob
-- data. we can end up writing as little as 10Mb rather than 20Gb.
--
-- The layout looks like this:
--
-- > backups/blobs/${blobid}
-- > backups/backup-2013-03-01.tar.gz
--
-- Then inside the tar.gz we have entries like:
--
-- > backup-2013-03-01/core/package/zlib-0.2/zlib.cabal
-- > backup-2013-03-01/core/package/zlib-0.2/zlib-0.2.tar.gz
-- >   -> ../../../../blobs/ecfdf802c16bab706b6985de57c73663
--
-- If you were to unpack the .tar.gz, that symlink would point to the file
--
-- > backups/blobs/ecfdf802c16bab706b6985de57c73663
--

dumpServerBackup :: Verbosity -> BackupType -> FilePath -> Maybe String
                 -> BlobStorage -> Bool
                 -> [(String, IO [BackupEntry])] -> IO ()
dumpServerBackup verbosity backupType backupDir tarfileTemplate
                 store linkBlobs backupActions = do

    now <- getCurrentTime
    let template    = fromMaybe defaultTarfileTemplate tarfileTemplate
        tarfileName = substTemplate template now
        tarfilePath = backupDir </> tarfileName <.> "tar.gz"
        blobDir     = backupDir </> "blobs"

    lognotice verbosity $ "Writing backup tarball " ++ tarfilePath ++ "\n"
                       ++ "with blob files in " ++ blobDir

    createDirectoryIfMissing False backupDir
    createDirectoryIfMissing False blobDir

    entriesChan <- newChan
    let writeEntry = writeChan entriesChan

    let writeTarEntries = do
          entries <- getChanContents entriesChan
          BS.writeFile tarfilePath (GZip.compress (Tar.write entries))

    Async.withAsync writeTarEntries $ \tarWriter -> do
      Async.link tarWriter

      processEntries verbosity store tarfileName blobDir linkBlobs
                               writeEntry backupActions
      closeChan entriesChan

      Async.wait tarWriter
    lognotice verbosity "Backup complete"

  where
    leadin = if backupType == ScrubbedBackup then "scrubbed-" else ""
    defaultTarfileTemplate = leadin ++ "backup-" ++ "%Y-%m-%d"
    substTemplate = formatTime defaultTimeLocale

    -- MVar as 1-place chan
    newChan      = newEmptyMVar
    writeChan ch = putMVar ch . Just
    closeChan ch = putMVar ch Nothing
    getChanContents :: MVar (Maybe a) -> IO [a]
    getChanContents ch = unsafeInterleaveIO $ do
        mx <- takeMVar ch
        case mx of
          Nothing -> return []
          Just x  -> do xs <- getChanContents ch
                        return (x:xs)

processEntries :: Verbosity
               -> BlobStorage -> FilePath -> FilePath -> Bool
               -> (Tar.Entry -> IO ())
               -> [(String, IO [BackupEntry])]
               -> IO ()
processEntries verbosity store tarfileName blobDir linkBlobs writeEntry featureMap =
    sequence_
      [ do loginfo verbosity $ "Exporting " ++ featureName ++ " feature state"
           entries <- ioEntries
           sequence_
             [ processEntry verbosity store tarfileName blobDir linkBlobs featureName
                            writeEntry entry
             | entry <- entries ]
           loginfo verbosity $ "Export for " ++ featureName ++ " complete"
      | (featureName, ioEntries) <- featureMap ]


processEntry :: Verbosity -> BlobStorage -> FilePath -> FilePath -> Bool -> String
             -> (Tar.Entry -> IO ())
             -> BackupEntry
             -> IO ()
processEntry _verbosity _store tarfileName _blobDir _linkBlobs featureName writeEntry
  (BackupByteString path bs) = do

  tarPath <- either exportError return $
             Tar.toTarPath False (mkTarPath tarfileName featureName path)
  writeEntry (Tar.fileEntry tarPath bs)

processEntry verbosity store tarfileName blobDir linkBlobs featureName writeEntry
    (BackupBlob path blobId) = do

    blobExists <- doesFileExist (Blob.filepath store blobId)
    if blobExists
      then do
        let blobName = Blob.blobMd5 blobId
            blobPath = blobDir </> blobName
            -- go back to the root of the tarball. 'path' here would be something like
            -- a/b/c/filename, which we store in
            -- backup-2013-03-01/feature/a/b/c/filename; in this example, we want
            -- to go back 4 directories, which is @length path + 1@
            -- (because 'path' includes the filename)
            relRoot  = joinPath (replicate (length path + 1) "..")
            blobLink = relRoot </> "blobs" </> blobName

        entryPath   <- either exportError return $
                       Tar.toTarPath False (mkTarPath tarfileName featureName path)
        linkTarget  <- maybe (fail "Could not generate link target") return $
                       Tar.toLinkTarget blobLink

        writeBlob blobPath blobId
        writeEntry (Tar.simpleEntry entryPath (Tar.SymbolicLink linkTarget))
      else
        lognotice verbosity $ "Warning: skipping non-existent blob " ++ show blobId ++ " (" ++ foldr1 (</>) path ++ ")"
  where
    -- blobPath is the path in the backup, not the original
    writeBlob blobPath blobid = do
      exists <- doesFileExist blobPath
      unless exists $
        if linkBlobs
          then Posix.createLink (Blob.filepath store blobid) blobPath
          else Blob.fetch store blobid >>= BS.writeFile blobPath

exportError :: String -> IO a
exportError err = fail $ "Error in export: " ++ err

mkTarPath :: FilePath -> String -> [FilePath] -> FilePath
mkTarPath tarfileName featureName ps = joinPath (tarfileName : featureName : ps)

csvToBackup :: [String] -> CSV -> BackupEntry
csvToBackup fpath csv = BackupByteString fpath $ packUTF8 (printCSV csv)

blobToBackup :: [String] -> BlobId -> BackupEntry
blobToBackup = BackupBlob

testBlobsExist :: BlobStorage -> [Blob.BlobId] -> IO [String]
testBlobsExist store blobs
  = liftM catMaybes $ forM blobs $ \blob -> do
       (Blob.fetch store blob >> return Nothing) `Exception.catch`
         \e -> return $ Just $ "Could not open blob " ++ show blob ++ ": " ++ show (e :: IOError)
