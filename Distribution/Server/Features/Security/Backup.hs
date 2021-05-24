{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Distribution.Server.Features.Security.Backup (
    securityBackup
  , securityRestore
  ) where

-- stdlib
import Control.Monad.State
import Data.Time
import Data.Version (Version(..), showVersion)
import Text.CSV hiding (csv)
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.State
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec
import qualified Hackage.Security.Util.Pretty as Sec

{-------------------------------------------------------------------------------
  Backup
-------------------------------------------------------------------------------}

-- | Backup the security state
--
-- We do not backup the actual keys; we re-load those from disk.
securityBackup :: SecurityState -> [BackupEntry]
securityBackup st =
    [ csvToBackup ["partialstate.csv"] (exportSecurityState st) ]
 ++ case securityStateFiles st of
      Nothing -> []
      Just SecurityStateFiles{..} ->
        [ BackupByteString ["root.json"]    (tufFileContent securityRoot)
        , BackupByteString ["mirrors.json"] (tufFileContent securityMirrors)
        , BackupByteString ["snapshot.private"]  (showKey securitySnapshotKey)
        , BackupByteString ["timestamp.private"] (showKey securityTimestampKey)
        ]
  where
    showKey = Sec.renderJSON_NoLayout

-- | Export the security state to CSV
--
-- Version 0.2 introduced more information into this format:
--
-- > "0.2"
-- > "version" , "timestamp" , <timestamp version>
-- > "version" , "snapshot"  , <snapshot version>
-- > "update"  , "time"      , <time of last update>
-- > "update"  , "info"      , "tarGz"   , <file length> , <SHA256> [, <MD5> ]
-- > "update"  , "info"      , "tar"     , <file length> , <SHA256> [, <MD5> ]
--
-- Version 0.2 originally also included the following two entries, but we no
-- longer use these. For compatibility They're accepted but ignored.
--
-- > "update"  , "info"      , "root"    , <file length> , <SHA256> [, <MD5> ]
-- > "update"  , "info"      , "mirrors" , <file length> , <SHA256> [, <MD5> ]
--
-- Version 0.1 was prior to the introduction of 'TUFUpdate' and looked like:
--
-- > "0.1"
-- > "role"      , "file-version"
-- > "timestamp" , <timestamp version>
-- > "snapshot"  , <snapshot version>
exportSecurityState :: SecurityState -> CSV
exportSecurityState SecurityState{..} = [
      [showVersion versionCSVVer]
    , ["version", "timestamp", show securityTimestampVersion]
    , ["version", "snapshot",  show securitySnapshotVersion]
    ] ++ case securityStateFiles of
           Nothing -> []
           Just SecurityStateFiles{..} -> [
               ["update", "time", formatUTCTime securityTimestampTime]
               , fileInfoCSV "tarGz"   securityTarGzFileInfo
               , fileInfoCSV "tar"     securityTarFileInfo
               ]
  where
    versionCSVVer = Version [0,2] []

    fileInfoCSV file FileInfo{..} =
      ["update", "info", file, show fileInfoLength, show fileInfoSHA256] ++
      [ show x | Just x <- [fileInfoMD5] ]

{-------------------------------------------------------------------------------
  Restore
-------------------------------------------------------------------------------}

data PartialState = PartialState {
    partialRoot              :: Maybe Root
  , partialMirrors           :: Maybe Mirrors
  , partialSnapshotKey       :: Maybe (Some Sec.Key)
  , partialTimestampKey      :: Maybe (Some Sec.Key)
  , partialTarGzFileInfo     :: Maybe FileInfo
  , partialTarFileInfo       :: Maybe FileInfo
  , partialTimestampVersion  :: Maybe Sec.FileVersion
  , partialSnapshotVersion   :: Maybe Sec.FileVersion
  , partialTimestampTime     :: Maybe UTCTime
  }

emptyPartialState :: PartialState
emptyPartialState = PartialState Nothing Nothing Nothing Nothing
                                 Nothing Nothing Nothing Nothing
                                 Nothing

securityRestore :: RestoreBackup SecurityState
securityRestore =
    aux emptyPartialState
  where
    aux :: PartialState -> RestoreBackup SecurityState
    aux st = RestoreBackup {
        restoreEntry    = restore st
      , restoreFinalize = finalize st
      }

    finalize :: PartialState -> Restore SecurityState
    finalize PartialState {
               partialRoot,
               partialMirrors,
               partialSnapshotKey,
               partialTimestampKey,
               partialTarGzFileInfo    = Just tarGzFileInfo,
               partialTarFileInfo      = Just tarFileInfo,
               partialSnapshotVersion  = Just snapshotVersion,
               partialTimestampVersion = Just timestampVersion,
               partialTimestampTime    = Just timestampTime
             } =
      return SecurityState {
        securityStateFiles = do
          root         <- partialRoot
          mirrors      <- partialMirrors
          snapshotKey  <- partialSnapshotKey
          timestampKey <- partialTimestampKey
          let (snapshot, timestamp) = signSnapshotAndTimestamp
                                        root          mirrors
                                        tarGzFileInfo tarFileInfo
                                        snapshotKey   snapshotVersion
                                        timestampKey  timestampVersion
                                        timestampTime
          return SecurityStateFiles {
            securityRoot            = root,
            securityMirrors         = mirrors,
            securitySnapshotKey     = snapshotKey,
            securityTimestampKey    = timestampKey,
            securitySnapshot        = snapshot,
            securityTimestamp       = timestamp
          },
        securityTarGzFileInfo    = tarGzFileInfo,
        securityTarFileInfo      = tarFileInfo,
        securitySnapshotVersion  = snapshotVersion,
        securityTimestampVersion = timestampVersion,
        securityTimestampTime    = timestampTime
      }
    finalize _ = fail "missing required security state entries"

    restore :: PartialState -> BackupEntry -> Restore (RestoreBackup SecurityState)
    restore st (BackupByteString ["partialstate.csv"] bs) = do
      st' <- importCSV "partialstate.csv" bs >>= importSecurityState st
      return $ aux st'
    restore st (BackupByteString ["root.json"] bs) =
      return $ aux st { partialRoot = Just (Root (mkTUFFile bs)) }
    restore st (BackupByteString ["mirrors.json"] bs) =
      return $ aux st { partialMirrors = Just (Mirrors (mkTUFFile bs)) }
    restore st (BackupByteString ["snapshot.private"] bs) = do
      snapshotKey <- importKey "snapshot.private" bs
      return $ aux st { partialSnapshotKey = Just snapshotKey }
    restore st (BackupByteString ["timestamp.private"] bs) = do
      timestampKey <- importKey "timestamp.private" bs
      return $ aux st { partialTimestampKey = Just timestampKey }
    restore st _otherEntry =
      return $ aux st

importKey :: FilePath -> BS.Lazy.ByteString -> Restore (Some Sec.Key)
importKey keyfile content =
    case Sec.parseJSON_NoKeys_NoLayout content of
      Left  err -> fail $ keyfile ++ ": " ++ Sec.pretty err
      Right key -> return key

importSecurityState :: PartialState -> CSV -> Restore PartialState
importSecurityState st ([version]:csv) =
    byVersion =<< parseVersion "CSV version header" version
  where
    byVersion :: Version -> Restore PartialState
    byVersion (Version [0,1] []) = execStateT (import_v0 csv) st
    byVersion (Version [0,2] []) = execStateT (import_v1 csv) st
    byVersion _otherversion = fail "Unsupported version"
importSecurityState _ _ = fail "Unrecognized CSV format"

import_v1 :: CSV -> StateT PartialState Restore ()
import_v1 = mapM_ fromRecord
  where
    fromRecord :: Record -> StateT PartialState Restore ()
    fromRecord ["version", "timestamp", strTimestampVersion] = do
      version <- parseRead "timestamp version" strTimestampVersion
      modify $ \st -> st { partialTimestampVersion = Just version }
    fromRecord ["version", "snapshot", strSnapshotVersion] = do
      version <- parseRead "snapshot version" strSnapshotVersion
      modify $ \st -> st { partialSnapshotVersion = Just version }
    fromRecord ["update", "time", strUpdateTime] = do
      updateTime <- parseUTCTime "last update time" strUpdateTime
      modify $ \st -> st { partialTimestampTime = Just updateTime }
    fromRecord ("update":"info":"root":_)    = return ()
    fromRecord ("update":"info":"mirrors":_) = return ()
    fromRecord ("update":"info":"tarGz":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialTarGzFileInfo = Just info }
    fromRecord ("update":"info":"tar":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialTarFileInfo = Just info }
    fromRecord otherRecord =
      fail $ "Unexpected record: " ++ show otherRecord

    fromInfoRecord :: (Monad m, MonadFail m) => Record -> m FileInfo
    fromInfoRecord [strFileLength, strSHA256] = do
      fileInfoLength <- parseRead "file length" strFileLength
      fileInfoSHA256 <- parseSHA "file SHA256" strSHA256
      let fileInfoMD5 = Nothing
      return FileInfo{..}
    fromInfoRecord [strFileLength, strSHA256, strMD5] = do
      fileInfoLength <- parseRead "file length" strFileLength
      fileInfoSHA256 <- parseSHA "file SHA256" strSHA256
      fileInfoMD5    <- Just `liftM` parseMD5  "file MD5" strMD5
      return FileInfo{..}
    fromInfoRecord otherRecord =
      fail $ "Unexpected info record: " ++ show otherRecord

import_v0 :: CSV -> StateT PartialState Restore ()
import_v0 = mapM_ fromRecord . drop 1
  where
    fromRecord :: Record -> StateT PartialState Restore ()
    fromRecord ["timestamp", strTimestampVersion] = do
      version <- parseRead "timestamp version" strTimestampVersion
      modify $ \st -> st { partialTimestampVersion = Just version }
    fromRecord ["snapshot", strSnapshotVersion] = do
      version <- parseRead "snapshot version" strSnapshotVersion
      modify $ \st -> st { partialSnapshotVersion = Just version }
    fromRecord otherRecord =
      fail $ "Unexpected record: " ++ show otherRecord
