{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Security.Backup (
    securityBackup
  , securityRestore
  ) where

-- stdlib
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Version (Version(..), showVersion)
import Text.CSV hiding (csv)

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.State
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec

{-------------------------------------------------------------------------------
  Backup
-------------------------------------------------------------------------------}

-- | Backup the security state
--
-- We do not backup the actual keys; we re-load those from disk.
securityBackup :: SecurityState -> [BackupEntry]
securityBackup = (:[]) . csvToBackup ["partialstate.csv"] . exportSecurityState

-- | Export the security state to CSV
--
-- Version 0.2 introduced more information into this format:
--
-- > "0.2"
-- > "version" , "timestamp" , <timestamp version>
-- > "version" , "snapshot"  , <snapshot version>
-- > "update"  , "time"      , <time of last update>
-- > "update"  , "info"      , "root"    , <file length> , <SHA256> [, <MD5> ]
-- > "update"  , "info"      , "mirrors" , <file length> , <SHA256> [, <MD5> ]
-- > "update"  , "info"      , "tarGz"   , <file length> , <SHA256> [, <MD5> ]
-- > "update"  , "info"      , "tar"     , <file length> , <SHA256> [, <MD5> ]
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
    ] ++ case securityLastUpdate of
           Nothing -> []
           Just TUFUpdate{..} -> [
               ["update", "time", formatUTCTime tufUpdateTime]
             , fileInfoCSV "root"    tufUpdateInfoRoot
             , fileInfoCSV "mirrors" tufUpdateInfoMirrors
             , fileInfoCSV "tarGz"   tufUpdateInfoTarGz
             , fileInfoCSV "tar"     tufUpdateInfoTar
             ]
  where
    versionCSVVer = Version [0,2] []

    fileInfoCSV file FileInfo{..} =
      ["update", "info", file, show fileInfoLength, show fileInfoSHA256] ++
      [ show x | Just x <- [fileInfoMD5] ]

{-------------------------------------------------------------------------------
  Restore
-------------------------------------------------------------------------------}

data PartialState = PartialSecurityState {
    partialTimestampVersion  :: Maybe Sec.FileVersion
  , partialSnapshotVersion   :: Maybe Sec.FileVersion
  , partialUpdateTime        :: Maybe UTCTime
  , partialUpdateInfoRoot    :: Maybe FileInfo
  , partialUpdateInfoMirrors :: Maybe FileInfo
  , partialUpdateInfoTarGz   :: Maybe FileInfo
  , partialUpdateInfoTar     :: Maybe FileInfo
  }

securityRestore :: Some Sec.Key -> Some Sec.Key -> RestoreBackup SecurityState
securityRestore securityTimestampKey
                securitySnapshotKey
                =
    aux PartialSecurityState {
        partialTimestampVersion  = Nothing
      , partialSnapshotVersion   = Nothing
      , partialUpdateTime        = Nothing
      , partialUpdateInfoRoot    = Nothing
      , partialUpdateInfoMirrors = Nothing
      , partialUpdateInfoTarGz   = Nothing
      , partialUpdateInfoTar     = Nothing
      }
  where
    aux :: PartialState -> RestoreBackup SecurityState
    aux st = RestoreBackup {
        restoreEntry    = restore st
      , restoreFinalize = finalize st
      }

    finalize :: PartialState -> Restore SecurityState
    finalize PartialSecurityState{..} =
      return SecurityState{
          securityTimestampVersion = fromMaybe reset partialTimestampVersion
        , securitySnapshotVersion  = fromMaybe reset partialSnapshotVersion
        , securityLastUpdate       = do
            -- We can construct the last update only if we have all fields
            -- Otherwise, we have to return 'Nothing'
            tufUpdateTime        <- partialUpdateTime
            tufUpdateInfoRoot    <- partialUpdateInfoRoot
            tufUpdateInfoMirrors <- partialUpdateInfoMirrors
            tufUpdateInfoTarGz   <- partialUpdateInfoTarGz
            tufUpdateInfoTar     <- partialUpdateInfoTar
            return TUFUpdate{..}
        , ..
        }

    -- If file versions are missing for certain roles, we might be importing
    -- from an old backup. In this case, we have no choice but to start
    -- counting from 1.
    reset :: Sec.FileVersion
    reset = Sec.FileVersion 1

    restore :: PartialState -> BackupEntry -> Restore (RestoreBackup SecurityState)
    restore st (BackupByteString ["partialstate.csv"] bs) = do
      st' <- importCSV "partialstate.csv" bs >>= importSecurityState st
      return $ aux st'
    restore st _otherEntry =
      return $ aux st

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
      modify $ \st -> st { partialUpdateTime = Just updateTime }
    fromRecord ("update":"info":"root":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialUpdateInfoRoot = Just info }
    fromRecord ("update":"info":"mirrors":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialUpdateInfoMirrors = Just info }
    fromRecord ("update":"info":"tarGz":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialUpdateInfoTarGz = Just info }
    fromRecord ("update":"info":"tar":sec) = do
      info <- fromInfoRecord sec
      modify $ \st -> st { partialUpdateInfoTar = Just info }
    fromRecord otherRecord =
      fail $ "Unexpected record: " ++ show otherRecord

    fromInfoRecord :: Monad m => Record -> m FileInfo
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
