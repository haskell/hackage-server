{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Security.Backup (
    securityBackup
  , securityRestore
  ) where

-- stdlib
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Version
import Text.CSV

-- hackage
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Features.Security.State

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

exportSecurityState :: SecurityState -> CSV
exportSecurityState SecurityState{..} = [
      [showVersion versionCSVVer]
    , versionCSVKey
    , ["timestamp", show securityTimestampVersion]
    , ["snapshot",  show securitySnapshotVersion]
    ]
  where
    versionCSVVer = Version [0,1] []
    versionCSVKey = [ "role"
                    , "file-version"
                    ]

{-------------------------------------------------------------------------------
  Restore
-------------------------------------------------------------------------------}

data PartialState = PartialSecurityState {
    partialTimestampVersion :: Maybe Sec.FileVersion
  , partialSnapshotVersion  :: Maybe Sec.FileVersion
  }

securityRestore :: Some Sec.Key -> Some Sec.Key -> RestoreBackup SecurityState
securityRestore securityTimestampKey
                securitySnapshotKey
                =
    aux PartialSecurityState {
        partialTimestampVersion = Nothing
      , partialSnapshotVersion  = Nothing
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
importSecurityState =
    \st -> (`execStateT` st) . mapM fromRecord . drop 2
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
