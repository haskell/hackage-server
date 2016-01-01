{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
module Distribution.Server.Features.Security.State where

-- stdlib
import Control.Monad.Reader
import Data.Acid
import Data.SafeCopy
import Data.Time
import Data.Typeable
import qualified Control.Monad.State as State

-- hackage
import Distribution.Server.Framework.MemSize
import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.SHA256

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec

-- | Simplified form of the FileInfo used in hackage-security
data FileInfo = FileInfo {
    fileInfoLength :: Int
  , fileInfoSHA256 :: SHA256Digest
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''FileInfo

instance MemSize FileInfo where
  memSize FileInfo{..} =
    memSize2
      fileInfoLength
      fileInfoSHA256

-- | Input that determines the TUF files
--
-- From 'TUFUpdate' (along with the keys and versions numbers stored in the
-- security state) we can unique reconstruct the timestamp and snapshot files.
data TUFUpdate = TUFUpdate {
    tufUpdateTime        :: UTCTime
  , tufUpdateInfoRoot    :: FileInfo
  , tufUpdateInfoMirrors :: FileInfo
  , tufUpdateInfoTarGz   :: FileInfo
  , tufUpdateInfoTar     :: FileInfo
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''TUFUpdate

instance MemSize TUFUpdate where
  memSize TUFUpdate{..} =
    memSize5
      tufUpdateTime
      tufUpdateInfoRoot
      tufUpdateInfoMirrors
      tufUpdateInfoTarGz
      tufUpdateInfoTar

-- | TUF specific state
--
-- The security state as a whole uniquely determines the TUF timestamp and
-- snapshot files, provided that 'securityLastUpdate' is not 'Nothing'.
data SecurityState = SecurityState {
    securityTimestampKey     :: Some Sec.Key
  , securitySnapshotKey      :: Some Sec.Key
  , securityTimestampVersion :: Sec.FileVersion
  , securitySnapshotVersion  :: Sec.FileVersion
  , securityLastUpdate       :: Maybe TUFUpdate
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 1 'extension ''SecurityState

instance MemSize SecurityState where
  memSize SecurityState{..} =
    memSize5
      securityTimestampKey
      securitySnapshotKey
      securityTimestampVersion
      securitySnapshotVersion
      securityLastUpdate

initialSecurityState :: Some Sec.Key -- ^ Timestamp key
                     -> Some Sec.Key -- ^ Snapshot key
                     -> SecurityState
initialSecurityState timestampKey snapshotKey = SecurityState {
      securityTimestampKey     = timestampKey
    , securitySnapshotKey      = snapshotKey
    , securityTimestampVersion = Sec.versionInitial
    , securitySnapshotVersion  = Sec.versionInitial
    , securityLastUpdate       = Nothing
    }

getSecurityState :: Query SecurityState SecurityState
getSecurityState = ask

replaceSecurityState :: SecurityState -> Update SecurityState ()
replaceSecurityState = State.put

getTimestampKey :: Query SecurityState (Some Sec.Key)
getTimestampKey = asks securityTimestampKey

getSnapshotKey :: Query SecurityState (Some Sec.Key)
getSnapshotKey = asks securitySnapshotKey

nextTimestampVersion :: Update SecurityState Sec.FileVersion
nextTimestampVersion = do
    st <- State.get
    let fv = (securityTimestampVersion st)
    State.put $ st { securityTimestampVersion = Sec.versionIncrement fv }
    return fv

nextSnapshotVersion :: Update SecurityState Sec.FileVersion
nextSnapshotVersion = do
    st <- State.get
    let fv = securitySnapshotVersion st
    State.put $ st { securitySnapshotVersion = Sec.versionIncrement fv }
    return fv

makeAcidic ''SecurityState [
    'getSecurityState
  , 'replaceSecurityState
  , 'getSnapshotKey
  , 'getTimestampKey
  , 'nextSnapshotVersion
  , 'nextTimestampVersion
  ]

{-------------------------------------------------------------------------------
  Migration
-------------------------------------------------------------------------------}

-- | First version: we did not have securityTUFInput
data SecurityState_v0 = SecurityState_v0 {
    v0_securityTimestampKey     :: Some Sec.Key
  , v0_securitySnapshotKey      :: Some Sec.Key
  , v0_securityTimestampVersion :: Sec.FileVersion
  , v0_securitySnapshotVersion  :: Sec.FileVersion
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''SecurityState_v0

instance Migrate SecurityState where
  type MigrateFrom SecurityState = SecurityState_v0
  migrate SecurityState_v0{..} = SecurityState{
      securityTimestampKey     = v0_securityTimestampKey
    , securitySnapshotKey      = v0_securitySnapshotKey
    , securityTimestampVersion = v0_securityTimestampVersion
    , securitySnapshotVersion  = v0_securitySnapshotVersion
    , securityLastUpdate       = Nothing
    }
