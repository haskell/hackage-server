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
import Data.Typeable
import qualified Control.Monad.State as State

-- hackage
import Distribution.Server.Framework.MemSize
import Distribution.Server.Features.Security.Orphans ()

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec

-- | TUF specific state
--
-- We store the timestamp and snapshot key as part of the server state so that
-- the files can be removed from the serverTUFDir after the server has been
-- initialized, if desired.
data SecurityState = SecurityState {
    securityTimestampKey     :: Some Sec.Key
  , securitySnapshotKey      :: Some Sec.Key
  , securityTimestampVersion :: Sec.FileVersion
  , securitySnapshotVersion  :: Sec.FileVersion
  }
  deriving (Typeable, Show, Eq)

instance MemSize SecurityState where
  memSize SecurityState{..} = sum [
      memSize securityTimestampKey
    , memSize securitySnapshotKey
    , memSize securityTimestampVersion
    , memSize securitySnapshotVersion
    ]

deriveSafeCopy 0 'base ''SecurityState

initialSecurityState :: Some Sec.Key -- ^ Timestamp key
                     -> Some Sec.Key -- ^ Snapshot key
                     -> SecurityState
initialSecurityState timestampKey snapshotKey = SecurityState {
      securityTimestampKey     = timestampKey
    , securitySnapshotKey      = snapshotKey
    , securityTimestampVersion = Sec.versionInitial
    , securitySnapshotVersion  = Sec.versionInitial
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
