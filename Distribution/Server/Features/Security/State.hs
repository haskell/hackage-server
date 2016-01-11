{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
module Distribution.Server.Features.Security.State where

-- stdlib
import Control.Monad.Reader
import Data.Acid
import Data.Maybe (fromJust)
import Data.SafeCopy
import Data.Time
import Data.Typeable
import qualified Control.Monad.State  as State
import qualified Crypto.Classes       as Crypto
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Framework.MemSize
import qualified Distribution.Server.Features.Security.SHA256 as SHA

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec

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

-- | Construct the TUF files
--
-- Returns 'Nothing' if the 'securityLastUpdate' field is 'Nothing'.
--
-- TODO: Compute length, MD5 and SHA256 hashes simultenously when updating
-- any of these TUF files.
constructTUFFiles :: Query SecurityState (Maybe (Timestamp, Snapshot))
constructTUFFiles = do
    mUpdate <- asks securityLastUpdate
    case mUpdate of
      Nothing -> return Nothing
      Just TUFUpdate{..} -> do
        snapshotVersion  <- asks securitySnapshotVersion
        snapshotKey      <- asks securitySnapshotKey
        timestampVersion <- asks securityTimestampVersion
        timestampKey     <- asks securityTimestampKey

        -- Construct new snapshot
        let snapshot = Sec.Snapshot {
                Sec.snapshotVersion     = snapshotVersion
              , Sec.snapshotExpires     = Sec.expiresInDays tufUpdateTime 3
              , Sec.snapshotInfoRoot    = toSecFileInfo tufUpdateInfoRoot
              , Sec.snapshotInfoMirrors = toSecFileInfo tufUpdateInfoMirrors
              , Sec.snapshotInfoTarGz   = toSecFileInfo tufUpdateInfoTarGz
              , Sec.snapshotInfoTar     = Just $ toSecFileInfo tufUpdateInfoTar
              }
            ssSigned = Sec.withSignatures layout [snapshotKey] snapshot
            ssRaw    = Sec.renderJSON layout ssSigned
            ssMD5    = Crypto.hash ssRaw
            ssSHA256 = SHA.sha256  ssRaw
            ssFile   = Snapshot TUFFile {
                _tufFileContent    = ssRaw
              , _tufFileLength     = fromIntegral $ BS.Lazy.length ssRaw
              , _tufFileHashMD5    = ssMD5
              , _tufFileHashSHA256 = ssSHA256
              , _tufFileModified   = tufUpdateTime
              }

        -- Construct timestamp
        -- We don't actually use the SHA256 of the timestamp for anything; we
        -- compute it just for uniformity's sake.
        let timestamp   = Sec.Timestamp {
                timestampVersion      = timestampVersion
              , timestampExpires      = Sec.expiresInDays tufUpdateTime 3
              , timestampInfoSnapshot = secFileInfo ssFile
              }
            ttSigned    = Sec.withSignatures layout [timestampKey] timestamp
            ttRaw       = Sec.renderJSON layout ttSigned
            ttMD5       = Crypto.hash ttRaw
            ttSHA256    = SHA.sha256  ttRaw
            ttFile      = Timestamp TUFFile {
                _tufFileContent    = ttRaw
              , _tufFileLength     = fromIntegral $ BS.Lazy.length ttRaw
              , _tufFileHashMD5    = ttMD5
              , _tufFileHashSHA256 = ttSHA256
              , _tufFileModified   = tufUpdateTime
              }

        return $ Just (ttFile, ssFile)
  where
    layout = Sec.hackageRepoLayout

-- | Update the security state
--
-- If the update is the same as the last one stored, and the current one is not
-- yet sufficiently old, then we ignore this update and return the old state.
--
-- NOTE: We pass in the maximum age as an argument so that if we change the
-- policy then we can still accurately replay the old log.
updateSecurityState :: Int        -- ^ Maximum age of previous update in secs
                    -> TUFUpdate
                    -> Update SecurityState (Timestamp, Snapshot)
updateSecurityState maxAge newUpdate = do
    oldUpdate <- State.gets securityLastUpdate
    when (needUpdate oldUpdate newUpdate) $ do
      _newTimestampVersion <- nextTimestampVersion
      _newSnapshotVersion  <- nextSnapshotVersion
      State.modify $ \st -> st { securityLastUpdate = Just newUpdate }
    fromJust `liftM` liftQuery constructTUFFiles
  where
    needUpdate :: Maybe TUFUpdate -> TUFUpdate -> Bool
    needUpdate Nothing    _   = True
    needUpdate (Just old) new = changed old new || tooOld old new

    -- Did the files change (ignoring time)?
    changed :: TUFUpdate -> TUFUpdate -> Bool
    changed old new = old /= new{tufUpdateTime = tufUpdateTime old}

    -- Is the existing update too told?
    tooOld :: TUFUpdate -> TUFUpdate -> Bool
    tooOld old new = (tufUpdateTime new `diffUTCTime` tufUpdateTime old)
                  >= (fromIntegral maxAge)

makeAcidic ''SecurityState [
    'getSecurityState
  , 'replaceSecurityState
  , 'getSnapshotKey
  , 'getTimestampKey
  , 'nextSnapshotVersion
  , 'nextTimestampVersion
  , 'constructTUFFiles
  , 'updateSecurityState
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
