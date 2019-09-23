{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE BangPatterns         #-}
module Distribution.Server.Features.Security.State where

-- stdlib
import Control.Monad.Reader
import Data.Acid
import Data.Maybe
import Data.SafeCopy
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable
import qualified Control.Monad.State  as State
import qualified Data.ByteString.Lazy as BS.Lazy

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.ResponseContentTypes
import qualified Distribution.Server.Features.Security.MD5 as MD5
import qualified Distribution.Server.Features.Security.SHA256 as SHA

-- hackage-security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec


-- | TUF specific state
--
-- The security state as a whole uniquely determines the TUF timestamp and
-- snapshot files.
--
data SecurityState =
     SecurityState {
       -- | The files from disk (root, mirrors plus operational keys). This is
       -- optional, but if not present then security is not in use.
       securityStateFiles       :: !(Maybe SecurityStateFiles)
     , securityTarGzFileInfo    :: !FileInfo        -- ^ 01-index.tar.gz size and hashes
     , securityTarFileInfo      :: !FileInfo        -- ^ 01-index.tar    size and hashes
     , securitySnapshotVersion  :: !Sec.FileVersion -- ^ version of snapshot.json
     , securityTimestampVersion :: !Sec.FileVersion -- ^ version of timestamp.json
     , securityTimestampTime    :: !UTCTime  -- ^ when the snapshot.json and
                                             -- timestamp.json last got updated
    }
  deriving (Typeable, Show, Eq)

data SecurityStateFiles =
     SecurityStateFiles {
       securityRoot         :: !Root       -- ^ root.json content and hashes
     , securityMirrors      :: !Mirrors    -- ^ mirrors.json content and hashes
     , securitySnapshotKey  :: !(Some Sec.Key) -- ^ key to sign snapshot.json
     , securityTimestampKey :: !(Some Sec.Key) -- ^ key to sign timestamp.json

       -- These two are a cache, they can be calculated from the info above
     , securitySnapshot     ::  Snapshot       -- ^ content of snapshot.json
     , securityTimestamp    ::  Timestamp      -- ^ content of timestamp.json
     }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 2 'extension ''SecurityState
deriveSafeCopy 0 'base      ''SecurityStateFiles

instance MemSize SecurityState where
  memSize (SecurityState a b c d e f) = memSize6 a b c d e f

instance MemSize SecurityStateFiles where
  memSize (SecurityStateFiles a b c d e f) = memSize6 a b c d e f

initialSecurityState :: SecurityState
initialSecurityState =
    SecurityState {
      securityStateFiles       = Nothing,
      securityTarGzFileInfo    = tarGzFileInfo,
      securityTarFileInfo      = tarFileInfo,
      securitySnapshotVersion  = Sec.versionInitial,
      securityTimestampVersion = Sec.versionInitial,
      securityTimestampTime    = epoch1970
    }
  where
    emptyTar      = Tar.write []
    emptyTarGz    = GZip.compress emptyTar
    tarGzFileInfo = fileInfo (mkTarballCompressed   epoch1970 emptyTarGz)
    tarFileInfo   = fileInfo (mkTarballUncompressed epoch1970 emptyTar)
    epoch1970     = posixSecondsToUTCTime 0

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

getSecurityState :: Query SecurityState SecurityState
getSecurityState = ask

replaceSecurityState :: SecurityState -> Update SecurityState ()
replaceSecurityState = State.put

getSecurityFiles :: Query SecurityState (Maybe SecurityStateFiles)
getSecurityFiles = asks securityStateFiles


-- | This is used whenever any of the external files change
setRootMirrorsAndKeys :: Root -> Mirrors
                      -> Some Sec.Key -> Some Sec.Key
                      -> UTCTime
                      -> Update SecurityState ()
setRootMirrorsAndKeys root mirrors snapshotKey timestampKey now = do
    st <- State.get
    State.put st {
        securityStateFiles =
          Just SecurityStateFiles {
            securityRoot            = root,
            securityMirrors         = mirrors,
            securitySnapshotKey     = snapshotKey,
            securityTimestampKey    = timestampKey,
            -- these two get filled in by updateSnapshotAndTimestamp
            securitySnapshot        = error "snapshot cache not set",
            securityTimestamp       = error "timestamp cache not set"
          }
      }
    updateSnapshotAndTimestamp now


-- | This is used whenever the main index is updated
setTarGzFileInfo :: FileInfo -> FileInfo -> UTCTime
                 -> Update SecurityState ()
setTarGzFileInfo tarGzFileInfo tarFileInfo now = do
    st@SecurityState{..} <- State.get
    unless (securityTarGzFileInfo == tarGzFileInfo
         && securityTarFileInfo   == tarFileInfo) $ do
      State.put st {
        securityTarGzFileInfo = tarGzFileInfo,
        securityTarFileInfo   = tarFileInfo
      }
      updateSnapshotAndTimestamp now


-- | This is used by a cron job to resign the snapshot and timestamp if they
-- are too old.
resignSnapshotAndTimestamp :: Int -> UTCTime -> Update SecurityState ()
resignSnapshotAndTimestamp maxAge now = do
    st <- State.get
    let -- Is the existing update too told?
        tooOld = (now `diffUTCTime` securityTimestampTime st)
              >= fromIntegral maxAge
    when tooOld $
      -- note: if we care, it would be possible in this case, to resign just
      -- the timestamp and leave the snapshot as is. If so, think about the
      -- snapshot expiry.
      updateSnapshotAndTimestamp now


-- | Increment the snapshot and timestamp versions and resign them both.
updateSnapshotAndTimestamp :: UTCTime -> Update SecurityState ()
updateSnapshotAndTimestamp now = do
    st@SecurityState{..} <- State.get
    case securityStateFiles of
      Nothing -> return ()
      Just files@SecurityStateFiles{..} -> do
        let !snapshotVersion  = Sec.versionIncrement securitySnapshotVersion
            !timestampVersion = Sec.versionIncrement securityTimestampVersion
            (snapshot, timestamp) =
              signSnapshotAndTimestamp
                securityRoot          securityMirrors
                securityTarGzFileInfo securityTarFileInfo
                securitySnapshotKey   snapshotVersion
                securityTimestampKey  timestampVersion
                now
        State.put st {
          securityStateFiles       = Just files {
            securitySnapshot       = snapshot,
            securityTimestamp      = timestamp
          },
          securitySnapshotVersion  = snapshotVersion,
          securityTimestampVersion = timestampVersion,
          securityTimestampTime    = now
        }

-- | Construct the TUF snapshot and timestamp files
--
signSnapshotAndTimestamp :: Root -> Mirrors
                         -> FileInfo -> FileInfo
                         -> Some Sec.Key -> Sec.FileVersion
                         -> Some Sec.Key -> Sec.FileVersion
                         -> UTCTime
                         -> (Snapshot, Timestamp)
signSnapshotAndTimestamp root mirrors
                         tarGzFileInfo tarFileInfo
                         snapshotKey snapshotVersion
                         timestampKey timestampVersion
                         now =
    (ssFile, ttFile)
  where
    -- Construct new snapshot
    snapshot = Sec.Snapshot {
        Sec.snapshotVersion     = snapshotVersion
      , Sec.snapshotExpires     = Sec.expiresInDays now 3
      , Sec.snapshotInfoRoot    = toSecFileInfo (fileInfo root)
      , Sec.snapshotInfoMirrors = toSecFileInfo (fileInfo mirrors)
      , Sec.snapshotInfoTarGz   = toSecFileInfo tarGzFileInfo
      , Sec.snapshotInfoTar     = Just (toSecFileInfo tarFileInfo)
      }
    ssSigned = Sec.withSignatures layout [snapshotKey] snapshot
    ssRaw    = Sec.renderJSON layout ssSigned
    ssMD5    = MD5.md5     ssRaw
    ssSHA256 = SHA.sha256  ssRaw
    ssFile   = Snapshot TUFFile {
                 _tufFileContent    = ssRaw
               , _tufFileLength     = fromIntegral $ BS.Lazy.length ssRaw
               , _tufFileHashMD5    = ssMD5
               , _tufFileHashSHA256 = ssSHA256
               }

    -- Construct timestamp
    -- We don't actually use the SHA256 of the timestamp for anything; we
    -- compute it just for uniformity's sake.
    timestamp = Sec.Timestamp {
        timestampVersion      = timestampVersion
      , timestampExpires      = Sec.expiresInDays now 3
      , timestampInfoSnapshot = secFileInfo ssFile
      }
    ttSigned = Sec.withSignatures layout [timestampKey] timestamp
    ttRaw    = Sec.renderJSON layout ttSigned
    ttMD5    = MD5.md5     ttRaw
    ttSHA256 = SHA.sha256  ttRaw
    ttFile   = Timestamp TUFFile {
                 _tufFileContent    = ttRaw
               , _tufFileLength     = fromIntegral $ BS.Lazy.length ttRaw
               , _tufFileHashMD5    = ttMD5
               , _tufFileHashSHA256 = ttSHA256
               }

    layout = Sec.hackageRepoLayout

{-------------------------------------------------------------------------------
  Legacy transactions
-------------------------------------------------------------------------------}

-- | Only used by 'SecurityState_v1' and 'updateSecurityState'.
data TUFUpdate = TUFUpdate {
    tufUpdateTime        :: UTCTime
  , tufUpdateInfoRoot    :: FileInfo
  , tufUpdateInfoMirrors :: FileInfo
  , tufUpdateInfoTarGz   :: FileInfo
  , tufUpdateInfoTar     :: FileInfo
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''TUFUpdate

-- | Legacy transaction for compatibility with old transaction logs.
--
-- NOTE: We pass in the maximum age as an argument so that if we change the
-- policy then we can still accurately replay the old log.
updateSecurityState :: Int        -- ^ Maximum age of previous update in secs
                    -> TUFUpdate
                    -> Update SecurityState (Timestamp, Snapshot)
updateSecurityState maxAge newUpdate = do
    oldUpdate <- getTufUpdate
    when (needUpdate oldUpdate newUpdate) $ do
      setTufUpdate newUpdate
      updateSnapshotAndTimestamp (tufUpdateTime newUpdate)
    st <- State.get
    case st of
      SecurityState {
        securityStateFiles  = Just SecurityStateFiles {
                                securitySnapshot  = snapshot,
                                securityTimestamp = timestamp
                              }
      } -> return (timestamp, snapshot)
      _ -> error "updateSecurityState: unexpected state"
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

    -- These helpers mediate the changes in the SecurityState representation
    -- Both require that the securityStateFiles is Just, which is set up by
    -- the migration.
    getTufUpdate = do
      st <- State.get
      case st of
        SecurityState {
          securityTimestampTime = timestampTime,
          securityStateFiles    = Just SecurityStateFiles {
            securityRoot        = root,
            securityMirrors     = mirrors
          },
          securityTarGzFileInfo = tarGzFileInfo,
          securityTarFileInfo   = tarFileInfo
        } -> return $ Just TUFUpdate {
               tufUpdateInfoRoot    = fauxFileInfo root,
               tufUpdateInfoMirrors = fauxFileInfo mirrors,
               tufUpdateInfoTarGz   = tarGzFileInfo,
               tufUpdateInfoTar     = tarFileInfo,
               tufUpdateTime        = timestampTime
             }
        _ -> error "getTufUpdate: unexpected state"

    setTufUpdate TUFUpdate {
                   tufUpdateInfoRoot    = rootFileInfo,
                   tufUpdateInfoMirrors = mirrorsFileInfo,
                   tufUpdateInfoTarGz   = tarGzFileInfo,
                   tufUpdateInfoTar     = tarFileInfo,
                   tufUpdateTime        = timestampTime
                 } = do
      st <- State.get
      let files = case st of
                    SecurityState{ securityStateFiles = Just files' } -> files'
                    _                                                 -> error "setTufUpdate: unexpected state"
      State.put st {
        securityStateFiles =
          Just files {
            securityRoot      = Root    (fauxTUFFile rootFileInfo),
            securityMirrors   = Mirrors (fauxTUFFile mirrorsFileInfo)
          },
        securityTarGzFileInfo = tarGzFileInfo,
        securityTarFileInfo   = tarFileInfo,
        securityTimestampTime = timestampTime
      }

-- | This is just to support the old 'updateSecurityState' transaction when
-- using the new 'SecurityState' representation. The 'updateSecurityState'
-- compares 'FileInfo's to see if things changed, while the new 'SecurityState'
-- stores more than this (a full 'TUFFile'). So if we can stuff a 'FileInfo'
-- into a 'TUFFile' and get it back out again then we're ok.
--
-- That is, @fauxFileInfo . fauxTUFFile@ should be a round trip.
--
fauxTUFFile :: FileInfo -> TUFFile
fauxTUFFile  FileInfo {..} =
    TUFFile {
      _tufFileContent    = BS.Lazy.empty,
      _tufFileLength     = fileInfoLength,
      _tufFileHashSHA256 = fileInfoSHA256,
      _tufFileHashMD5    = fromMaybe (MD5.md5 BS.Lazy.empty)
                                     fileInfoMD5
    }

fauxFileInfo :: IsTUFFile file => file -> FileInfo
fauxFileInfo file =
    FileInfo {
      fileInfoLength = tufFileLength file,
      fileInfoSHA256 = tufFileHashSHA256 file,
      fileInfoMD5    = if tufFileHashMD5 file == MD5.md5 BS.Lazy.empty
                         then Nothing
                         else Just (tufFileHashMD5 file)
    }

{-------------------------------------------------------------------------------
  The acid-state transactions
-------------------------------------------------------------------------------}

makeAcidic ''SecurityState [
    'getSecurityState
  , 'replaceSecurityState
  , 'getSecurityFiles
  , 'updateSecurityState
  , 'setRootMirrorsAndKeys
  , 'setTarGzFileInfo
  , 'resignSnapshotAndTimestamp
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

instance Migrate SecurityState_v1 where
  type MigrateFrom SecurityState_v1 = SecurityState_v0
  migrate SecurityState_v0{..} = SecurityState_v1 {
      v1_securityTimestampKey     = v0_securityTimestampKey
    , v1_securitySnapshotKey      = v0_securitySnapshotKey
    , v1_securityTimestampVersion = v0_securityTimestampVersion
    , v1_securitySnapshotVersion  = v0_securitySnapshotVersion
    , v1_securityLastUpdate       = Nothing
    }

data SecurityState_v1 = SecurityState_v1 {
    v1_securityTimestampKey     :: Some Sec.Key
  , v1_securitySnapshotKey      :: Some Sec.Key
  , v1_securityTimestampVersion :: Sec.FileVersion
  , v1_securitySnapshotVersion  :: Sec.FileVersion
  , v1_securityLastUpdate       :: Maybe TUFUpdate
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 1 'extension ''SecurityState_v1

instance Migrate SecurityState where
  type MigrateFrom SecurityState = SecurityState_v1
  migrate SecurityState_v1 {
            v1_securitySnapshotKey      = snapshotKey,
            v1_securityTimestampKey     = timestampKey,
            v1_securitySnapshotVersion  = snapshotVersion,
            v1_securityTimestampVersion = timestampVersion,
            v1_securityLastUpdate       = Just TUFUpdate {
              tufUpdateTime        = timestampTime,
              tufUpdateInfoRoot    = rootFileInfo,
              tufUpdateInfoMirrors = mirrorsFileInfo,
              tufUpdateInfoTarGz   = tarGzFileInfo,
              tufUpdateInfoTar     = tarFileInfo
            }
          } =
    -- So note that we discard the snapshot and timestamp keys, and the
    -- file info for the root and mirrors. We will set those again later
    -- using a 'setRootMirrorsAndKeys' transaction.
    SecurityState {
      securityStateFiles        = Just SecurityStateFiles {
        securityRoot            = root,
        securityMirrors         = mirrors,
        securitySnapshotKey     = snapshotKey,
        securityTimestampKey    = timestampKey,
        securitySnapshot        = snapshot,
        securityTimestamp       = timestamp
      },
      securityTarGzFileInfo     = tarGzFileInfo,
      securityTarFileInfo       = tarFileInfo,
      securityTimestampTime     = timestampTime,
      securitySnapshotVersion   = snapshotVersion,
      securityTimestampVersion  = timestampVersion
    }
    where
      root    = Root    (fauxTUFFile rootFileInfo)
      mirrors = Mirrors (fauxTUFFile mirrorsFileInfo)
      (snapshot, timestamp) = signSnapshotAndTimestamp
                                root          mirrors
                                tarGzFileInfo tarFileInfo
                                snapshotKey   snapshotVersion
                                timestampKey  timestampVersion
                                timestampTime

  migrate SecurityState_v1 { v1_securityLastUpdate = Nothing } =
    -- In this case (which can only happen with an empty server)
    -- we just use the new style initial state
    initialSecurityState
