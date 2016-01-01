-- | Security cache
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Server.Features.Security.Cache (
    -- * Main cache
    SecurityCache(..)
  , updateSecurityCache
    -- * Auxiliary file cache
  , SecurityFileCache
  , updateSecurityFileCache
  ) where

-- Standard libraries
import Control.DeepSeq
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import System.IO (IOMode(..))
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types (EpochTime)
import qualified Crypto.Classes             as Crypto
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

-- Hackage
import Distribution.Server.Framework
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Features.Security.State
import Distribution.Server.Features.Core
import qualified Distribution.Server.Features.Security.SHA256 as SHA

-- Hackage security
import qualified Hackage.Security.Server    as Sec
import qualified Hackage.Security.Util.Path as Sec

{-------------------------------------------------------------------------------
  The security cache

  It is important that we always update the following together (atomically):

  - The timestamp
  - The snapshot
  - The root metadata
  - The mirrors metadata

  A change in the root metadata or the mirrors metadata causes a change in the
  snapshot, and a change in the snapshot causes a change in the timestamp;
  updating one without the other would mean the client sees an inconsistent
  state.

  Hence, we all keep these together in the single cache. Note that it _is_
  ok to update the index without updating the TUF data, because the index
  is append only.

  TODO: Compute length, MD5 and SHA256 hashes simultenously when updating
  any of these TUF files.
-------------------------------------------------------------------------------}

data SecurityCache = SecurityCache {
      securityCacheTimestamp :: !Timestamp
    , securityCacheSnapshot  :: !Snapshot
    , securityCacheRoot      :: !Root
    , securityCacheMirrors   :: !Mirrors
    }

instance NFData SecurityCache where
  rnf (SecurityCache a b c d) = rnf (a, b, c, d)

updateSecurityCache :: StateComponent AcidState SecurityState
                    -> AsyncCache SecurityFileCache
                    -> CoreFeature
                    -> IO SecurityCache
updateSecurityCache securityState securityFileCache coreFeature = do
    now       <- getCurrentTime
    files     <- readAsyncCache securityFileCache
    snapshot  <- computeSnapshot  securityState now coreFeature files
    timestamp <- computeTimestamp securityState now snapshot

    return SecurityCache {
        securityCacheTimestamp = timestamp
      , securityCacheSnapshot  = snapshot
      , securityCacheRoot      = securityFileCacheRoot    files
      , securityCacheMirrors   = securityFileCacheMirrors files
      }

-- | Compute new snapshot
computeSnapshot :: StateComponent AcidState SecurityState
                -> UTCTime
                -> CoreFeature
                -> SecurityFileCache
                -> IO Snapshot
computeSnapshot securityState now coreFeature SecurityFileCache{..} = do
    indexTarballInfo <- queryGetIndexTarballInfo coreFeature
    snapshotVersion  <- updateState securityState NextSnapshotVersion
    snapshotKey      <- queryState  securityState GetSnapshotKey
    let rootInfo    = secFileInfo securityFileCacheRoot
        mirrorsInfo = secFileInfo securityFileCacheMirrors
        tarGzInfo   = secFileInfo $ indexTarballIncremGz indexTarballInfo
        tarInfo     = secFileInfo $ indexTarballIncremUn indexTarballInfo
        snapshot    = Sec.Snapshot {
                          Sec.snapshotVersion     = snapshotVersion
                        , Sec.snapshotExpires     = Sec.expiresInDays now 3
                        , Sec.snapshotInfoRoot    = rootInfo
                        , Sec.snapshotInfoMirrors = mirrorsInfo
                        , Sec.snapshotInfoTarGz   = tarGzInfo
                        , Sec.snapshotInfoTar     = Just tarInfo
                        }
        signed      = Sec.withSignatures layout [snapshotKey] snapshot
        raw         = Sec.renderJSON layout signed
        md5         = Crypto.hash raw
        sha256      = SHA.sha256  raw
    return $ Snapshot TUFFile {
        _tufFileContent    = raw
      , _tufFileLength     = fromIntegral $ BS.Lazy.length raw
      , _tufFileHashMD5    = md5
      , _tufFileHashSHA256 = sha256
      , _tufFileModified   = now
      }
  where
    layout = Sec.hackageRepoLayout

-- | Compute new timestamp
computeTimestamp :: StateComponent AcidState SecurityState
                 -> UTCTime
                 -> Snapshot
                 -> IO Timestamp
computeTimestamp securityState now snapshot = do
    timestampVersion <- updateState securityState NextTimestampVersion
    timestampKey     <- queryState  securityState GetTimestampKey
    let timestamp = Sec.Timestamp {
                        timestampVersion      = timestampVersion
                      , timestampExpires      = Sec.expiresInDays now 3
                      , timestampInfoSnapshot = secFileInfo snapshot
                      }
        signed    = Sec.withSignatures layout [timestampKey] timestamp
        raw       = Sec.renderJSON layout signed
        md5       = Crypto.hash raw
        sha256    = SHA.sha256  raw
    -- We don't actually use the SHA256 of the timestamp for anything; we
    -- compute it just for uniformity's sake.
    return $ Timestamp TUFFile {
        _tufFileContent    = raw
      , _tufFileLength     = fromIntegral $ BS.Lazy.length raw
      , _tufFileHashMD5    = md5
      , _tufFileHashSHA256 = sha256
      , _tufFileModified   = now
      }
  where
    layout = Sec.hackageRepoLayout

{-------------------------------------------------------------------------------
  The security file cache

  We want to avoid re-reading the root and mirrors files every time the
  security cache is prodded; at the same time we cannot have the root and
  mirrors files in a different cache from the cache containing the snapshot
  for reasons explained above. Hence, we cache the root and mirrors files
  in a separate cache but, crucially, we do not serve from this cache. Instead
  whenever we prod the main cache we just copy the cached root and mirrors files
  from this secondary cache to the main cache. We prod the file cache only
  when the user specifically requests it.
-------------------------------------------------------------------------------}

data SecurityFileCache = SecurityFileCache {
    securityFileCacheRoot    :: Root
  , securityFileCacheMirrors :: Mirrors
  }

instance NFData SecurityFileCache where
  rnf (SecurityFileCache a b) = rnf (a, b)

updateSecurityFileCache :: ServerEnv -> IO SecurityFileCache
updateSecurityFileCache env =
    SecurityFileCache <$> (Root    <$> getTUFFile (onDiskRoot    env))
                      <*> (Mirrors <$> getTUFFile (onDiskMirrors env))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getTUFFile :: Sec.Path Sec.Absolute -> IO TUFFile
getTUFFile file = do
    (content, status) <- getFile file
    return $ TUFFile {
        _tufFileContent    = content
      , _tufFileLength     = fromIntegral $ BS.Lazy.length content
      , _tufFileHashMD5    = Crypto.hash content
      , _tufFileHashSHA256 = SHA.sha256  content
      , _tufFileModified   = lastModified status
      }
  where
    lastModified :: FileStatus -> UTCTime
    lastModified = convertTime . modificationTime

    convertTime :: EpochTime -> UTCTime
    convertTime = posixSecondsToUTCTime . realToFrac

getFile :: Sec.Path Sec.Absolute -> IO (BS.Lazy.ByteString, FileStatus)
getFile file = Sec.withFile file ReadMode $ \h -> do
    -- It's a bit of a dance to get the file modification time while keeping
    -- the handle open. If we first call 'hGetContents' it will close the
    -- handle and the call to handleToFd will fail. So we must do that first,
    -- fully read the file before leaving the scope of 'withFile'.
    fd      <- handleToFd  h
    status  <- getFdStatus fd
    h'      <- fdToHandle  fd
    content <- BS.Lazy.hGetContents h'
    evaluate $ rnf content
    return (content, status)
