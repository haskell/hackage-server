-- | Security cache
{-# LANGUAGE RecordWildCards #-}
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
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

-- Hackage
import Distribution.Server.Framework
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Features.Security.State
import Distribution.Server.Features.Core

-- Hackage security
import qualified Hackage.Security.Server    as Sec
import qualified Hackage.Security.Util.Path as Sec
import qualified Data.Digest.Pure.SHA       as SHA

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
    let rootInfo    = fileInfo securityFileCacheRoot
        mirrorsInfo = fileInfo securityFileCacheMirrors
        tarGzInfo   = fileInfo $ indexTarballCompressed   indexTarballInfo
        tarInfo     = fileInfo $ indexTarballUncompressed indexTarballInfo
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
        sha256      = SHA.sha256 raw
    return $ Snapshot raw (fromIntegral (BS.Lazy.length raw)) sha256
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
                      , timestampInfoSnapshot = fileInfo snapshot
                      }
        signed    = Sec.withSignatures layout [timestampKey] timestamp
        raw       = Sec.renderJSON layout signed
    return $ Timestamp raw (fromIntegral (BS.Lazy.length raw))
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
updateSecurityFileCache env = do
    root <- do
      content <- Sec.readLazyByteString $ onDiskRoot env
      return Root {
          rootContent    = content
        , rootLength     = fromIntegral $ BS.Lazy.length content
        , rootHashSHA256 = SHA.sha256 content
        }

    mirrors <- do
      content <- Sec.readLazyByteString $ onDiskMirrors env
      return Mirrors {
          mirrorsContent    = content
        , mirrorsLength     = fromIntegral $ BS.Lazy.length content
        , mirrorsHashSHA256 = SHA.sha256 content
        }

    return SecurityFileCache {
        securityFileCacheRoot    = root
      , securityFileCacheMirrors = mirrors
      }
