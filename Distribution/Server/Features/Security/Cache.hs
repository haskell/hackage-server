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
import qualified Distribution.Server.Features.Security.MD5 as MD5
import qualified Distribution.Server.Features.Security.SHA256 as SHA

-- Hackage security
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
    now                   <- getCurrentTime
    SecurityFileCache{..} <- readAsyncCache securityFileCache
    IndexTarballInfo{..}  <- queryGetIndexTarballInfo coreFeature

    let maxAge = 60 * 60 -- Don't update cache if unchanged and younger than 1hr
        tufUpdate = TUFUpdate{
            tufUpdateTime        = now
          , tufUpdateInfoRoot    = fileInfo securityFileCacheRoot
          , tufUpdateInfoMirrors = fileInfo securityFileCacheMirrors
          , tufUpdateInfoTarGz   = fileInfo indexTarballIncremGz
          , tufUpdateInfoTar     = fileInfo indexTarballIncremUn
          }

    (timestamp, snapshot) <- updateState securityState $
                               UpdateSecurityState maxAge tufUpdate

    return SecurityCache {
        securityCacheTimestamp = timestamp
      , securityCacheSnapshot  = snapshot
      , securityCacheRoot      = securityFileCacheRoot
      , securityCacheMirrors   = securityFileCacheMirrors
      }

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
      , _tufFileHashMD5    = MD5.md5     content
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
