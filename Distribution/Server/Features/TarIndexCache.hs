{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
-- | The tar index cache provides generic support for caching a tarball's
-- TarIndex; this is used by various other modules.
module Distribution.Server.Features.TarIndexCache (
    TarIndexCacheFeature(..)
  , initTarIndexCacheFeature
  ) where

import Control.Exception (throwIO)
import Data.Serialize (runGetLazy, runPutLazy)
import Data.SafeCopy (safeGet, safePut)

import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Features.TarIndexCache.State
import Distribution.Server.Packages.Types (PkgTarball(..))
import Data.TarIndex
import Distribution.Server.Util.ServeTarball (constructTarIndex)

data TarIndexCacheFeature = TarIndexCacheFeature {
    tarIndexCacheFeatureInterface :: HackageFeature
  , cachedTarIndex        :: BlobId -> IO TarIndex
  , cachedPackageTarIndex :: PkgTarball -> IO TarIndex
  }

instance IsHackageFeature TarIndexCacheFeature where
  getFeatureInterface = tarIndexCacheFeatureInterface

initTarIndexCacheFeature :: ServerEnv -> IO TarIndexCacheFeature
initTarIndexCacheFeature env@ServerEnv{serverStateDir} = do
  tarIndexCache <- tarIndexCacheStateComponent serverStateDir
  return $ tarIndexCacheFeature env tarIndexCache

tarIndexCacheStateComponent :: FilePath -> IO (StateComponent TarIndexCache)
tarIndexCacheStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "TarIndexCache") initialTarIndexCache
  return StateComponent {
      stateDesc    = "Mapping from tarball blob IDs to tarindex blob IDs"
    , acidState    = st
    , getState     = query st GetTarIndexCache
    , putState     = update st . ReplaceTarIndexCache
    , resetState   = tarIndexCacheStateComponent
    -- We don't backup the tar indices, but reconstruct them on demand
    , backupState  = \_ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "The impossible happened"
                       , restoreFinalize = return initialTarIndexCache
                       }
    }

tarIndexCacheFeature :: ServerEnv
                     -> StateComponent TarIndexCache
                     -> TarIndexCacheFeature
tarIndexCacheFeature ServerEnv{serverBlobStore = store} tarIndexCache =
   TarIndexCacheFeature{..}
  where
    tarIndexCacheFeatureInterface :: HackageFeature
    tarIndexCacheFeatureInterface = (emptyHackageFeature "tarIndexCache") {
        featureDesc  = "Generic cache for tarball indices"
        -- We don't want to compare blob IDs
        -- (TODO: We could potentially check that if a package occurs in both
        -- packages then both caches point to identical tar indices, but for
        -- that we would need to be in IO)
      , featureState = [abstractStateComponent' (\_ _ -> []) tarIndexCache]
      }

    -- This is the heart of this feature
    cachedTarIndex :: BlobId -> IO TarIndex
    cachedTarIndex tarBallBlobId = do
      mTarIndexBlobId <- queryState tarIndexCache (FindTarIndex tarBallBlobId)
      case mTarIndexBlobId of
        Just tarIndexBlobId -> do
          serializedTarIndex <- fetch store tarIndexBlobId
          case runGetLazy safeGet serializedTarIndex of
            Left  err      -> throwIO (userError err)
            Right tarIndex -> return tarIndex
        Nothing -> do
          tarBall        <- fetch store tarBallBlobId
          tarIndex       <- case constructTarIndex tarBall of
                              Left  err      -> throwIO (userError err)
                              Right tarIndex -> return tarIndex
          tarIndexBlobId <- add store (runPutLazy (safePut tarIndex))
          updateState tarIndexCache (SetTarIndex tarBallBlobId tarIndexBlobId)
          return tarIndex

    cachedPackageTarIndex :: PkgTarball -> IO TarIndex
    cachedPackageTarIndex = cachedTarIndex . pkgTarballNoGz
