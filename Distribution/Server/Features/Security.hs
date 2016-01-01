{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
-- | TUF security features
module Distribution.Server.Features.Security (
    initSecurityFeature
  ) where

-- Standard libraries
import Control.Exception

-- Hackage
import Distribution.Server.Features.Core
import Distribution.Server.Features.Security.Backup
import Distribution.Server.Features.Security.Cache
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Features.Security.State
import Distribution.Server.Framework
import Distribution.Server.Packages.Index
import Distribution.Server.Packages.Types

-- Hackage security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server      as Sec
import qualified Hackage.Security.Util.Path   as Sec

data SecurityFeature = SecurityFeature {
      securityFeatureInterface :: HackageFeature
    }

instance IsHackageFeature SecurityFeature where
  getFeatureInterface SecurityFeature{..} = securityFeatureInterface

initSecurityFeature :: ServerEnv -> IO (CoreFeature -> IO SecurityFeature)
initSecurityFeature env = do
    securityState <- securityStateComponent env (serverStateDir env)
    return $ \coreFeature -> do
       -- Initialize caches
       securityFileCache <- newAsyncCacheNF
         (updateSecurityFileCache env)
         defaultAsyncCachePolicy {
             asyncCacheName         = "TUF file cache"
           , asyncCacheUpdateDelay  = serverCacheDelay env
           , asyncCacheSyncInit     = False
           , asyncCacheLogVerbosity = serverVerbosity env
           }
       securityCache <- newAsyncCacheNF
         (updateSecurityCache securityState securityFileCache coreFeature)
         defaultAsyncCachePolicy {
             asyncCacheName         = "TUF main cache"
           , asyncCacheUpdateDelay  = serverCacheDelay env
           , asyncCacheSyncInit     = False
           , asyncCacheLogVerbosity = serverVerbosity env
           }

       -- Update security cache whenever index or file cache changes
       registerHook (asyncCacheUpdatedHook securityFileCache) $ \_ ->
         prodAsyncCache securityCache "files changed"
       registerHook (indexUpdatedHook coreFeature) $ \_ ->
         prodAsyncCache securityCache "index updated"

       -- Add package metadata whenever a package is added/changed
       --
       -- For package changes we just add a new metadata file to the index,
       -- which will override any previous one.
       --
       -- TODO: We cannot deal with deletes (they are a problem elsewhere too)
       registerHook (preIndexUpdateHook coreFeature) $ \chg -> return $
         case chg of
           PackageChangeAdd    pkg   -> indexEntriesFor pkg
           PackageChangeInfo _ new   -> indexEntriesFor new
           PackageChangeDelete _     -> []
           PackageChangeIndexExtra{} -> []

       return $ securityFeature env
                                securityState
                                securityFileCache
                                securityCache
  where
    indexEntriesFor :: PkgInfo -> [TarIndexEntry]
    indexEntriesFor pkgInfo =
      case pkgLatestTarball pkgInfo of
        Nothing -> []
        Just (_tarball, (uploadTime, _uploadUserId), latestRev) ->
          [MetadataEntry (pkgInfoId pkgInfo) latestRev uploadTime]

-- | The main security feature
--
-- Missing resources (for Phase 2 of the security work):
--
-- * Top-level targets.json (currently top-level targets.json is not
--   required because it's hardcoded in the clients)
-- * Other targets.json files for OOT targets
--
-- Note that even once we have author signing, per-package targets.json file
-- do not get their own resource, but are instead recorded in the tarball.
securityFeature :: ServerEnv
                -> StateComponent AcidState SecurityState
                -> AsyncCache SecurityFileCache
                -> AsyncCache SecurityCache
                -> SecurityFeature
securityFeature env securityState securityFileCache securityCache =
    SecurityFeature{..}
  where
    securityFeatureInterface = (emptyHackageFeature "security") {
        featureDesc        = "TUF Security"
      , featureState       = [abstractAcidStateComponent securityState]
      , featureReloadFiles = prodAsyncCache securityFileCache "reload"
      , featurePostInit    = setupResignCronJob env securityCache
      , featureResources   = [
            securityTimestamp
          , securitySnapshot
          , securityRoot
          , securityMirrors
          ]
      }

    securityTimestamp = (secResourceAt Sec.repoLayoutTimestamp) {
        resourceDesc = [(GET, "Get TUF timestamp")]
      , resourceGet  = [("json", serveFromCache securityCacheTimestamp)]
      }
    securitySnapshot = (secResourceAt Sec.repoLayoutSnapshot) {
        resourceDesc = [(GET, "Get TUF snapshot")]
      , resourceGet  = [("json", serveFromCache securityCacheSnapshot)]
      }
    securityRoot = (secResourceAt Sec.repoLayoutRoot) {
        resourceDesc = [(GET, "Get TUF root")]
      , resourceGet  = [("json", serveFromCache securityCacheRoot)]
      }
    securityMirrors = (secResourceAt Sec.repoLayoutMirrors) {
        resourceDesc = [(GET, "Get TUF mirrors")]
      , resourceGet  = [("json", serveFromCache securityCacheMirrors )]
      }

    serveFromCache :: (IsTUFFile a, ToMessage a)
                   => (SecurityCache -> a)
                   -> DynamicPath
                   -> ServerPartE Response
    serveFromCache file _ = do
      tufFile <- file <$> readAsyncCache securityCache
      let eTag = ETag $ show (tufFileHashMD5 tufFile)
      -- Higher max-age values result in higher cache hit ratios, but also
      -- in higher likelyhood of cache incoherence problems (and of course in
      -- higher likelyhood of caches beind out of date with updates to the
      -- central server).
      cacheControl [Public, NoTransform, maxAgeMinutes 1] eTag
      enableRange
      enableGZip' 3
      return $ toResponse tufFile

securityStateComponent :: ServerEnv
                       -> FilePath
                       -> IO (StateComponent AcidState SecurityState)
securityStateComponent env stateDir = do
    timestampKey <- readKey $ onDiskTimestampKey env
    snapshotKey  <- readKey $ onDiskSnapshotKey  env
    let stateFile = stateDir </> "db" </> "TUF"
    st <- logTiming (serverVerbosity env) "Loaded SecurityState" $
            openLocalStateFrom stateFile $
              initialSecurityState timestampKey snapshotKey
    return StateComponent {
        stateDesc    = "TUF specific state"
      , stateHandle  = st
      , getState     = query st GetSecurityState
      , putState     = update st . ReplaceSecurityState
      , resetState   = securityStateComponent env
      , backupState  = \_ -> securityBackup
      , restoreState = securityRestore timestampKey snapshotKey
      }
  where
    readKey :: Sec.Path Sec.Absolute -> IO (Some Sec.Key)
    readKey fp = do
      mKey <- Sec.readJSON_NoKeys_NoLayout fp
      case mKey of
        Left  err -> throwIO err
        Right key -> return key

setupResignCronJob :: ServerEnv -> AsyncCache SecurityCache -> IO ()
setupResignCronJob env securityCache =
    addCronJob (serverCron env) CronJob {
        cronJobName      = "Resign TUF data"
      , cronJobFrequency = DailyJobFrequency
      , cronJobOneShot   = False
      , cronJobAction    = prodAsyncCache securityCache "cron"
      }
