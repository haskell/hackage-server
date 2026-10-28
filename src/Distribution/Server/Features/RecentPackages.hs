{-# LANGUAGE RecursiveDo, RankNTypes, NamedFieldPuns, RecordWildCards, FlexibleContexts #-}
module Distribution.Server.Features.RecentPackages (
    RecentPackagesFeature(..),
    RecentPackagesResource(..),
    initRecentPackagesFeature,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Packages.Types

data RecentPackagesFeature = RecentPackagesFeature {
    recentPackagesFeatureInterface :: HackageFeature,
    getRecentPackages :: forall m. MonadIO m => m [PkgInfo],
    getRecentRevisions :: forall m. MonadIO m => m [PkgInfo]

    -- other informational hooks: perhaps a simplified CondTree so a browser script can dynamically change the package page based on flags
}

instance IsHackageFeature RecentPackagesFeature where
    getFeatureInterface = recentPackagesFeatureInterface

data RecentPackagesResource = RecentPackagesResource {
    -- replace with log resource
    recentPackages :: Resource,
    recentRevisions :: Resource
}

initRecentPackagesFeature :: ServerEnv
                          -> IO (UserFeature
                              -> CoreFeature
                              -> IO RecentPackagesFeature)
initRecentPackagesFeature env@ServerEnv{serverCacheDelay, serverVerbosity = verbosity} = do
    return $ \user core@CoreFeature{packageChangeHook} -> do

      -- recent caches. in lieu of an ActionLog
      -- TODO: perhaps a hook, recentUpdated :: HookList ([PkgInfo] -> IO ())
      rec let (feature, updateRecentCache) =
                recentPackagesFeature env user core
                                      cacheRecent

          cacheRecent <- newAsyncCacheWHNF updateRecentCache
                           defaultAsyncCachePolicy {
                             asyncCacheName = "recent uploads and revisions",
                             asyncCacheUpdateDelay  = serverCacheDelay,
                             asyncCacheSyncInit     = False,
                             asyncCacheLogVerbosity = verbosity
                           }

      registerHookJust packageChangeHook isPackageChangeAny $ \_ ->
        prodAsyncCache cacheRecent "package change"

      return feature


recentPackagesFeature :: ServerEnv
                      -> UserFeature
                      -> CoreFeature
                      -> AsyncCache RecentPackageVersions
                      -> (RecentPackagesFeature, IO RecentPackageVersions)

recentPackagesFeature _
                      UserFeature{..}
                      CoreFeature{..}
                      cacheRecent
  = (RecentPackagesFeature{..}, updateRecentCache)
  where
    recentPackagesFeatureInterface = (emptyHackageFeature "recentPackages") {
      featureState     = [],
      featureCaches    = [
            CacheComponent {
              cacheDesc       = "recent packages and revisions",
              getCacheMemSize = memSize <$> readAsyncCache cacheRecent
            }
          ]
      }


    getRecentPackages :: MonadIO m => m [PkgInfo]
    getRecentPackages = recentPackageVersionsUploads <$> readAsyncCache cacheRecent
    
    getRecentRevisions :: MonadIO m => m [PkgInfo]
    getRecentRevisions = recentPackageVersionsRevisions <$> readAsyncCache cacheRecent

    updateRecentCache :: IO RecentPackageVersions
    updateRecentCache = queryRecentPackageVersions