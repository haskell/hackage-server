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

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.Time.Clock (getCurrentTime)
import Data.List (sortBy, sortOn)
import Data.Ord (comparing, Down (Down))

-- the goal is to have the HTML modules import /this/ one, not the other way around
import qualified Distribution.Server.Pages.Recent as Pages

data RecentPackagesFeature = RecentPackagesFeature {
    recentPackagesFeatureInterface :: HackageFeature,
    recentPackagesResource :: RecentPackagesResource

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

          cacheRecent <- newAsyncCacheNF updateRecentCache
                           defaultAsyncCachePolicy {
                             asyncCacheName = "recent uploads and revisions (html,rss,html,rss)",
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
                      -> AsyncCache (Response, Response)
                      -> (RecentPackagesFeature, IO (Response, Response))

recentPackagesFeature env
                      UserFeature{..}
                      CoreFeature{..}
                      cacheRecent
  = (RecentPackagesFeature{..}, updateRecentCache)
  where
    recentPackagesFeatureInterface = (emptyHackageFeature "recentPackages") {
        featureResources = map ($ recentPackagesResource) [recentPackages, recentRevisions]
      , featureState     = []
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "recent packages and revisions page (rss, rss)",
              getCacheMemSize = memSize <$> readAsyncCache cacheRecent
            }
          ]
      , featurePostInit = syncAsyncCache cacheRecent
      }

    recentPackagesResource = RecentPackagesResource {
        recentPackages = (extendResourcePath "/recent.:format" (corePackagesPage coreResource)) {
            resourceGet = [
                ("rss",  const $ addAllowOriginHeader >> (fmap fst  . readAsyncCache $ cacheRecent))
              ]
          },
        recentRevisions = (extendResourcePath "/recent/revisions.:format" (corePackagesPage coreResource)) {
            resourceGet = [
                ("rss",  const $ addAllowOriginHeader >> (fmap snd . readAsyncCache $ cacheRecent))
              ]
          }
      }

    updateRecentCache :: IO (Response, Response)
    updateRecentCache = do
        pkgIndex <- queryGetPackageIndex
        users <- queryGetUserDb
        now   <- liftIO getCurrentTime
        

        let packages = PackageIndex.allPackages pkgIndex
            isRevised pkgInfo = pkgNumRevisions pkgInfo > 1
            recentChanges = sortOn (Down . pkgOriginalUploadTime) packages
            rssRepresentation = toResponse $ Pages.recentFeed users (serverBaseURI env) now recentChanges
            recentRevisions = sortOn (Down . revisionTime) . filter isRevised $ packages
            revisionTime pkgInfo = pkgLatestUploadTime pkgInfo
            rssRevisions = toResponse $ Pages.recentRevisionsFeed users (serverBaseURI env) now recentRevisions

        return (rssRepresentation, rssRevisions)


addAllowOriginHeader :: (FilterMonad Response m) => m ()
addAllowOriginHeader = addHeaderM "Access-Control-Allow-Origin" "*"

{-
data SimpleCondTree = SimpleCondNode [Dependency] [(Condition ConfVar, SimpleCondTree, SimpleCondTree)]
                    | SimpleCondLeaf
    deriving (Show, Eq)

doMakeCondTree :: GenericPackageDescription -> [(String, SimpleCondTree)]
doMakeCondTree desc = map (\lib -> ("library", makeCondTree lib)) (maybeToList $ condLibrary desc)
                   ++ map (\(exec, tree) -> (exec, makeCondTree tree)) (condExecutables desc)
  where
    makeCondTree (CondNode _ deps comps) = case deps of
        [] -> SimpleCondLeaf
        _  -> SimpleCondNode deps $ map makeCondComponents comps
    makeCondComponents (cond, tree, mtree) = (cond, makeCondTree tree, maybe SimpleCondLeaf makeCondTree mtree)
-}
