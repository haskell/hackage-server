{-# LANGUAGE DoRec, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.RecentPackages (
    RecentPackagesFeature(..),
    RecentPackagesResource(..),
    initRecentPackagesFeature,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Features.PackageContents (PackageContentsFeature(..))

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Data.Time.Clock (getCurrentTime)
import Data.List (sortBy)
import Data.Ord (comparing)


-- the goal is to have the HTML modules import /this/ one, not the other way around
import qualified Distribution.Server.Pages.Recent as Pages

data RecentPackagesFeature = RecentPackagesFeature {
    recentPackagesFeatureInterface :: HackageFeature,
    recentPackagesResource :: RecentPackagesResource,

    -- necessary information for the representation of a package resource
    packageRender :: PkgInfo -> IO PackageRender
    -- other informational hooks: perhaps a simplified CondTree so a browser script can dynamically change the package page based on flags
}

instance IsHackageFeature RecentPackagesFeature where
    getFeatureInterface = recentPackagesFeatureInterface

data RecentPackagesResource = RecentPackagesResource {
    -- replace with log resource
    recentPackages :: Resource
}

initRecentPackagesFeature :: ServerEnv
                          -> UserFeature
                          -> CoreFeature
                          -> PackageContentsFeature
                          -> IO RecentPackagesFeature
initRecentPackagesFeature env@ServerEnv{serverCacheDelay, serverVerbosity = verbosity}
                          user
                          core@CoreFeature{packageChangeHook}
                          packageContents = do
    loginfo verbosity "Initialising recentPackages feature, start"

    -- recent caches. in lieu of an ActionLog
    -- TODO: perhaps a hook, recentUpdated :: HookList ([PkgInfo] -> IO ())
    rec let (feature, updateRecentCache) =
              recentPackagesFeature env user core packageContents
                                    cacheRecent

        cacheRecent <- newAsyncCacheNF updateRecentCache
                         defaultAsyncCachePolicy {
                           asyncCacheName = "recent uploads (html,rss)",
                           asyncCacheUpdateDelay  = serverCacheDelay,
                           asyncCacheSyncInit     = False,
                           asyncCacheLogVerbosity = verbosity
                         }

    registerHookJust packageChangeHook isPackageChangeAny $ \_ ->
      prodAsyncCache cacheRecent

    loginfo verbosity "Initialising recentPackages feature, end"
    return feature


recentPackagesFeature :: ServerEnv
                      -> UserFeature
                      -> CoreFeature
                      -> PackageContentsFeature
                      -> AsyncCache (Response, Response)
                      -> (RecentPackagesFeature, IO (Response, Response))

recentPackagesFeature env
                      UserFeature{..}
                      CoreFeature{..}
                      PackageContentsFeature{packageChangeLog}
                      cacheRecent
  = (RecentPackagesFeature{..}, updateRecentCache)
  where
    recentPackagesFeatureInterface = (emptyHackageFeature "recentPackages") {
        featureResources = map ($ recentPackagesResource) [recentPackages]
      , featureState     = []
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "recents packages page (html, rss)",
              getCacheMemSize = memSize <$> readAsyncCache cacheRecent
            }
          ]
      , featurePostInit = syncAsyncCache cacheRecent
      }

    recentPackagesResource = RecentPackagesResource {
        recentPackages = (extendResourcePath "/recent.:format" (corePackagesPage coreResource)) {
            resourceGet = [
                ("html", const $ liftM fst $ readAsyncCache cacheRecent)
              , ("rss",  const $ liftM snd $ readAsyncCache cacheRecent)
              ]
          }
      }

    packageRender pkg = do
      users <- queryGetUserDb
      changeLog <- packageChangeLog pkg
      let showChangeLogLink = case changeLog of Right _ -> True ; _ -> False
      doPackageRender users pkg showChangeLogLink

    updateRecentCache = do
        -- TODO: move the html version to the HTML feature
        pkgIndex <- queryGetPackageIndex
        users <- queryGetUserDb
        now   <- getCurrentTime
        let recentChanges = reverse $ sortBy (comparing pkgUploadTime) (PackageIndex.allPackages pkgIndex)
            xmlRepresentation = toResponse $ Resource.XHtml $ Pages.recentPage users recentChanges
            rssRepresentation = toResponse $ Pages.recentFeed users (serverBaseURI env) now recentChanges
        return (xmlRepresentation, rssRepresentation)

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




