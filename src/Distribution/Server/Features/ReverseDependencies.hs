{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.ReverseDependencies (
    ReverseFeature,
    reverseResource,
    ReverseResource(..),
    reverseUpdateHook,
    initReverseFeature,
    ReverseRender(..),
    ReversePageRender(..),
    revPackageId,
    revPackageName,
    renderReverseRecent,
    renderReverseOld,
    revPackageFlat,
    revPackageStats,
    revPackageSummary,
    revSummary,
    sortedRevSummary
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump (testRoundtripByQuery)
import Distribution.Server.Features.Core
import Distribution.Server.Features.PreferredVersions

import Distribution.Server.Packages.State
import Distribution.Server.Packages.Reverse
import Distribution.Server.Packages.Preferred
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Package
import Distribution.Text (display)
import Distribution.Version

import Data.List (mapAccumL, sortBy)
import Data.Maybe (catMaybes)
import Data.Function (fix, on)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (liftM, forever)
import Control.Monad.Trans (MonadIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

data ReverseFeature = ReverseFeature {
    reverseFeatureInterface :: HackageFeature,

    reverseResource :: ReverseResource,
    reverseUpdateHook :: Hook (Map PackageName [Version] -> IO ())
}

instance IsHackageFeature ReverseFeature where
    getFeatureInterface = reverseFeatureInterface


data ReverseResource = ReverseResource {
    reversePackage :: Resource,
    reversePackageOld :: Resource,
    reversePackageAll :: Resource,
    reversePackageStats :: Resource,
    reversePackages :: Resource,
    reversePackagesAll :: Resource,

    reverseUri :: String -> PackageId -> String,
    reverseNameUri :: String -> PackageName -> String,
    reverseOldUri :: String -> PackageId -> String,
    reverseOldNameUri :: String -> PackageName -> String,
    reverseAllUri :: String -> PackageName -> String,
    reverseStatsUri :: String -> PackageName -> String,
    reversesUri :: String -> String,
    reversesAllUri  :: String -> String
}


initReverseFeature :: ServerEnv
                   -> IO (CoreFeature
                       -> IO ReverseFeature)
initReverseFeature ServerEnv{serverVerbosity = verbosity} = do
    revChan <- newChan
    registerHook (packageAddHook core) $ \pkg -> writeChan revChan $
        update $ AddReversePackage (packageId pkg) (getAllDependencies pkg)
    registerHook (packageRemoveHook core) $ \pkg -> writeChan revChan $
        update $ RemoveReversePackage (packageId pkg) (getAllDependencies pkg)
    registerHook (packageChangeHook core) $ \pkg pkg' -> writeChan revChan $
        update $ ChangeReversePackage (packageId pkg)
                    (getAllDependencies pkg) (getAllDependencies pkg')

    revHook <- newHook
    let select (_, b, _) = b
        sortedRevs = fmap (sortBy $ on (flip compare) select) revSummary
    revTopCache <- Cache.newCacheable =<< sortedRevs
    registerHook revHook $ \_ -> Cache.putCache revTopCache =<< sortedRevs

    return $ \core -> do
      let feature = reverseFeature core
                       revChan revHook revTopCache

      return feature

reverseFeature :: CoreFeature
               -> Chan (IO (Map PackageName [Version]))
               -> Hook (Map PackageName [Version] -> IO ())
               -> Cache.Cache [(PackageName, Int, Int)]
               -> ReverseFeature

reverseFeature CoreFeature{..}
               reverseStream reverseUpdateHook reverseTopCache
  = ReverseFeature{..}
  where
    reverseFeatureInterface = (emptyHackageFeature "reverse") {
        featureResources = map ($reverseResource) []
      , featurePostInit = forkIO transferReverse >> return ()
      , featureDumpRestore = Just (return [], restoreBackup, testRoundtripByQuery (query GetReverseIndex))
      }

    transferReverse = forever $ do
            revFunc <- readChan reverseStream
            modded  <- revFunc
            runHook' reverseUpdateHook modded

    --TODO: this isn't a restore!
    --      do we need a post init/restore hook for initialising caches?
    restoreBackup = RestoreBackup
          { restoreEntry    = \_ -> return $ Right restoreBackup
          , restoreFinalize = return $ Right restoreBackup
          , restoreComplete = do
                putStrLn "Calculating reverse dependencies"
                index <- fmap packageList $ query GetPackagesState
                let revs = constructReverseIndex index
                update $ ReplaceReverseIndex revs
          }

    reverseResource = fix $ \r -> ReverseResource
          { reversePackage = resourceAt ("/package/:package/reverse.:format")
          , reversePackageOld = resourceAt ("/package/:package/reverse/old.:format")
          , reversePackageAll = resourceAt ("/package/:package/reverse/all.:format")
          , reversePackageStats = resourceAt ("/package/:package/reverse/summary.:format")
          , reversePackages = resourceAt ("/packages/reverse.:format")
          , reversePackagesAll = resourceAt ("/packages/reverse/all.:format")

          , reverseUri = \format pkg -> renderResource (reversePackage r) [display pkg, format]
          , reverseNameUri = \format pkg -> renderResource (reversePackage r) [display pkg, format]
          , reverseOldUri = \format pkg -> renderResource (reversePackageOld r) [display pkg, format]
          , reverseOldNameUri = \format pkg -> renderResource (reversePackageOld r) [display pkg, format]
          , reverseAllUri = \format pkg -> renderResource (reversePackageAll r) [display pkg, format]
          , reverseStatsUri = \format pkg -> renderResource (reversePackageStats r) [display pkg, format]
          , reversesUri = \format -> renderResource (reversePackages r) [format]
          , reversesAllUri = \format -> renderResource (reversePackagesAll r) [format]
          }

    --textRevDisplay :: ReverseDisplay -> String
    --textRevDisplay m = unlines . map (\(n, (v, m)) -> display n ++ "-" ++ display v ++ ": " ++ show m) . Map.toList $ m

-- If VersionStatus caching is used, revPackageId and revPackageName could be
-- reduced to a single map lookup (see Distribution.Server.Packages.Reverse).
revPackageId :: MonadIO m => PackageId -> m ReverseDisplay
revPackageId pkgid = do
    dispInfo <- revDisplayInfo
    revs <- liftM reverseDependencies $ query GetReverseIndex
    return $ perVersionReverse dispInfo revs pkgid

revPackageName :: MonadIO m => PackageName -> m ReverseDisplay
revPackageName pkgname = do
    dispInfo <- revDisplayInfo
    revs <- liftM reverseDependencies $ query GetReverseIndex
    return $ perPackageReverse dispInfo revs pkgname

revDisplayInfo :: MonadIO m => m VersionIndex
revDisplayInfo = do
    pkgIndex <- liftM packageList $ query GetPackagesState
    prefs <- query GetPreferredVersions
    return $ getDisplayInfo prefs pkgIndex

data ReverseRender = ReverseRender {
    rendRevPkg :: PackageId,
    rendRevStatus :: Maybe VersionStatus,
    rendRevCount :: Int
} deriving (Show, Eq, Ord)

data ReversePageRender = ReversePageRender {
    rendRevList :: [ReverseRender],
    rendFilterCount :: (Int, Int),
    rendPageTotal :: Int
}

renderReverseWith :: MonadIO m => PackageName -> ReverseDisplay -> (Maybe VersionStatus -> Bool) -> m ReversePageRender
renderReverseWith pkg rev filterFunc = do
    counts <- liftM reverseCount $ query GetReverseIndex
    let toRender (i, i') (pkgname, (version, status)) = case filterFunc status of
            False -> (,) (i, i'+1) Nothing
            True  -> (,) (i+1, i') $ Just $ ReverseRender {
                rendRevPkg = PackageIdentifier pkgname version,
                rendRevStatus = status,
                rendRevCount = maybe 0 directReverseCount $ Map.lookup pkgname counts
            }
        (res, rlist) = mapAccumL toRender (0, 0) (Map.toList rev)
        pkgCount = maybe 0 directReverseCount $ Map.lookup pkg counts
    return $ ReversePageRender (catMaybes rlist) res pkgCount

renderReverseRecent :: MonadIO m => PackageName -> ReverseDisplay -> m ReversePageRender
renderReverseRecent pkg rev = renderReverseWith pkg rev $ \status -> case status of
    Just DeprecatedVersion -> False
    Nothing -> False
    _ -> True

renderReverseOld :: MonadIO m => PackageName -> ReverseDisplay -> m ReversePageRender
renderReverseOld pkg rev = renderReverseWith pkg rev $ \status -> case status of
    Just DeprecatedVersion -> True
    Nothing -> True
    _ -> False

-- This could also differentiate between direct and indirect dependencies
-- with a bit more calculation.
revPackageFlat :: MonadIO m => PackageName -> m [(PackageName, Int)]
revPackageFlat pkgname = do
    index <- query GetReverseIndex
    let counts = reverseCount index
        count pkg = maybe 0 flattenedReverseCount $ Map.lookup pkg counts
        pkgs = maybe [] Set.toList $ Map.lookup pkgname $ flattenedReverse index
    return $ map (\pkg -> (pkg, count pkg)) pkgs

revPackageStats :: MonadIO m => PackageName -> m ReverseCount
revPackageStats = query . GetReverseCount

revPackageSummary :: MonadIO m => PackageId -> m (Int, Int)
revPackageSummary (PackageIdentifier pkgname version) = do
    ReverseCount direct _ versions <- revPackageStats pkgname
    return (direct, Map.findWithDefault 0 version versions)

-- This returns a list of (package name, direct dependencies, flat dependencies)
-- for all packages. An interesting fact: it even does so for packages which
-- don't exist in the index, except the latter two fields are always zero. This is
-- because no versions of these packages exist, so the union of no versions is
-- still no versions. TODO: use this fact to make an index of dependencies which
-- are not in Hackage at all, which might be useful for fixing accidentally
-- broken packages.
revSummary :: MonadIO m => m [(PackageName, Int, Int)]
revSummary = do
    counts <- liftM reverseCount $ query GetReverseIndex
    return $ map (\(pkg, ReverseCount direct flat _) -> (pkg, direct, flat)) $ Map.toList counts

sortedRevSummary :: MonadIO m => ReverseFeature -> m [(PackageName, Int, Int)]
sortedRevSummary revs = Cache.getCache $ reverseTopCache revs

