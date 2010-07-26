module Distribution.Server.Features.ReverseDependencies (
    ReverseFeature(..),
    ReverseResource(..),
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
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Types
import Distribution.Server.Backup.Import
import Distribution.Server.Features.Core
import Distribution.Server.Features.PreferredVersions

import Distribution.Server.Packages.State
import Distribution.Server.Packages.Reverse
import Distribution.Server.Packages.Preferred
--import Distribution.Server.PackageIndex (PackageIndex)

import Distribution.Package
import Distribution.Text (display)

import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (liftM, forever)
import Control.Monad.Trans (MonadIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

import Happstack.State (query, update)

data ReverseFeature = ReverseFeature {
    reverseResource :: ReverseResource,
    reverseStream :: Chan ReverseIndex
}

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

instance HackageFeature ReverseFeature where
    getFeature rev = HackageModule
      { featureName = "reverse"
      , resources   = map ($reverseResource rev) []
      , dumpBackup    = Nothing
      , restoreBackup = Just $ \_ -> fix $ \r -> RestoreBackup
              { restoreEntry    = \_ -> return $ Right r
              , restoreFinalize = return $ Right r
              , restoreComplete = do
                    putStrLn "Calculating reverse dependencies"
                    index <- fmap packageList $ query GetPackagesState
                    let revs = constructReverseIndex index
                    update $ ReplaceReverseIndex revs
              }
      }
    initHooks down = [forkIO transferReverse >> return ()]
      where transferReverse = forever $ do
                revs <- readChan (reverseStream down)
                update $ ReplaceReverseIndex revs

initReverseFeature :: Config -> CoreFeature -> VersionsFeature -> IO ReverseFeature
initReverseFeature _ core _ = do
    revChan <- newChan
    registerHook (packageAddHook core) $ \pkg -> do
        index <- fmap packageList $ query GetPackagesState
        revs  <- query GetReverseIndex
        writeChan revChan $ addPackage index pkg revs
    registerHook (packageRemoveHook core) $ \pkg -> do
        index <- fmap packageList $ query GetPackagesState
        revs  <- query GetReverseIndex
        writeChan revChan $ removePackage index pkg revs
    registerHook (packageChangeHook core) $ \pkg pkg' -> do
        index <- fmap packageList $ query GetPackagesState
        revs  <- query GetReverseIndex
        writeChan revChan $ addPackage index pkg' $ removePackage index pkg revs
    return ReverseFeature
      { reverseResource = fix $ \r -> ReverseResource
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
      , reverseStream = revChan
      }
  where
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

revSummary :: MonadIO m => m [(PackageName, Int, Int)]
revSummary = do
    counts <- liftM reverseCount $ query GetReverseIndex
    return $ map (\(pkg, ReverseCount direct flat _) -> (pkg, direct, flat)) $ Map.toList counts



