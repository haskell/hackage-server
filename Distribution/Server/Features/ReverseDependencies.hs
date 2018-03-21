{-# LANGUAGE DeriveGeneric, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.ReverseDependencies (
    ReverseFeature(..),
    ReverseCount(..),
    ReverseResource(..),
    initReverseFeature,
    ReverseRender(..),
    ReversePageRender(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Packages.PackageIndex (packageNames, allPackagesByName)
import Distribution.Server.Packages.Types (pkgInfoId)

import Distribution.Server.Features.ReverseDependencies.State

import Distribution.Package
import Distribution.Text (display)

import Data.List (mapAccumL, sortBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Function (fix)
import qualified Data.Bimap as Bimap
import qualified Data.Array as Arr
import qualified Data.Graph as Gr
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (liftM, forever)
import Control.Monad.Trans (MonadIO)
import Data.Aeson
import GHC.Generics hiding (packageName)
import Data.ByteString.Lazy (ByteString)

data ReverseFeature = ReverseFeature {
    reverseFeatureInterface :: HackageFeature,

    reverseResource :: ReverseResource,

    reverseHook :: Hook [PackageId] (),

    queryReverseIndex :: forall m. MonadIO m => m ReverseIndex,
    queryReverseDeps :: forall m. MonadIO m => PackageName -> m ([PackageName], [PackageName]),
    revPackageId :: forall m. MonadIO m => PackageId -> m ReverseDisplay,
    revPackageName :: forall m. MonadIO m => PackageName -> m ReverseDisplay,
    renderReverseRecent :: forall m. (Functor m, MonadIO m) => PackageName -> ReverseDisplay -> m ReversePageRender,
    renderReverseOld :: forall m. (Functor m, MonadIO m) => PackageName -> ReverseDisplay -> m ReversePageRender,
    revPackageFlat :: forall m. (Functor m, MonadIO m) => PackageName -> m [(PackageName, Int)],
    revPackageStats :: forall m. (Functor m, MonadIO m) => PackageName -> m ReverseCount,
    revPackageSummary :: forall m. (Functor m, MonadIO m) => PackageId -> m (Int, Int),
    revSummary :: forall m. (Functor m, MonadIO m) => m [(PackageName, Int, Int)],
    revJSON :: forall m. MonadIO m => m ByteString
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
                       -> VersionsFeature
                       -> IO ReverseFeature)
initReverseFeature _ = do
    updateReverse <- newHook

    return $ \core@CoreFeature{..}
             versionsf@VersionsFeature{..} -> do
      idx <- queryGetPackageIndex
      memState <- newMemStateWHNF (constructReverseIndex idx)

      let feature = reverseFeature core versionsf memState updateReverse

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
            Nothing -> return () --PackageRemoveHook
            Just pkginfo -> do
                index <- queryGetPackageIndex
                modifyMemState memState (addPackage pkgid (getAllDependencies pkginfo index))
                runHook_ updateReverse [pkgid]

      return feature

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

-- data Node = Node {id::Int, label::String} deriving Generic
data Edge = Edge {
    id::Int,
    name::String,
    deps::[String]
    } deriving Generic
-- data JGraph = JGraph { nodes::[Node], edges::[Edge]} deriving Generic
-- instance ToJSON Node
instance ToJSON Edge
-- instance ToJSON JGraph
-- instance ToJSON PackageName

reverseFeature :: CoreFeature
               -> VersionsFeature
               -> MemState ReverseIndex
               -> Hook [PackageId] ()
               -> ReverseFeature

reverseFeature CoreFeature{..}
               VersionsFeature{..}
               reverseMemState
               reverseHook
  = ReverseFeature{..}
  where
    reverseFeatureInterface = (emptyHackageFeature "reverse") {
        featureResources = map ($ reverseResource) []
      , featurePostInit  = initReverseIndex
      , featureState     = []
      , featureCaches    = [
              CacheComponent {
                     cacheDesc       = "reverse index",
                     getCacheMemSize = memSize <$> readMemState reverseMemState
               }
             ]
      }

    initReverseIndex :: IO ()
    initReverseIndex = do
            index <- queryGetPackageIndex
            -- We build the proper index earlier, this just fires the reverse hooks
            let allPackages = map pkgInfoId $ concat $ allPackagesByName index
            runHook_ reverseHook allPackages


    reverseResource = fix $ \r -> ReverseResource
          { reversePackage = resourceAt "/package/:package/reverse.:format"
          , reversePackageOld = resourceAt "/package/:package/reverse/old.:format"
          , reversePackageAll = resourceAt "/package/:package/reverse/all.:format"
          , reversePackageStats = resourceAt "/package/:package/reverse/summary.:format"
          , reversePackages = resourceAt "/packages/reverse.:format"
          , reversePackagesAll = resourceAt "/packages/reverse/all.:format"

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
    queryReverseIndex :: MonadIO m => m ReverseIndex
    queryReverseIndex = readMemState reverseMemState

    queryReverseDeps :: MonadIO m => PackageName -> m ([PackageName], [PackageName])
    queryReverseDeps pkgname = do
        pkgIndex <- queryGetPackageIndex
        ms <- readMemState reverseMemState
        let rdeps = getDependencies' pkgname pkgIndex ms
            rdepsall = getDependenciesI' pkgname pkgIndex ms
            indirect = Set.difference rdepsall rdeps
        return (Set.toList rdeps, Set.toList indirect)

    revPackageId :: MonadIO m => PackageId -> m ReverseDisplay
    revPackageId pkgid = do
        dispInfo <- revDisplayInfo
        revs <- queryReverseIndex
        return $ perVersionReverse dispInfo revs pkgid

    revPackageName :: MonadIO m => PackageName -> m ReverseDisplay
    revPackageName pkgname = do
        dispInfo <- revDisplayInfo
        pkgIndex <- queryGetPackageIndex
        revs <- queryReverseIndex
        return $ perPackageReverse dispInfo pkgIndex revs pkgname

    revJSON :: MonadIO m => m ByteString
    revJSON = do
        ReverseIndex revdeps nodemap <- queryReverseIndex
        let assoc = takeWhile (\(a,_) -> a < Bimap.size nodemap) $ Arr.assocs . Gr.transposeG $ revdeps
            nodeToString node = unPackageName (packageName (nodemap Bimap.!> node))
            -- nodes = map (uncurry Node) $ map (\n -> (fst n, nodeToString (fst n))) assoc
            edges = map (\(a,b) -> Edge a (nodeToString a) (map (\x-> nodeToString x) b)) assoc
        return $ encode edges

    revDisplayInfo :: MonadIO m => m VersionIndex
    revDisplayInfo = do
        pkgIndex <- queryGetPackageIndex
        prefs <- queryGetPreferredVersions
        return $ getDisplayInfo prefs pkgIndex

    renderReverseWith :: (Functor m, MonadIO m) => PackageName -> ReverseDisplay -> (Maybe VersionStatus -> Bool) -> m ReversePageRender
    renderReverseWith pkg rev filterFunc = do
        let rev' = map fst $ Map.toList rev
        revcount <- mapM revPackageStats (pkg:rev')
        let counts = zip (pkg:rev') (map directCount revcount)
            toRender (i, i') (pkgname, (version, status)) = if filterFunc status then (,) (i+1, i') $ Just ReverseRender {
                                                                rendRevPkg = PackageIdentifier pkgname version,
                                                                rendRevStatus = status,
                                                                rendRevCount = fromJust $ lookup pkgname counts
                                                            } else (,) (i, i'+1) Nothing
            (res, rlist) = mapAccumL toRender (0, 0) (Map.toList rev)
            pkgCount = fromJust $ lookup pkg counts
        return $ ReversePageRender (catMaybes rlist) res pkgCount

    renderReverseRecent :: (Functor m, MonadIO m) => PackageName -> ReverseDisplay -> m ReversePageRender
    renderReverseRecent pkg rev = renderReverseWith pkg rev $ \status -> case status of
        Just DeprecatedVersion -> False
        Nothing -> False
        _ -> True

    renderReverseOld :: (Functor m, MonadIO m) => PackageName -> ReverseDisplay -> m ReversePageRender
    renderReverseOld pkg rev = renderReverseWith pkg rev $ \status -> case status of
        Just DeprecatedVersion -> True
        Nothing -> True
        _ -> False

    -- -- This could also differentiate between direct and indirect dependencies
    -- -- with a bit more calculation.
    revPackageFlat :: (Functor m, MonadIO m) => PackageName -> m [(PackageName, Int)]
    revPackageFlat pkgname = do
        pkgIndex <- queryGetPackageIndex
        deps <- getDependenciesI' pkgname pkgIndex <$> readMemState reverseMemState
        counts <- mapM revPackageStats $ Set.toList deps
        return $ zip (Set.toList deps) (map totalCount counts)

    revPackageStats :: (Functor m, MonadIO m) => PackageName -> m ReverseCount
    revPackageStats pkgname = do
            pkgIndex <- queryGetPackageIndex
            getReverseCount' pkgname pkgIndex <$> readMemState reverseMemState

    revPackageSummary :: (Functor m, MonadIO m) => PackageId -> m (Int, Int)
    revPackageSummary pkg = getReverseCountId' pkg <$> readMemState reverseMemState

    -- -- This returns a list of (package name, direct dependencies, flat dependencies)
    -- -- for all packages. An interesting fact: it even does so for packages which
    -- -- don't exist in the index, except the latter two fields are always zero. This is
    -- -- because no versions of these packages exist, so the union of no versions is
    -- -- still no versions. TODO: use this fact to make an index of dependencies which
    -- -- are not in Hackage at all, which might be useful for fixing accidentally
    -- -- broken packages.
    revSummary :: (Functor m, MonadIO m) => m [(PackageName, Int, Int)]
    revSummary = do
        index <- queryGetPackageIndex
        let pkgnames = packageNames index
            util = sortBy (\(_,d1,_) (_,d2,_) -> compare d1 d2)
        counts <- mapM revPackageStats pkgnames
        return $ util $ zip3 pkgnames (map directCount counts) (map totalCount counts)