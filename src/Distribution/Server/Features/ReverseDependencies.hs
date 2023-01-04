{-# LANGUAGE DeriveGeneric, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.ReverseDependencies (
    ReverseCount(..),
    ReverseFeature(..),
    ReversePageRender(..),
    ReverseRender(..),
    ReverseResource(..),
    initReverseFeature,
    reverseFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Features.PreferredVersions.State (PreferredVersions)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex, packageNames, allPackagesByNameNE)
import Distribution.Server.Packages.Types (PkgInfo)
import Distribution.Server.Features.ReverseDependencies.State
import Distribution.Package
import Distribution.Text (display)
import Distribution.Version (Version)

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.List (mapAccumL, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Function (fix)
import qualified Data.Bimap as Bimap
import qualified Data.Array as Arr
import qualified Data.Graph as Gr
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics hiding (packageName)

data ReverseFeature = ReverseFeature {
    reverseFeatureInterface :: HackageFeature,

    reverseResource :: ReverseResource,

    reverseHook :: Hook [NE.NonEmpty PkgInfo] (),

    queryReverseDeps :: forall m. MonadIO m => PackageName -> m ([PackageName], [PackageName]),
    revPackageId :: forall m. MonadIO m => PackageId -> m ReverseDisplay,
    revPackageName :: forall m. MonadIO m => PackageName -> m ReverseDisplay,
    renderReverseRecent :: forall m. MonadIO m => PackageName -> ReverseDisplay -> m ReversePageRender,
    renderReverseOld :: forall m. MonadIO m => PackageName -> ReverseDisplay -> m ReversePageRender,
    revPackageFlat :: forall m. MonadIO m => PackageName -> m [(PackageName, Int)],
    revDirectCount :: forall m. MonadIO m => PackageName -> m Int,
    revPackageStats :: forall m. MonadIO m => PackageName -> m ReverseCount,
    revCountForAllPackages :: forall m. MonadIO m => m [(PackageName, ReverseCount)],
    revJSON :: IO ByteString,
    revDisplayInfo :: forall m. MonadIO m => m VersionIndex,
    revForEachVersion :: PackageName -> IO (Map.Map Version (Set PackageIdentifier))
}

instance IsHackageFeature ReverseFeature where
    getFeatureInterface = reverseFeatureInterface

data ReverseResource = ReverseResource {
    reversePackage :: Resource,
    reversePackageOld :: Resource,
    reversePackageFlat :: Resource,
    reversePackageVerbose :: Resource,
    reversePackages :: Resource,

    reverseUri :: String -> PackageId -> String,
    reverseNameUri :: String -> PackageName -> String,
    reverseOldUri :: String -> PackageId -> String,
    reverseFlatUri :: String -> PackageName -> String,
    reverseVerboseUri :: String -> PackageName -> String
}


initReverseFeature :: ServerEnv
                   -> IO (CoreFeature
                       -> VersionsFeature
                       -> IO ReverseFeature)
initReverseFeature _ = do
    updateReverse <- newHook

    return $ \CoreFeature{queryGetPackageIndex,packageChangeHook}
             VersionsFeature{queryGetPreferredVersions} -> do
      idx <- queryGetPackageIndex
      memState <- newMemStateWHNF $ constructReverseIndex idx

      let feature = reverseFeature queryGetPackageIndex queryGetPreferredVersions memState updateReverse

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
            Nothing -> return () --PackageRemoveHook
            Just pkginfo -> do
                index <- queryGetPackageIndex
                modifyMemState memState $ addPackage index (packageName pkgid) (getDepNames pkginfo)
                runHook_ updateReverse [pure pkginfo]

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

reverseFeature :: IO (PackageIndex PkgInfo)
               -> IO PreferredVersions
               -> MemState ReverseIndex
               -> Hook [NE.NonEmpty PkgInfo] ()
               -> ReverseFeature

reverseFeature queryGetPackageIndex
               queryGetPreferredVersions
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
            index <- liftIO queryGetPackageIndex
            -- We build the proper index earlier, this just fires the reverse hooks
            runHook_ reverseHook $ allPackagesByNameNE index


    reverseResource = fix $ \r -> ReverseResource
          { reversePackage = resourceAt "/package/:package/reverse.:format"
          , reversePackageOld = resourceAt "/package/:package/reverse/old.:format"
          , reversePackageFlat = resourceAt "/package/:package/reverse/flat.:format"
          , reversePackageVerbose = resourceAt "/package/:package/reverse/verbose.:format"
          , reversePackages = resourceAt "/packages/reverse.:format"

          , reverseUri = \format pkg -> renderResource (reversePackage r) [display pkg, format]
          , reverseNameUri = \format pkg -> renderResource (reversePackage r) [display pkg, format]
          , reverseOldUri = \format pkg -> renderResource (reversePackageOld r) [display pkg, format]
          , reverseFlatUri = \format pkg -> renderResource (reversePackageFlat r) [display pkg, format]
          , reverseVerboseUri = \format pkg -> renderResource (reversePackageVerbose r) [display pkg, format]
          }

    --textRevDisplay :: ReverseDisplay -> String
    --textRevDisplay m = unlines . map (\(n, (v, m)) -> display n ++ "-" ++ display v ++ ": " ++ show m) . Map.toList $ m

    -- If VersionStatus caching is used, revPackageId and revPackageName could be
    -- reduced to a single map lookup (see Distribution.Server.Packages.Reverse).
    queryReverseIndex :: MonadIO m => m ReverseIndex
    queryReverseIndex = readMemState reverseMemState

    queryReverseDeps :: MonadIO m => PackageName -> m ([PackageName], [PackageName])
    queryReverseDeps pkgname = do
        ms <- readMemState reverseMemState
        let rdeps = getDependencies pkgname ms
            rdepsall = getDependenciesFlat pkgname ms
            indirect = Set.difference rdepsall rdeps
        pure (Set.toList rdeps, Set.toList indirect)

    revPackageId :: MonadIO m => PackageId -> m ReverseDisplay
    revPackageId pkgid = do
        dispInfo <- revDisplayInfo
        pkgIndex <- liftIO queryGetPackageIndex
        revs <- queryReverseIndex
        pure $ perVersionReverse dispInfo pkgIndex revs pkgid

    revPackageName :: MonadIO m => PackageName -> m ReverseDisplay
    revPackageName pkgname = do
        dispInfo <- revDisplayInfo
        pkgIndex <- liftIO queryGetPackageIndex
        revs <- queryReverseIndex
        pure $ perPackageReverse dispInfo pkgIndex revs pkgname

    revJSON :: IO ByteString
    revJSON = do
        ReverseIndex revdeps nodemap _depmap <- queryReverseIndex
        let assoc = takeWhile (\(a,_) -> a < Bimap.size nodemap) $ Arr.assocs . Gr.transposeG $ revdeps
            nodeToString node = unPackageName (nodemap Bimap.!> node)
            -- nodes = map (uncurry Node) $ map (\n -> (fst n, nodeToString (fst n))) assoc
            edges = map (\(a,b) -> Edge a (nodeToString a) (map (\x-> nodeToString x) b)) assoc
        return $ encode edges

    revDisplayInfo :: MonadIO m => m VersionIndex
    revDisplayInfo = do
        pkgIndex <- liftIO queryGetPackageIndex
        prefs <- liftIO queryGetPreferredVersions
        return $ getDisplayInfo prefs pkgIndex

    renderReverseWith :: MonadIO m => PackageName -> ReverseDisplay -> (Maybe VersionStatus -> Bool) -> m ReversePageRender
    renderReverseWith pkg rev filterFunc = do
        let rev' = map fst $ Map.toList rev
        directCounts <- mapM revDirectCount (pkg:rev')
        let counts = zip (pkg:rev') directCounts
            toRender (i, i') (pkgname, (version, status)) = if filterFunc status then (,) (i+1, i') $ Just ReverseRender {
                                                                rendRevPkg = PackageIdentifier pkgname version,
                                                                rendRevStatus = status,
                                                                rendRevCount = fromMaybe 0 $ lookup pkgname counts
                                                            } else (,) (i, i'+1) Nothing
            (res, rlist) = mapAccumL toRender (0, 0) (Map.toList rev)
            pkgCount = fromMaybe 0 $ lookup pkg counts
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

    -- -- This could also differentiate between direct and indirect dependencies
    -- -- with a bit more calculation.
    revPackageFlat :: MonadIO m => PackageName -> m [(PackageName, Int)]
    revPackageFlat pkgname = do
        memState <- readMemState reverseMemState
        let depList = Set.toList $ getDependenciesFlat pkgname memState
        pure $ map (\d -> (d, getTotalCount d memState)) depList

    revPackageStats :: MonadIO m => PackageName -> m ReverseCount
    revPackageStats pkgname = do
      (direct, transitive) <- getReverseCount pkgname <$> readMemState reverseMemState
      return $ ReverseCount direct transitive

    revDirectCount :: MonadIO m => PackageName -> m Int
    revDirectCount pkgname = do
      getDirectCount pkgname <$> readMemState reverseMemState

    -- This returns a list of (package name, direct dependencies, flat dependencies)
    -- for all packages. An interesting fact: it even does so for packages which
    -- don't exist in the index, except the latter two fields are always zero. This is
    -- because no versions of these packages exist, so the union of no versions is
    -- still no versions. TODO: use this fact to make an index of dependencies which
    -- are not in Hackage at all, which might be useful for fixing accidentally
    -- broken packages.
    --
    -- The returned list is sorted ascendingly on directCount (see ReverseCount).
    revCountForAllPackages :: MonadIO m => m [(PackageName, ReverseCount)]
    revCountForAllPackages = do
        index <- liftIO queryGetPackageIndex
        let pkgnames = packageNames index
        counts <- mapM revPackageStats pkgnames
        return . sortOn (directCount . snd) $ zip pkgnames counts

    revForEachVersion :: PackageName -> IO (Map.Map Version (Set PackageIdentifier))
    revForEachVersion pkg = do
        ReverseIndex revs nodemap depmap <- readMemState reverseMemState
        index <- queryGetPackageIndex
        let revDepNames :: [PackageName]
            revDepNames = case Bimap.lookup pkg nodemap of
              Nothing -> []
              Just nodeid -> mapMaybe (`Bimap.lookupR` nodemap) (Set.toList $ suc revs nodeid)
        let -- The key is the version of 'pkg', and the values are specific
          -- package versions that accept this version of pkg specified in the key
          revDepVersions :: [(Version, Set PackageIdentifier)]
          revDepVersions = do
            x <- nubOrd revDepNames
            pkginfo <- PackageIndex.lookupPackageName index pkg
            pure (packageVersion pkginfo, dependsOnPkg index (packageId pkginfo) x depmap)
        pure $ Map.fromListWith Set.union revDepVersions
