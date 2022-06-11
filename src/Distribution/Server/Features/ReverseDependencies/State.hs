{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Server.Features.ReverseDependencies.State
  ( ReverseIndex(..)
  , ReverseDisplay
  , ReverseCount(..)
  , VersionIndex
  , addPackage
  , constructReverseIndex
  , dependsOnPkg
  , emptyReverseIndex
  , getDepNames
  , getDependencies
  , getDependenciesFlat
  , getDependenciesFlatRaw
  , getDependenciesRaw
  , getDirectCount
  , getDisplayInfo
  , getReverseCount
  , getTotalCount
  , perPackageReverse
  , perVersionReverse
  , suc
  )
  where

import           Prelude hiding (lookup)

import           Control.Arrow ((&&&))
import           Control.Monad (forM)
import           Control.Monad.Catch
import           Control.Monad.Reader (MonadIO)
import qualified Data.Array as Arr ((!), assocs, accumArray)
import           Data.Bimap (Bimap, lookup, lookupR)
import qualified Data.Bimap as Bimap
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (union)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, mapMaybe, maybeToList)
import qualified Data.Set as Set
import           Data.Set (Set, fromList, toList, delete)
import           Data.Typeable (Typeable)
import           Data.Graph (Graph, Vertex)
import qualified Data.Graph as Gr

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Packages.Types
import           Distribution.Server.Framework.MemSize
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import           Distribution.Version

import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS

emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex (Gr.buildG (0,0) []) Bimap.empty mempty

type NodeId = Int
type RevDeps = Graph

data ReverseIndex = ReverseIndex
  { reverseDependencies :: !RevDeps
  , packageNodeIdMap    :: !(Bimap PackageName NodeId)
  , deps :: Map PackageIdentifier [Dependency]
  } deriving (Eq, Show, Typeable)

instance MemSize Dependency where
    memSize = fromIntegral . BS.length . encode

instance MemSize ReverseIndex where
    memSize (ReverseIndex a b c) = memSize3 a b c

constructReverseIndex :: MonadCatch m => PackageIndex PkgInfo -> m ReverseIndex
constructReverseIndex index = do
    let nodePkgMap = foldr (uncurry Bimap.insert) Bimap.empty $ zip (PackageIndex.allPackageNames index) [0..]
    (revs, deps) <- constructRevDeps index nodePkgMap
    pure $
      ReverseIndex
        { reverseDependencies = revs
        , packageNodeIdMap = nodePkgMap
        , deps = deps
        }

addPackage :: (MonadCatch m, MonadIO m) => PackageIndex PkgInfo -> PackageName -> [PackageName]
           -> ReverseIndex -> m ReverseIndex
addPackage index pkgname deps ri@(ReverseIndex revs nodemap pkgIdToDeps) = do
  let
    npm = Bimap.tryInsert pkgname (Bimap.size nodemap) nodemap
  new :: [(Int, [Int])] <-
    forM deps $ \d ->
      (,) <$> lookup d npm <*> fmap (:[]) (lookup pkgname npm)
  let rd = insEdges (Bimap.size npm) new revs
      pkginfos = PackageIndex.lookupPackageName index pkgname
      newPackageDepMap = Map.fromList $ map (packageId &&& getDeps) pkginfos
  pure
    ri
      { reverseDependencies = rd
      , packageNodeIdMap = npm
      , deps = Map.union newPackageDepMap pkgIdToDeps
      }

constructRevDeps :: forall m. MonadCatch m => PackageIndex PkgInfo -> Bimap PackageName NodeId -> m (RevDeps, Map PackageIdentifier [Dependency])
constructRevDeps index nodemap = do
    let allPackages :: [PkgInfo]
        allPackages = concat $ PackageIndex.allPackagesByName index
        nodeIdsOfDependencies :: PkgInfo -> m [(NodeId, NodeId)]
        nodeIdsOfDependencies pkg = catMaybes <$> mapM findNodesIfPresent (getDepNames pkg)
          where
          findNodesIfPresent :: PackageName -> m (Maybe (NodeId, NodeId))
          findNodesIfPresent dep = do
            eitherErrOrFound :: Either SomeException (NodeId, NodeId) <-
              try $ (,) <$> lookup dep nodemap <*> lookup (packageName pkg) nodemap
            pure $ either (const Nothing) Just eitherErrOrFound
    -- This will mix dependencies of different versions of the same package, but that is intended.
    edges <- traverse nodeIdsOfDependencies allPackages
    let deps = Map.fromList $ map (packageId &&& getDeps) allPackages

    pure (Gr.buildG (0, Bimap.size nodemap) (nubOrd $ concat edges)
         , deps
         )

getDeps :: PkgInfo -> [Dependency]
getDeps pkg =
    concatMap harvestDependencies (maybeToList $ condLibrary $ pkgDesc pkg)

getDepNames :: PkgInfo -> [PackageName]
getDepNames pkg =
  map depPkgName $ getDeps pkg

-- | Returns [containers 0.5.0.0 to 0.6.0.1] for needle=ghc-prim-0.6.0.0 and packageHaystack=containers
-- | because these are the versions that could accept that version of ghc-prim as a dep
-- | Note that this doesn't include executables! Only the library.
dependsOnPkg :: PackageIndex PkgInfo -> PackageId -> PackageName -> Map PackageIdentifier [Dependency] -> Set PackageIdentifier
dependsOnPkg index needle packageHaystack deps =
    fromList $ mapMaybe descPermits (PackageIndex.lookupPackageName index packageHaystack)
    where
        descPermits :: PkgInfo -> Maybe PackageIdentifier
        descPermits pkginfo
          | Just found <- Map.lookup (packageId pkginfo) deps
          , any toDepsList found = Just (packageId pkginfo)
          | otherwise = Nothing
        toDepsList (Dependency name versionRange _) =
              packageVersion needle `withinRange` versionRange
              && packageName needle == name

-- | Collect all dependencies from all branches of a condition tree.
harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
harvestDependencies (CondNode _ deps comps) = deps ++ concatMap forComponent comps
  where forComponent (CondBranch _ iftree elsetree) = harvestDependencies iftree ++ maybe [] harvestDependencies elsetree

---------------------------------ReverseDisplay

--------------------------------------------------------------------------------
-- Calculating ReverseDisplays
data ReverseCount = ReverseCount
  { directCount :: Int
  , totalCount :: Int
  } deriving (Show, Eq, Typeable, Ord)

instance MemSize ReverseCount where
    memSize (ReverseCount a b) = memSize2 a b

type ReverseDisplay = Map PackageName (Version, Maybe VersionStatus)

type VersionIndex = (PackageName -> (PreferredInfo, [Version]))

perPackageReverse :: MonadCatch m => (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageName -> m (Map PackageName (Version, Maybe VersionStatus))
perPackageReverse indexFunc index revdeps pkg = do
  let pkgids = (packageVersion. packageId) <$> PackageIndex.lookupPackageName index pkg
  let best :: PackageId
      best = PackageIdentifier pkg (maximum pkgids)
  perVersionReverse indexFunc index revdeps best

perVersionReverse :: MonadCatch m => (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageId -> m (Map PackageName (Version, Maybe VersionStatus))
perVersionReverse indexFunc index (ReverseIndex revs nodemap deps) pkg = do
    found <- lookup (packageName pkg) nodemap
    -- this will be too much, since we are throwing away the specific version
    revDepNames :: Set PackageName <- fromList <$> mapM (`lookupR` nodemap) (toList $ suc revs found)
    let packagemap :: Map PackageName (Set Version)
        packagemap = Map.fromList $ map (\x -> (x, Set.map packageVersion $ dependsOnPkg index pkg x deps)) (toList revDepNames)
    pure $ constructReverseDisplay indexFunc packagemap

constructReverseDisplay :: (PackageName -> (PreferredInfo, [Version])) -> Map PackageName (Set Version) -> Map PackageName (Version, Maybe VersionStatus)
constructReverseDisplay indexFunc =
    Map.mapMaybeWithKey (uncurry maybeBestVersion . indexFunc)

getDisplayInfo :: PreferredVersions -> PackageIndex PkgInfo -> VersionIndex
getDisplayInfo preferred index pkgname = (,)
    (Map.findWithDefault emptyPreferredInfo pkgname $ preferredMap preferred)
    (map packageVersion . PackageIndex.lookupPackageName index $ pkgname)


------------------------------------ Utility

----------------------------Graph Utility----------
suc :: RevDeps -> Vertex -> Set Vertex
suc g v = fromList $ g Arr.! v

insEdges :: Int -> [(NodeId, [NodeId])] -> RevDeps -> RevDeps
insEdges nodesize edges revdeps = Arr.accumArray union [] (0, nodesize) (edges ++ Arr.assocs revdeps)

--------------------------------------

getDependencies :: MonadCatch m => PackageName -> ReverseIndex -> m (Set PackageName)
getDependencies pkg revs =
  names revs =<< getDependenciesRaw pkg revs

getDependenciesRaw :: MonadCatch m => PackageName -> ReverseIndex -> m (Set NodeId)
getDependenciesRaw pkg (ReverseIndex revdeps nodemap _) = do
  enodeid <- try (lookup pkg nodemap)
  onRight enodeid $ \nodeid ->
    nodeid `delete` suc revdeps nodeid

-- | The flat/total/transitive/indirect reverse dependencies are all the packages that depend on something that depends on the given 'pkg'
getDependenciesFlat :: forall m. MonadCatch m => PackageName -> ReverseIndex -> m (Set PackageName)
getDependenciesFlat pkg revs =
  names revs =<< getDependenciesFlatRaw pkg revs

getDependenciesFlatRaw :: forall m. MonadCatch m  => PackageName -> ReverseIndex -> m (Set NodeId)
getDependenciesFlatRaw pkg (ReverseIndex revdeps nodemap _) = do
  enodeid <- try (lookup pkg nodemap)
  onRight enodeid $ \nodeid ->
    nodeid `delete` fromList (Gr.reachable revdeps nodeid)

-- | The direct dependencies depend on the given 'pkg' directly, i.e. not transitively
getDirectCount :: MonadCatch m => PackageName -> ReverseIndex -> m Int
getDirectCount pkg revs = do
  length <$> getDependenciesRaw pkg revs

-- | Given a set of NodeIds, look up the package names for all of them
names :: MonadThrow m => ReverseIndex -> Set NodeId -> m (Set PackageName)
names (ReverseIndex _ nodemap _) ids = do
  fromList <$> mapM (`lookupR` nodemap) (toList ids)

onRight :: Monad m => Either SomeException t -> (t -> Set NodeId) -> m (Set NodeId)
onRight e fun = do
  case e of
    Left (_ :: SomeException) -> do
      pure mempty
    Right nodeid ->
      pure $ fun nodeid

-- | The flat/total/transitive/indirect dependency count is the amount of package names that depend transitively on the given 'pkg'
getTotalCount :: MonadCatch m => PackageName -> ReverseIndex -> m Int
getTotalCount pkg revs = do
  length <$> getDependenciesFlatRaw pkg revs

getReverseCount :: MonadCatch m => PackageName -> ReverseIndex -> m (Int, Int)
getReverseCount pkg revs = do
  direct <- getDirectCount pkg revs
  total <- getTotalCount pkg revs
  pure (direct, total)
