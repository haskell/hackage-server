{-# LANGUAGE RankNTypes, DeriveGeneric, TypeSynonymInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}

module Distribution.Server.Features.ReverseDependencies.State
  ( ReverseIndex(..)
  , ReverseDisplay
  , ReverseCount(..)
  , VersionIndex
  , addPackage
  , constructReverseIndex
  , dependsOnPkg
  , emptyReverseIndex
  , getAllDependencies
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
import           Data.Maybe (maybeToList, mapMaybe)
import           Data.SafeCopy hiding (Version)
import qualified Data.Set as Set
import           Data.Set (Set, fromList, toList, delete)
import           Data.Typeable (Typeable)
import           Data.Graph (Graph, Vertex)
import qualified Data.Graph as Gr
import           GHC.Stack (HasCallStack, callStack, prettyCallStack)

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Packages.Types
import           Distribution.Server.Framework.MemSize
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import           Distribution.Version


emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex (Gr.buildG (0,0) []) Bimap.empty

type NodeId = Int
type RevDeps = Graph

data ReverseIndex = ReverseIndex {
    reverseDependencies :: !RevDeps,
    packageNodeIdMap    :: !(Bimap PackageName NodeId)
} deriving (Eq, Show, Typeable)

instance MemSize ReverseIndex where
    memSize (ReverseIndex a b) = memSize2 a b


constructReverseIndex :: (MonadCatch m, HasCallStack) => PackageIndex PkgInfo -> m ReverseIndex
constructReverseIndex index = do
    let nodePkgMap = foldr (uncurry Bimap.insert) Bimap.empty $ zip (PackageIndex.allPackageNames index) [0..]
    revs <- constructRevDeps index nodePkgMap
    pure $
      ReverseIndex {
        reverseDependencies = revs,
        packageNodeIdMap = nodePkgMap
      }

addPackage :: (HasCallStack, MonadCatch m, MonadIO m) => HasCallStack => PackageName -> [PackageName]
           -> ReverseIndex -> m ReverseIndex
addPackage pkgid deps ri@(ReverseIndex revs nodemap) = do
  let
    npm = Bimap.tryInsert pkgid (Bimap.size nodemap) nodemap
  new :: [(Int, [Int])] <-
    forM deps $ \d ->
      (,) <$> looky d npm <*> fmap (:[]) (looky pkgid npm)
      --pure (npm ! d, [npm ! pkgid])
  let rd = insEdges (Bimap.size npm) new revs
  pure $
    ri {
        reverseDependencies = rd,
        packageNodeIdMap = npm
    }

constructRevDeps :: forall m. (MonadCatch m, HasCallStack) => PackageIndex PkgInfo -> Bimap PackageName NodeId -> m RevDeps
constructRevDeps index nodemap = do
    let allPackages :: [PkgInfo]
        allPackages = concat {- concatMap (take 5) -} $ PackageIndex.allPackagesByName index
        g :: PkgInfo -> m [(NodeId, NodeId)]
        g pkg = mapM (f pkg) (getAllDependencies pkg index)
        f :: PkgInfo -> PackageName -> m (NodeId, NodeId)
        f pkg dep = (,) <$> (lookup dep nodemap) <*> (lookup (packageName pkg) nodemap :: m NodeId)
    edges <- traverse g allPackages

    pure $ Gr.buildG (0, Bimap.size nodemap) (nubOrd $ concat edges)

getAllDependencies :: forall pkg. Package pkg => PkgInfo -> PackageIndex pkg -> [PackageName]
getAllDependencies pkg index =
    map packageName $ toDepsList (maybeToList $ condLibrary desc) ++ toDepsList (map snd $ condExecutables desc)
    where
        desc = pkgDesc pkg
        toDepsList :: [CondTree v [Dependency] a] -> [pkg]
        toDepsList l = concatMap (PackageIndex.lookupDependency index) $ concatMap harvestDependencies l
        -- | Collect all dependencies from all branches of a condition tree.
        harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
        harvestDependencies (CondNode _ deps comps) = deps ++ concatMap forComponent comps
          where forComponent (CondBranch _ iftree elsetree) = harvestDependencies iftree ++ maybe [] harvestDependencies elsetree

-- | Returns [containers 0.5.0.0 to 0.6.0.1] for needle=ghc-prim-0.6.0.0 and packageHaystack=containers
-- | because these are the versions that could accept that version of ghc-prim as a dep
-- | Note that this doesn't include executables! Only the library.
dependsOnPkg :: PackageIndex PkgInfo -> PackageId -> PackageName -> Set PackageIdentifier
dependsOnPkg index needle packageHaystack =
    fromList $ mapMaybe descPermits (PackageIndex.lookupPackageName index packageHaystack)
    where
        descPermits :: PkgInfo -> Maybe PackageIdentifier
        descPermits pkginfo
          | any toDepsList . concatMap harvestDependencies . condLibrary $ pkgDesc pkginfo = Just (packageId pkginfo)
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

-- | Look up 'key', but in case of failure, fails with a more informative error message than what Bimap provides
looky :: (HasCallStack, MonadCatch m, Ord a, Ord b, Show a) => a -> Bimap a b -> m b
looky key bimap = do
  res <- try (lookup key bimap)
  case res of
    Left (_ :: SomeException) -> throwM $ userError $ "couldn't find key " ++ show key ++ " " ++ prettyCallStack callStack
    Right a -> pure a

perPackageReverse :: (HasCallStack, MonadCatch m) => (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageName -> m (Map PackageName (Version, Maybe VersionStatus))
perPackageReverse indexFunc index revdeps pkg = do
  let pkgids = (packageVersion. packageId) <$> PackageIndex.lookupPackageName index pkg
  let best :: PackageId
      best = PackageIdentifier pkg (maximum pkgids)
  perVersionReverse indexFunc index revdeps best

perVersionReverse :: (HasCallStack, MonadCatch m) => (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageId -> m (Map PackageName (Version, Maybe VersionStatus))
perVersionReverse indexFunc index (ReverseIndex revs nodemap) pkg = do
    found <- lookup (packageName pkg) nodemap
    -- this will be too much, since we are throwing away the specific version
    revDepNames :: Set PackageName <- fromList <$> mapM (`lookupR` nodemap) (toList $ suc revs found)
    let packagemap :: Map PackageName (Set Version)
        packagemap = Map.fromList $ map (\x -> (x, Set.map packageVersion $ dependsOnPkg index pkg x)) (toList revDepNames)
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

instance (SafeCopy a, SafeCopy b, Ord a, Ord b) => SafeCopy (Bimap a b) where
    getCopy = contain $ fmap Bimap.fromList safeGet
    putCopy = contain . safePut . Bimap.toList

-- instance (SafeCopy a, SafeCopy b) => SafeCopy (Graph) where
--     putCopy = contain . safePut . Gr.edges
--     getCopy = contain $ fmap (Gr.buildG (0,maxNodes)) safeGet

$(deriveSafeCopy 0 'base ''ReverseIndex)
$(deriveSafeCopy 0 'base ''ReverseCount)

getDependencies :: (MonadCatch m, HasCallStack) => PackageName -> ReverseIndex -> m (Set PackageName)
getDependencies pkg revs =
  names revs =<< getDependenciesRaw pkg revs

getDependenciesRaw :: (MonadCatch m, HasCallStack) => PackageName -> ReverseIndex -> m (Set NodeId)
getDependenciesRaw pkg (ReverseIndex revdeps nodemap) = do
  enodeid <- try (looky pkg nodemap)
  onRight enodeid $ \nodeid ->
    nodeid `delete` suc revdeps nodeid

-- | The flat/total/transitive/indirect reverse dependencies are all the packages that depend on something that depends on the given 'pkg'
getDependenciesFlat :: forall m. (MonadCatch m, HasCallStack) => PackageName -> ReverseIndex -> m (Set PackageName)
getDependenciesFlat pkg revs =
  names revs =<< getDependenciesFlatRaw pkg revs

getDependenciesFlatRaw :: forall m. (MonadCatch m, HasCallStack) => PackageName -> ReverseIndex -> m (Set NodeId)
getDependenciesFlatRaw pkg (ReverseIndex revdeps nodemap) = do
  enodeid <- try (looky pkg nodemap)
  onRight enodeid $ \nodeid ->
    nodeid `delete` fromList (Gr.reachable revdeps nodeid)

-- | The direct dependencies depend on the given 'pkg' directly, i.e. not transitively
getDirectCount :: MonadCatch m => PackageName -> ReverseIndex -> m Int
getDirectCount pkg revs = do
  length <$> getDependenciesRaw pkg revs

-- | Given a set of NodeIds, look up the package names for all of them
names :: MonadThrow m => ReverseIndex -> Set NodeId -> m (Set PackageName)
names (ReverseIndex _ nodemap) ids = do
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

getReverseCount :: (HasCallStack, MonadCatch m) => PackageName -> ReverseIndex -> m (Int, Int)
getReverseCount pkg revs = do
  direct <- getDirectCount pkg revs
  total <- getTotalCount pkg revs
  pure (direct, total)
