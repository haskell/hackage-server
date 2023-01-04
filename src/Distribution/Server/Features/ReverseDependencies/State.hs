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
import qualified Data.Array as Arr ((!), assocs, accumArray)
import           Data.Bimap (Bimap, lookup, lookupR)
import qualified Data.Bimap as Bimap
import           Data.Containers.ListUtils (nubOrd)
import           Data.List (union)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, maybeToList)
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

constructReverseIndex :: PackageIndex PkgInfo -> ReverseIndex
constructReverseIndex index =
    let nodePkgMap = foldr (uncurry Bimap.insert) Bimap.empty $ zip (PackageIndex.allPackageNames index) [0..]
        (revs, dependencies) = constructRevDeps index nodePkgMap
    in ReverseIndex
        { reverseDependencies = revs
        , packageNodeIdMap = nodePkgMap
        , deps = dependencies
        }

addPackage :: PackageIndex PkgInfo -> PackageName -> [PackageName]
           -> ReverseIndex -> ReverseIndex
addPackage index pkgname dependencies (ReverseIndex revs nodemap pkgIdToDeps) =
  let npm = Bimap.tryInsert pkgname (Bimap.size nodemap) nodemap
      pn = (:[]) <$> lookup pkgname npm
      new :: [(Int, [Int])]
      new = mapMaybe (\d -> (,) <$> lookup d npm <*> pn) dependencies
      rd = insEdges (Bimap.size npm) new revs
      pkginfos = PackageIndex.lookupPackageName index pkgname
      newPackageDepMap = Map.fromList $ map (packageId &&& getDeps) pkginfos
  in ReverseIndex
      { reverseDependencies = rd
      , packageNodeIdMap = npm
      , deps = Map.union newPackageDepMap pkgIdToDeps
      }

constructRevDeps :: PackageIndex PkgInfo -> Bimap PackageName NodeId -> (RevDeps, Map PackageIdentifier [Dependency])
constructRevDeps index nodemap =
    let allPackages :: [PkgInfo]
        allPackages = concat $ PackageIndex.allPackagesByName index
        nodeIdsOfDependencies :: PkgInfo -> [(NodeId, NodeId)]
        nodeIdsOfDependencies pkg = mapMaybe (\dep -> (,) <$> lookup dep nodemap <*> lookup (packageName pkg) nodemap) (getDepNames pkg)
        -- This will mix dependencies of different versions of the same package, but that is intended.
        edges = map nodeIdsOfDependencies allPackages
        dependencies = Map.fromList $ map (packageId &&& getDeps) allPackages

    in (Gr.buildG (0, Bimap.size nodemap) (nubOrd $ concat edges)
         , dependencies
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
dependsOnPkg index needle packageHaystack dependencies =
    fromList $ mapMaybe descPermits (PackageIndex.lookupPackageName index packageHaystack)
    where
        descPermits :: PkgInfo -> Maybe PackageIdentifier
        descPermits pkginfo
          | Just found <- Map.lookup (packageId pkginfo) dependencies
          , any toDepsList found = Just (packageId pkginfo)
          | otherwise = Nothing
        toDepsList (Dependency name versionRange _) =
              packageVersion needle `withinRange` versionRange
              && packageName needle == name

-- | Collect all dependencies from all branches of a condition tree.
harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
harvestDependencies (CondNode _ dependencies comps) = dependencies ++ concatMap forComponent comps
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

perPackageReverse :: (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageName -> Map PackageName (Version, Maybe VersionStatus)
perPackageReverse indexFunc index revdeps pkg =
  let pkgids = (packageVersion. packageId) <$> PackageIndex.lookupPackageName index pkg
      best :: PackageId
      best = PackageIdentifier pkg (maximum pkgids)
  in perVersionReverse indexFunc index revdeps best

perVersionReverse :: (PackageName -> (PreferredInfo, [Version])) -> PackageIndex PkgInfo -> ReverseIndex -> PackageId -> Map PackageName (Version, Maybe VersionStatus)
perVersionReverse indexFunc index (ReverseIndex revs nodemap dependencies) pkg =  case lookup (packageName pkg) nodemap of
    Nothing -> Map.empty
    Just found ->
     -- this will be too much, since we are throwing away the specific version
      let revDepNames = mapMaybe (`lookupR` nodemap) (toList $ suc revs found)
          packagemap :: Map PackageName (Set Version)
          packagemap = Map.fromList $ map (\x -> (x, Set.map packageVersion $ dependsOnPkg index pkg x dependencies)) revDepNames
      in constructReverseDisplay indexFunc packagemap

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

getDependencies :: PackageName -> ReverseIndex -> Set PackageName
getDependencies pkg revs = names revs $ getDependenciesRaw pkg revs

getDependenciesRaw :: PackageName -> ReverseIndex -> Set NodeId
getDependenciesRaw pkg (ReverseIndex revdeps nodemap _) =
  case lookup pkg nodemap of
    Nothing -> mempty
    Just nodeid -> delete nodeid (suc revdeps nodeid)

-- | The flat/total/transitive/indirect reverse dependencies are all the packages that depend on something that depends on the given 'pkg'
getDependenciesFlat :: PackageName -> ReverseIndex -> Set PackageName
getDependenciesFlat pkg revs = names revs $ getDependenciesFlatRaw pkg revs

getDependenciesFlatRaw :: PackageName -> ReverseIndex -> Set NodeId
getDependenciesFlatRaw pkg (ReverseIndex revdeps nodemap _) = do
  case lookup pkg nodemap of
    Nothing -> mempty
    Just nodeid -> delete nodeid $ fromList (Gr.reachable revdeps nodeid)

-- | The direct dependencies depend on the given 'pkg' directly, i.e. not transitively
getDirectCount :: PackageName -> ReverseIndex -> Int
getDirectCount pkg revs = length $ getDependenciesRaw pkg revs

-- | Given a set of NodeIds, look up the package names for all of them
names :: ReverseIndex -> Set NodeId -> Set PackageName
names (ReverseIndex _ nodemap _) ids = do
  fromList $ mapMaybe (`lookupR` nodemap) (toList ids)


-- | The flat/total/transitive/indirect dependency count is the amount of package names that depend transitively on the given 'pkg'
getTotalCount :: PackageName -> ReverseIndex -> Int
getTotalCount pkg revs = length $ getDependenciesFlatRaw pkg revs

getReverseCount :: PackageName -> ReverseIndex -> (Int, Int)
getReverseCount pkg revs = (getDirectCount pkg revs, getTotalCount pkg revs)
