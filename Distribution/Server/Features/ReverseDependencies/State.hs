{-# LANGUAGE RankNTypes, DeriveGeneric, TypeSynonymInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.ReverseDependencies.State where

import Distribution.Server.Packages.Types
import Distribution.Server.Features.Core
import Distribution.Server.Features.Core.State
import Distribution.Server.Framework.MemSize
import Distribution.Server.Features.PreferredVersions.State
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

import Data.Acid (Query, Update, makeAcidic)
import Data.List (foldl', union, (\\))
import Data.Maybe (isJust, mapMaybe, maybeToList, fromMaybe)
import Data.SafeCopy hiding (Version)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bimap (Bimap, (!), (!>))
import qualified Data.Bimap as Bimap
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (void)
import Data.STRef
import Control.Monad.ST
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask, asks)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Gr
import qualified Data.Array as Arr ((!), accum, bounds)

type NodeId = Int
type RevDeps = Graph

data ReverseIndex = ReverseIndex {
    duplicatedIndex :: PackageIndex PackageId,
    reverseDependencies :: RevDeps,
    packageNodeIdMap :: Bimap PackageId NodeId
} deriving (Eq, Show, Typeable)

instance MemSize ReverseIndex where
    memSize (ReverseIndex a b c) = memSize3 a b c


emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex (PackageIndex.fromList []) (Gr.buildG (0, 20000) []) Bimap.empty

constructReverseIndex :: PackageIndex PkgInfo -> ReverseIndex
constructReverseIndex index =
    emptyReverseIndex {
        duplicatedIndex = constructDupIndex index,
        reverseDependencies = constructRevDeps index nodePkgMap,
        packageNodeIdMap = nodePkgMap
    }
    where
        nodePkgMap = foldr (uncurry Bimap.insert) Bimap.empty $ zip (map packageId (PackageIndex.allPackages index)) [0..]
        constructDupIndex :: PackageIndex PkgInfo -> PackageIndex PackageId
        constructDupIndex = PackageIndex.fromList
          . map packageId
          . PackageIndex.allPackages

addPackage :: PackageId -> [PackageId]
           -> ReverseIndex -> ReverseIndex
addPackage pkgid deps ri@(ReverseIndex pindex revs nodemap) =
    let index = PackageIndex.insert pkgid pindex
        rd' | revSize revs < Bimap.size nodemap = arrDouble revs | otherwise = revs
        npm = Bimap.tryInsert pkgid (Bimap.size nodemap) nodemap
        rd = insEdges (map (\d -> (npm ! d, [npm ! pkgid])) deps) rd'
    in ri {
        duplicatedIndex = index,
        reverseDependencies = rd,
        packageNodeIdMap = npm
    }

removePackage :: PackageId -> [PackageId]
              -> ReverseIndex -> ReverseIndex
removePackage pkgid deps ri@(ReverseIndex pindex revs nodemap) =
    let index = PackageIndex.deletePackageId pkgid pindex
        rd = delEdges (map (\d -> (nodemap ! d, [nodemap ! pkgid])) deps) revs
    in ri {
        duplicatedIndex = index,
        reverseDependencies = rd,
        packageNodeIdMap = nodemap
    }

changePackage :: PackageId -> [PackageId] -> [PackageId]
              -> ReverseIndex -> ReverseIndex
changePackage pkgid deps deps' ri@(ReverseIndex pindex revs nodemap) =
    let index = PackageIndex.insert pkgid pindex
        npm = Bimap.tryInsert pkgid (Bimap.size nodemap) nodemap
        rd  = delEdges (map (\d -> (npm ! d, [npm ! pkgid])) deps) revs
        rd' = insEdges (map (\d -> (npm ! d, [npm ! pkgid])) deps') rd
    in ri {
        duplicatedIndex = index,
        reverseDependencies = rd',
        packageNodeIdMap = npm
    }

getAllVersions :: Package pkg => PackageIndex pkg -> PackageName -> [Version]
getAllVersions index = map packageVersion . PackageIndex.lookupPackageName index

constructRevDeps :: PackageIndex PkgInfo -> Bimap PackageId NodeId -> RevDeps
constructRevDeps index nodemap =
    let allPackages = concatMap (take 5) $ PackageIndex.allPackagesByName index
        packageIds = map packageId allPackages
        edges = concatMap (\pkg -> map (\dep -> (nodemap ! dep, (nodemap !) . packageId $ pkg)) (getAllDependencies pkg index) ) allPackages
    in Gr.buildG (0, Bimap.size nodemap) edges

getAllDependencies :: Package pkg => PkgInfo -> PackageIndex pkg -> [PackageId]
getAllDependencies pkg index =
    toDepsList (maybeToList $ condLibrary desc) ++ toDepsList (map snd $ condExecutables desc)
    where
        desc = pkgDesc pkg
        toDepsList :: [CondTree v [Dependency] a] -> [PackageId]
        toDepsList l = map (packageId) (concatMap ((PackageIndex.lookupDependency index)) $ concatMap harvestDependencies l)
        -- | Collect all dependencies from all branches of a condition tree.
        harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
        harvestDependencies (CondNode _ deps comps) = deps ++ concatMap forComponent comps
          where forComponent (_, iftree, elsetree) = harvestDependencies iftree ++ maybe [] harvestDependencies elsetree

---------------------------------ReverseDisplay

--------------------------------------------------------------------------------
-- Calculating ReverseDisplays
data ReverseCount = ReverseCount {
    directCount :: Int,
    totalCount :: Int,
    versionCount :: Map Version Int
} deriving (Show, Eq, Typeable, Ord)

instance MemSize ReverseCount where
    memSize (ReverseCount a b c) = memSize3 a b c

type ReverseDisplay = Map PackageName (Version, Maybe VersionStatus)

type VersionIndex = (PackageName -> (PreferredInfo, [Version]))

perPackageReverse :: VersionIndex -> ReverseIndex -> PackageName -> ReverseDisplay
perPackageReverse indexFunc (ReverseIndex index revs nodemap) pkg =
    if any (/= 0) (map (outdeg revs . (nodemap !)) packageids)
        then constructReverseDisplay indexFunc packagemap
        else Map.empty
    where
        packageids = map packageId $ PackageIndex.lookupPackageName index pkg
        packagemap = toPackageMap $ map (nodemap !>) (concatMap (suc revs . (nodemap !)) packageids)

perVersionReverse :: VersionIndex -> ReverseIndex -> PackageId -> ReverseDisplay
perVersionReverse indexFunc (ReverseIndex _ revs nodemap) pkg =
    if (/= 0) (outdeg revs (nodemap ! pkg))
        then constructReverseDisplay indexFunc packagemap
        else Map.empty
    where
        packagemap = toPackageMap $ map (nodemap !>) $ (suc revs . (nodemap !)) pkg

constructReverseDisplay :: VersionIndex -> Map PackageName (Set Version) -> ReverseDisplay
constructReverseDisplay indexFunc deps =
    Map.mapMaybeWithKey (uncurry maybeBestVersion . indexFunc) deps

getDisplayInfo :: PreferredVersions -> PackageIndex PkgInfo -> VersionIndex
getDisplayInfo preferred index pkgname = (,)
    (Map.findWithDefault emptyPreferredInfo pkgname $ preferredMap preferred)
    (map packageVersion . PackageIndex.lookupPackageName index $ pkgname)


------------------------------------ Utility

----------------------------Graph Utility----------
suc :: RevDeps -> Vertex -> [Vertex]
suc g v = g Arr.! v

outdeg :: RevDeps -> Vertex -> Int
outdeg r = length . suc r

insEdges :: [(NodeId, [NodeId])] -> RevDeps -> RevDeps
insEdges edges revdeps =
    Arr.accum (union) revdeps edges

delEdges :: [(NodeId, [NodeId])] -> RevDeps -> RevDeps
delEdges edges revdeps =
    Arr.accum (\\) revdeps edges

revSize :: RevDeps -> Int
revSize rev = snd $ Arr.bounds rev

arrDouble :: RevDeps -> RevDeps
arrDouble rev =
    Gr.buildG (0, 2*(revSize rev)) (Gr.edges rev)
--------------------------------------
countUtil :: [(Version,Int,Int)] -> (Int,Int)
countUtil vcc = foldr (\(_,a, b) (as, bs) -> (a + as, b + bs)) (0,0) vcc

toPackageMap :: [PackageId] -> Map PackageName (Set Version)
toPackageMap assocs = Map.fromListWith (Set.union) [(k, Set.singleton v) | PackageIdentifier k v <- assocs]

toReverseCount :: [(Version, Int , Int)] -> ReverseCount
toReverseCount assocs = ReverseCount (fst count) (snd count) (Map.fromListWith (+) [(k, v) | (k, v, _) <- assocs])
    where
        count = countUtil assocs

-------------------------------------------------------
instance (SafeCopy a, SafeCopy b, Ord a, Ord b) => SafeCopy (Bimap a b) where
    getCopy = contain $ fmap Bimap.fromList safeGet
    putCopy = contain . safePut . Bimap.toList

-- instance (SafeCopy a, SafeCopy b) => SafeCopy (Graph) where
--     putCopy = contain . safePut . Gr.edges
--     getCopy = contain $ fmap (Gr.buildG (0,maxNodes)) safeGet

$(deriveSafeCopy 0 'base ''ReverseIndex)
$(deriveSafeCopy 0 'base ''ReverseCount)

initialReverseIndex :: ReverseIndex
initialReverseIndex = emptyReverseIndex

getReverseIndex :: Query ReverseIndex ReverseIndex
getReverseIndex = ask

replaceReverseIndex :: ReverseIndex -> Update ReverseIndex ()
replaceReverseIndex = put

addReversePackage :: PackageId -> [PackageId] -> Update ReverseIndex [PackageId]
addReversePackage pkgid deps = get >>= \revs ->
    let revs' = addPackage pkgid deps revs
    in put revs' >> return [pkgid]

removeReversePackage :: PackageId -> [PackageId] -> Update ReverseIndex [PackageId]
removeReversePackage pkgid deps = get >>= \revs ->
    let revs' = removePackage pkgid deps revs
    in put revs' >> return [pkgid]

changeReversePackage :: PackageId -> [PackageId] -> [PackageId] -> Update ReverseIndex [PackageId]
changeReversePackage pkgid deps deps' = get >>= \revs ->
    let revs' = changePackage pkgid deps deps' revs
    in put revs' >> return [pkgid]

getDependencies :: PackageName -> Query ReverseIndex (Set PackageName)
getDependencies pkg = do
    ReverseIndex index revdeps nodemap <- ask
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        pkgname p = packageName . (nodemap !>) $ p
    return $ Set.fromList (map (pkgname) (concatMap (suc revdeps . (nodemap !)) packageIds))

getDependenciesI :: PackageName -> Query ReverseIndex (Set PackageName)
getDependenciesI pkg = do
    ReverseIndex index revdeps nodemap <- ask
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        pkgname p = packageName . (nodemap !>) $ p
        reachables = Set.fromList (map (pkgname) (concatMap (\p -> Gr.reachable revdeps (nodemap ! p)) packageIds))
    return $ Set.difference reachables (Set.singleton pkg)


getReverseCount :: PackageName -> Query ReverseIndex ReverseCount
getReverseCount pkg = do
    ReverseIndex index revdeps nodemap <- ask
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        vii = map (\p -> (pkgVersion p, outdeg revdeps (nodemap ! p), length (Gr.reachable revdeps (nodemap ! p) ) - 1)) packageIds
    return  $ toReverseCount vii

getReverseCountId :: PackageId -> Query ReverseIndex (Int,Int)
getReverseCountId pkg = do
    ReverseIndex _ revdeps nodemap <- ask
    return $ (outdeg revdeps (nodemap ! pkg), length (Gr.reachable revdeps (nodemap ! pkg)) - 1)


$(makeAcidic ''ReverseIndex ['getReverseIndex
                            ,'replaceReverseIndex
                            ,'addReversePackage
                            ,'removeReversePackage
                            ,'changeReversePackage
                            ,'getReverseCount
                            ,'getReverseCountId
                            ,'getDependencies
                            ,'getDependenciesI
                          -- ,'getFlattenedReverse
                            ])
