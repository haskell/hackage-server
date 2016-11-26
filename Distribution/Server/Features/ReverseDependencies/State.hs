{-# LANGUAGE RankNTypes, DeriveGeneric, TypeSynonymInstances, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.ReverseDependencies.State where

import Distribution.Server.Packages.Types
import Distribution.Server.Framework.MemSize
import Distribution.Server.Features.PreferredVersions.State
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

import Data.Acid (Query, Update, makeAcidic)
import Data.List (union, (\\))
import Data.Maybe (maybeToList)
import Data.SafeCopy hiding (Version)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bimap (Bimap, (!>), (!), member)
import qualified Data.Bimap as Bimap
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Gr
import qualified Data.Array as Arr ((!), accum, assocs, accumArray)

type NodeId = Int
type RevDeps = Graph

data ReverseIndex = ReverseIndex {
    reverseDependencies :: !RevDeps,
    packageNodeIdMap    :: !(Bimap PackageId NodeId)
} deriving (Eq, Show, Typeable)

instance MemSize ReverseIndex where
    memSize (ReverseIndex a b) = memSize2 a b


emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex (Gr.buildG (0,-1) []) Bimap.empty

constructReverseIndex :: PackageIndex PkgInfo -> ReverseIndex
constructReverseIndex index =
    ReverseIndex {
        reverseDependencies = constructRevDeps index nodePkgMap,
        packageNodeIdMap = nodePkgMap
    }
    where
        nodePkgMap = foldr (uncurry Bimap.insert) Bimap.empty $ zip (map packageId (PackageIndex.allPackages index)) [0..]

addPackage :: PackageId -> [PackageId]
           -> ReverseIndex -> ReverseIndex
addPackage pkgid deps ri@(ReverseIndex revs nodemap) =
    let npm = Bimap.tryInsert pkgid (Bimap.size nodemap) nodemap
        rd = insEdges (Bimap.size npm) (map (\d -> (npm ! d, [npm ! pkgid])) deps) revs
    in ri {
        reverseDependencies = rd,
        packageNodeIdMap = npm
    }

removePackage :: PackageId -> [PackageId]
              -> ReverseIndex -> ReverseIndex
removePackage pkgid deps ri@(ReverseIndex revs nodemap) =
    let rd = delEdges (map (\d -> (nodemap ! d, [nodemap ! pkgid])) deps) revs
    in ri {
        reverseDependencies = rd,
        packageNodeIdMap = nodemap
    }

changePackage :: PackageId -> [PackageId] -> [PackageId]
              -> ReverseIndex -> ReverseIndex
changePackage pkgid deps deps' ri@(ReverseIndex revs nodemap) =
    let
        npm = Bimap.tryInsert pkgid (Bimap.size nodemap) nodemap
        rd  = delEdges (map (\d -> (npm ! d, [npm ! pkgid])) deps) revs
        rd' = insEdges (Bimap.size npm) (map (\d -> (npm ! d, [npm ! pkgid])) deps') rd
    in ri {
        reverseDependencies = rd',
        packageNodeIdMap = npm
    }

getAllVersions :: Package pkg => PackageIndex pkg -> PackageName -> [Version]
getAllVersions index = map packageVersion . PackageIndex.lookupPackageName index

constructRevDeps :: PackageIndex PkgInfo -> Bimap PackageId NodeId -> RevDeps
constructRevDeps index nodemap =
    let allPackages = concat {- concatMap (take 5) -} $ PackageIndex.allPackagesByName index
        edges = concatMap (\pkg -> map (\dep -> (nodemap ! dep, (nodemap !) . packageId $ pkg)) (getAllDependencies pkg index) ) allPackages
    in Gr.buildG (0, Bimap.size nodemap) edges

getAllDependencies :: Package pkg => PkgInfo -> PackageIndex pkg -> [PackageId]
getAllDependencies pkg index =
    toDepsList (maybeToList $ condLibrary desc) ++ toDepsList (map snd $ condExecutables desc)
    where
        desc = pkgDesc pkg
        toDepsList :: [CondTree v [Dependency] a] -> [PackageId]
        toDepsList l = map packageId (concatMap (PackageIndex.lookupDependency index) $ concatMap harvestDependencies l)
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

perPackageReverse :: VersionIndex -> PackageIndex PkgInfo -> ReverseIndex -> PackageName -> ReverseDisplay
perPackageReverse indexFunc index (ReverseIndex revs nodemap) pkg =
    if any (/= 0) (map (outdeg revs . (nodemap !)) packageids)
        then constructReverseDisplay indexFunc packagemap
        else Map.empty
    where
        packageids = map packageId $ PackageIndex.lookupPackageName index pkg
        packagemap = toPackageMap $ map (nodemap !>) (concatMap (suc revs . (nodemap !)) packageids)

perVersionReverse :: VersionIndex -> ReverseIndex -> PackageId -> ReverseDisplay
perVersionReverse indexFunc (ReverseIndex revs nodemap) pkg =
    if (/= 0) (outdeg revs (nodemap ! pkg))
        then constructReverseDisplay indexFunc packagemap
        else Map.empty
    where
        packagemap = toPackageMap $ map (nodemap !>) $ (suc revs . (nodemap !)) pkg

constructReverseDisplay :: VersionIndex -> Map PackageName (Set Version) -> ReverseDisplay
constructReverseDisplay indexFunc =
    Map.mapMaybeWithKey (uncurry maybeBestVersion . indexFunc)

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

insEdges :: Int -> [(NodeId, [NodeId])] -> RevDeps -> RevDeps
insEdges nodesize edges revdeps = Arr.accumArray union [] (0, nodesize) (edges ++ Arr.assocs revdeps)

delEdges :: [(NodeId, [NodeId])] -> RevDeps -> RevDeps
delEdges edges revdeps =
    Arr.accum (\\) revdeps edges

--------------------------------------
countUtil :: [(Version, Set PackageName, Int)] -> (Int, Int)
countUtil = (\(x,y) -> (Set.size x, y)) . foldr (\(_,a, b) (as, bs) -> (Set.union a as, b + bs)) (Set.empty,0)

toPackageMap :: [PackageId] -> Map PackageName (Set Version)
toPackageMap assocs = Map.fromListWith Set.union [(k, Set.singleton v) | PackageIdentifier k v <- assocs]

toReverseCount :: [(Version, Set PackageName , Int)] -> ReverseCount
toReverseCount assocs = uncurry ReverseCount count (Map.fromListWith (+) [(k, Set.size v) | (k, v, _) <- assocs])
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

getReverseIndex :: Query ReverseIndex ReverseIndex
getReverseIndex = ask

replaceReverseIndex :: ReverseIndex -> Update ReverseIndex ()
replaceReverseIndex = put

addReversePackage :: PackageId -> [PackageId] -> Update ReverseIndex ()
addReversePackage pkgid deps = get >>= \revs ->
    let revs' = addPackage pkgid deps revs
    in put revs'

removeReversePackage :: PackageId -> [PackageId] -> Update ReverseIndex ()
removeReversePackage pkgid deps = get >>= \revs ->
    let revs' = removePackage pkgid deps revs
    in put revs'

changeReversePackage :: PackageId -> [PackageId] -> [PackageId] -> Update ReverseIndex ()
changeReversePackage pkgid deps deps' = get >>= \revs ->
    let revs' = changePackage pkgid deps deps' revs
    in put revs'

getDependencies :: PackageName -> PackageIndex PkgInfo -> Query ReverseIndex (Set PackageName)
getDependencies pkg index = getDependencies' pkg index <$> ask

getDependencies' :: PackageName -> PackageIndex PkgInfo -> ReverseIndex -> Set PackageName
getDependencies' pkg index (ReverseIndex revdeps nodemap) =
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        pkgname = packageName . (nodemap !>)
    in Set.fromList (map pkgname (concatMap (suc revdeps . (nodemap !)) packageIds))

getDependenciesI :: PackageName -> PackageIndex PkgInfo -> Query ReverseIndex (Set PackageName)
getDependenciesI pkg index = getDependenciesI' pkg index <$> ask

getDependenciesI' :: PackageName -> PackageIndex PkgInfo -> ReverseIndex -> Set PackageName
getDependenciesI' pkg index (ReverseIndex revdeps nodemap) =
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        pkgname = packageName . (nodemap !>)
        reachables = Set.fromList . map pkgname . concatMap (\p -> Gr.reachable revdeps (nodemap ! p)) . filter (`member` nodemap) $ packageIds
    in Set.difference reachables (Set.singleton pkg)

getReverseCount :: PackageName -> PackageIndex PkgInfo -> Query ReverseIndex ReverseCount
getReverseCount pkg index = getReverseCount' pkg index <$> ask

getReverseCount' :: PackageName -> PackageIndex PkgInfo -> ReverseIndex -> ReverseCount
getReverseCount' pkg index (ReverseIndex revdeps nodemap) =
    let packageIds = map packageId $ PackageIndex.lookupPackageName index pkg
        pkgname = packageName . (nodemap !>)
        vii = map (\p -> (pkgVersion p, Set.fromList . map pkgname . suc revdeps $ (nodemap ! p), length (Gr.reachable revdeps (nodemap ! p) ) - 1)) (filter (`member` nodemap) packageIds)
    in toReverseCount vii

getReverseCountId :: PackageId -> Query ReverseIndex (Int,Int)
getReverseCountId pkg = getReverseCountId' pkg <$> ask

getReverseCountId' :: PackageId -> ReverseIndex -> (Int, Int)
getReverseCountId' pkg (ReverseIndex revdeps nodemap)
    | pkg `member` nodemap = (outdeg revdeps (nodemap ! pkg), length (Gr.reachable revdeps (nodemap ! pkg)) - 1)
    | otherwise = (0,0)


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
