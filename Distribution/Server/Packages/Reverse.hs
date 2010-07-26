{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances, ScopedTypeVariables #-}

module Distribution.Server.Packages.Reverse where

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Preferred
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

import Data.List (foldl')
import Data.Maybe (maybeToList, fromMaybe)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.STRef
import Control.Monad.ST
import Control.Monad.State (put)
import Control.Monad.Reader (ask, asks)

import Happstack.State hiding (Version)
import qualified Happstack.State as State (Version)

-- The main reverse dependencies map is a drawn-out Map PackageId PackageId.
type RevDeps = Map PackageName (Map Version (Map PackageName (Set Version)))
type ReverseDisplay = Map PackageName (Version, Maybe VersionStatus)

data ReverseIndex = ReverseIndex {
    reverseDependencies :: RevDeps,
    flattenedReverse :: Map PackageName (Set PackageName),
    reverseCount :: Map PackageName ReverseCount
} deriving (Show, Eq, Typeable)
emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex Map.empty Map.empty Map.empty

data ReverseCount = ReverseCount {
    directReverseCount :: Int,
    flattenedReverseCount :: Int,
    versionReverseCount :: Map Version Int
} deriving (Show, Eq, Typeable, Ord)
emptyReverseCount :: ReverseCount
emptyReverseCount = ReverseCount 0 0 Map.empty

constructReverseIndex :: PackageIndex PkgInfo -> ReverseIndex
constructReverseIndex index = 
    let deps = constructRevDeps index
    in updateReverseCount $ emptyReverseIndex {
        reverseDependencies = deps,
        flattenedReverse = packageNameClosure deps
    }

updateReverseCount :: ReverseIndex -> ReverseIndex
updateReverseCount index =
    let deps = reverseDependencies index
        flat = flattenedReverse index
    in index {
        reverseCount = flip Map.mapWithKey deps $ \pkg versions -> ReverseCount {
            directReverseCount = Map.size . Map.unions $ Map.elems versions,
            flattenedReverseCount = maybe 0 Set.size $ Map.lookup pkg flat,
            versionReverseCount = Map.map Map.size versions
        }
    }

addPackage :: PackageIndex PkgInfo -> PkgInfo -> ReverseIndex -> ReverseIndex
addPackage index pkg revs =
    let (deps, _) = registerPackage (getAllVersions index) pkg (reverseDependencies revs)
    in updateReverseCount $ revs {
        reverseDependencies = deps,
        flattenedReverse = packageNameClosure deps
    }

removePackage :: PackageIndex PkgInfo -> PkgInfo -> ReverseIndex -> ReverseIndex
removePackage index pkg revs =
    let (deps, _) = unregisterPackage (getAllVersions index) pkg (reverseDependencies revs)
    in updateReverseCount $ revs {
        reverseDependencies = deps,
        flattenedReverse = packageNameClosure deps
    }

--------------------------------------------------------------------------------
-- Managing the RevDeps index.

constructRevDeps :: PackageIndex PkgInfo -> RevDeps
constructRevDeps index = foldl' (\revs pkg -> fst $ registerPackage (getAllVersions index) pkg revs) Map.empty $ PackageIndex.allPackages index

getAllVersions :: PackageIndex PkgInfo -> PackageName -> [Version]
getAllVersions index = map packageVersion . PackageIndex.lookupPackageName index

-- FIXME: this is somewhat broken when a new version of a package is uploaded. The
-- reason is that packages that are registered earlier have dependencies on versions
-- that might not be added to the index later on. This is one of the pains of incremental
-- updates.
-- One possible fix:
-- type RevDeps = Map PackageName (Map PackageName (Map Version VersionRange))
-- An entry (pkgname, (pkgname', (version, range))) means that pkgname'-version
-- depends on all versions of pkgname within range. Just another way of organizing
-- the data. The resulting structure is less fragile though.

-- | Given a package id, modify the entries of the package's dependencies in
-- the reverse dependencies mapping to include it.
registerPackage :: (PackageName -> [Version]) -> PkgInfo -> RevDeps -> (RevDeps, Map PackageName [Version])
registerPackage getVersions pkg revs = (,) =<< (foldl' goRegister revs . Map.toList) $ deps
  where
    PackageIdentifier name version = packageId pkg
    deps = getLinkedNodes getVersions pkg
    goRegister prev (pkgname, versions) =
        let revPackage = Map.fromList $ map (\v -> (v, Map.singleton name $ Set.singleton version)) versions
        in  Map.insertWith (\new old -> Map.unionWith (Map.unionWith Set.union) old new) pkgname revPackage prev
    -- docs: Hedge-union is more efficient on (bigset `union` smallset). 

-- | Given a package id, modify the entries of the package's dependencies in
-- the reverse dependencies mapping to exclude it.
unregisterPackage :: (PackageName -> [Version]) -> PkgInfo -> RevDeps -> (RevDeps, Map PackageName [Version])
unregisterPackage getVersions pkg revs = (,) =<< (foldl' goUnregister revs . Map.toList) $ deps
  where
    PackageIdentifier name version = packageId pkg
    deps = getLinkedNodes getVersions pkg
    goUnregister prev (pkgname, versions) =
        let revPackage = Map.fromList $ map (\v -> (v, Map.singleton name $ Set.singleton version)) versions
        in  Map.differenceWith (\a b ->
                keepMap $ Map.differenceWith (\c d ->
                    keepMap $ Map.differenceWith (\e f ->
                        keepSet $ Set.difference e f)
                    c d)
                a b) prev (Map.singleton pkgname revPackage)

--------------------------------------------------------------------------------
-- Calculating dependencies and selecting versions

-- | Given a package id, return all packages in the index that depend on it.
-- For all such packages, return the specific versions that satisfy the
-- dependency as indicated in the cabal file.
getLinkedNodes :: (PackageName -> [Version]) -> PkgInfo -> Map PackageName [Version]
getLinkedNodes getVersions pkg =
    Map.mapWithKey (selectVersions . getVersions)
  $ getAllDependencies pkg

-- | Given a dependency (a package name and a version range), find all versions
-- in the current package index that satisfy it.
selectVersions :: [Version] -> VersionRange -> [Version]
selectVersions versions range = filter (flip withinRange range) versions

-- | Collect all dependencies specified in a package's cabal file, considering
-- all alternatives and unioning them together.
getAllDependencies :: PkgInfo -> Map PackageName VersionRange
getAllDependencies pkg = 
    let desc  = pkgDesc pkg
    in Map.fromListWith unionVersionRanges $ toDepsList (maybeToList $ condLibrary desc)
                                          ++ toDepsList (map snd $ condExecutables desc)
  where toDepsList :: [CondTree v [Dependency] a] -> [(PackageName, VersionRange)]
        toDepsList = map (\(Dependency p v) -> (p, v)) . concatMap harvestDependencies

-- | Collect all dependencies from all branches of a condition tree.
harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
harvestDependencies (CondNode _ deps comps) = deps ++ concatMap forComponent comps
  where forComponent (_, iftree, elsetree) = harvestDependencies iftree ++ maybe [] harvestDependencies elsetree


--------------------------------------------------------------------------------
-- Calculating ReverseDisplays

-- putStrLn $ showRevDisplay $ perVersionReverse (PackageIdentifier (PackageName "monoids") (Version [0,9] [])) ps (emptyPreferredVersions) u
-- u Map.! (PackageName "happstack-server") Map.! (Version [0,4,1] [])
-- let u = constructRevDeps ps
-- ps <- getPs
-- tx <- ctrl
type VersionIndex = (PackageName -> (PreferredInfo, [Version]))

perPackageReverse :: VersionIndex -> RevDeps -> PackageName -> ReverseDisplay
perPackageReverse indexFunc revs pkg = case Map.lookup pkg revs of
    Nothing   -> Map.empty
    Just dict -> constructReverseDisplay indexFunc (Map.unionsWith Set.union $ Map.elems dict)

perVersionReverse :: VersionIndex -> RevDeps -> PackageId -> ReverseDisplay
perVersionReverse indexFunc revs pkg = case Map.lookup (packageVersion pkg) =<< Map.lookup (packageName pkg) revs of
    Nothing   -> Map.empty
    Just dict -> constructReverseDisplay indexFunc dict

constructReverseDisplay :: VersionIndex -> Map PackageName (Set Version) -> ReverseDisplay
constructReverseDisplay indexFunc deps =
    Map.mapMaybeWithKey (uncurry maybeBestVersion . indexFunc) deps

getDisplayInfo :: PreferredVersions -> PackageIndex PkgInfo -> VersionIndex
getDisplayInfo preferred index pkgname = (,)
    (Map.findWithDefault emptyPreferredInfo pkgname $ preferredMap preferred)
    (map packageVersion . PackageIndex.lookupPackageName index $ pkgname)

--------------------------------------------------------------------------------
-- Keeping a cached map of selected versions.
--
-- Currently it's rather quick to calculate each, and the majority of processing
-- will probably go towards rendering it in HTML/JSON/whathaveyou.
--
-- Still, in a future Hackage where preferred-versions and deprecated versions
-- are *very* widely used, incremental updates of an display index might become
-- necessary. In such a future, there would be two maps in the ReverseIndex
-- structure, both to ReverseDisplay from PackageName and PackageId. They would
-- need to be updated with updatePackageReverse and updateVersionReverse,
-- respectively, whenever a package is added, removed, or has its preferred info
-- changed.

constructPackageReverse :: VersionIndex -> RevDeps -> Map PackageName ReverseDisplay
constructPackageReverse indexFunc revs =
    Map.fromList $ do
        pkg <- Map.keys revs
        rev <- maybeToList . keepMap $ perPackageReverse indexFunc revs pkg
        return (pkg, rev)

constructVersionReverse :: VersionIndex -> RevDeps -> Map PackageId ReverseDisplay
constructVersionReverse indexFunc revs = 
    Map.fromList $ do
        pkg <- getNodes =<< Map.toList revs
        rev <- maybeToList . keepMap $ perVersionReverse indexFunc revs pkg
        return (pkg, rev)
  where
    getNodes :: (PackageName, Map Version a) -> [PackageId]
    getNodes (name, versions) = map (PackageIdentifier name) $ Map.keys versions

-- | With a package which has just been updated, make sure the version displayed
-- in its reverse display is the most recent. To do this, each of its dependencies
-- needs its ReverseDisplay updated.
updateReverseDisplay :: VersionIndex -> PackageName -> Set Version -> ReverseDisplay -> ReverseDisplay
updateReverseDisplay indexFunc pkgname versions revDisplay =
    let toVersions = uncurry maybeBestVersion . indexFunc
    in case toVersions pkgname versions of
        Nothing -> revDisplay
        Just status -> Map.insert pkgname status revDisplay

-- If the RevDeps index is modified through registering/unregistering packages,
-- updatePackageReverse and updateVersionReverse should be given a list of
-- package names/package ids distrilled from the resultant (Map PackageName
-- [Version]). The idea is to sync it with the just-updated RevDeps.
--
-- If a package's PreferredVersions are modified, these functions should be
-- called with the same information taken from the getLinkedNodes function.
-- In this case, the RevDeps data structure hasn't changed.

updatePackageReverse :: VersionIndex -> PackageName -> [PackageName] -> RevDeps -> Map PackageName ReverseDisplay -> Map PackageName ReverseDisplay
updatePackageReverse indexFunc updated deps revs nameMap = 
    foldl' (\revd pkg -> Map.alter (alterRevDisplay pkg . fromMaybe Map.empty) pkg revd) nameMap deps
  where
    lookupVersions :: PackageName -> Set Version
    lookupVersions pkgname = maybe Set.empty (Set.unions . map (Map.findWithDefault Set.empty $ updated) . Map.elems) $ Map.lookup pkgname revs
    alterRevDisplay :: PackageName -> ReverseDisplay -> Maybe ReverseDisplay
    alterRevDisplay pkgname rev = keepMap $ updateReverseDisplay indexFunc updated (lookupVersions pkgname) rev

updateVersionReverse :: VersionIndex -> PackageName -> [PackageId] -> RevDeps -> Map PackageId ReverseDisplay -> Map PackageId ReverseDisplay
updateVersionReverse indexFunc updated deps revs pkgMap =
    foldl' (\revd pkg -> Map.alter (alterRevDisplay pkg . fromMaybe Map.empty) pkg revd) pkgMap deps
  where
    lookupVersions :: PackageId -> Set Version
    lookupVersions pkgid = maybe Set.empty (Map.findWithDefault Set.empty updated) $ Map.lookup (packageVersion pkgid) =<< Map.lookup (packageName pkgid) revs
    alterRevDisplay :: PackageId -> ReverseDisplay -> Maybe ReverseDisplay
    alterRevDisplay pkgid rev = keepMap $ updateReverseDisplay indexFunc updated (lookupVersions pkgid) rev

--------------------------------------------------------------------------------
-- Flattening the graph

-- Collect all indirect versioned dependencies. This takes around 45 seconds
-- on the current package index (well, in ghci). It probably isn't worth exposing.
packageIdClosure :: RevDeps -> Map PackageId (Set PackageId)
packageIdClosure revs = Map.fromDistinctAscList $ transitiveClosure
    (concatMap getNodes $ Map.toList revs)
    (\pkg -> maybe [] (concatMap getEdges . Map.toList)
           $ Map.lookup (packageVersion pkg) =<< Map.lookup (packageName pkg) revs)
  where
    getNodes :: (PackageName, Map Version a) -> [PackageId]
    getNodes (name, versions) = map (PackageIdentifier name) $ Map.keys versions

    getEdges :: (PackageName, Set Version) -> [PackageId]
    getEdges (name, versions) = map (PackageIdentifier name) $ Set.toList versions

-- Collect all indirect name dependencies. This takes around 2 seconds on the
-- current package index. It should be fine to reconstruct from scratch every
-- time the package index is updated, since code to incrementally update a
-- transitive closure can be messy and stateful and complicated.
packageNameClosure :: RevDeps -> Map PackageName (Set PackageName)
packageNameClosure revs = Map.fromDistinctAscList $ transitiveClosure
    (Map.keys revs)
    (\pkg -> maybe [] (concatMap Map.keys . Map.elems)
           $ Map.lookup pkg revs)

-- Get the transitive closure of a graph from the set of nodes and a neighbor
-- function. This implementation uses depth-first search in the ST monad
-- where cycles are broken if a visited node has been seen before.
--
-- The same basic algorithm could be used to make a DAG structure.
transitiveClosure :: forall a. Ord a => [a] -> (a -> [a]) -> [(a, Set a)]
transitiveClosure core edges = runST $ do
    list <- mapM (\node -> newSTRef Nothing >>= \ref -> return (node, ref)) core
    let visited = Map.fromList list
    mapM_ (collect visited) core
    list' <- mapM (\(node, ref) -> readSTRef ref >>= \val -> return (node, val)) list
    return [ (node, nodes) | (node, Just nodes) <- list' ]
  where
    collect :: Map a (STRef s (Maybe (Set a))) -> a -> ST s (Set a)
    collect links node = do
        case Map.lookup node links of
            -- attempting to visit a node which wasn't given to us
            Nothing  -> return Set.empty
            Just ref -> readSTRef ref >>= \t -> case t of
                -- the node has already been visited
                Just calc -> return calc
                Nothing   -> do
                    -- Mark the node as visited with results pending. If a cycle
                    -- brings us back to collect the same node, it will yield
                    -- an empty list. This breaks the cycle for whatever node
                    -- was visited first.
                    --
                    -- There are smarter algorithms that can
                    -- get transitive closures of cyclic graphs, but cycles are
                    -- highly pathological for the types of graphs we'll be
                    -- traversing, so no need to worry.
                    writeSTRef ref $ Just Set.empty
                    let outEdges = edges node
                    collected <- fmap Set.unions $ mapM (collect links) outEdges
                    let connected = Set.union (Set.fromList outEdges) collected
                    writeSTRef ref $ Just connected
                    return connected

------------------------------------ Utility
-- For cases when, if a Map or Set is empty, it's as good as nothing at all.
keepMap :: Ord k => Map k a -> Maybe (Map k a)
keepMap con = if Map.null con then Nothing else Just con

keepSet :: Ord a => Set a -> Maybe (Set a)
keepSet con = if Set.null con then Nothing else Just con

--------------------------------------------------------------------------------
-- State
--
-- Last but not least, methods for manipulating the global state.

instance State.Version ReverseIndex where mode = Versioned 0 Nothing
$(deriveSerialize ''ReverseIndex)
instance State.Version ReverseCount where mode = Versioned 0 Nothing
$(deriveSerialize ''ReverseCount)

instance Component ReverseIndex where
    type Dependencies ReverseIndex = End
    initialValue = emptyReverseIndex

getReverseIndex :: Query ReverseIndex ReverseIndex
getReverseIndex = ask

replaceReverseIndex :: ReverseIndex -> Update ReverseIndex ()
replaceReverseIndex = put

getReverseCount :: PackageName -> Query ReverseIndex ReverseCount
getReverseCount pkg = asks $ Map.findWithDefault emptyReverseCount pkg . reverseCount

getFlattenedReverse :: PackageName -> Query ReverseIndex (Set PackageName)
getFlattenedReverse pkg = asks $ Map.findWithDefault Set.empty pkg . flattenedReverse

$(mkMethods ''ReverseIndex ['getReverseIndex
                           ,'replaceReverseIndex
                           ,'getReverseCount
                           ,'getFlattenedReverse
                           ])

