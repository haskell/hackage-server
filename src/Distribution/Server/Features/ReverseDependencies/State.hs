{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.ReverseDependencies.State where

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State ()
import Distribution.Server.Packages.Preferred
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version

import Data.Acid (Query, Update, makeAcidic)
import Data.List (foldl', union)
import Data.Maybe (maybeToList, fromMaybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.STRef
import Control.Monad.ST
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask, asks)

-- The main reverse dependencies map is a drawn-out Map PackageId PackageId,
-- with an extra component to store ranges (all ranges that *could* be revdeps,
-- even if no packages in the index currently satisfy those ranges).
--
-- For selected entries of the map (foo, (2.0, (bar, [1.0]), (bar, (1.0, <3)))):
-- This means that bar-1.0 depends on a version of foo <3, and foo 2.0 meets this criterion.
type RevDeps = Map PackageName (Map Version (Map PackageName (Set Version)), Map PackageName (Map Version VersionRange))

type CombinedDeps = Map PackageName VersionRange

-- TODO: should this be (Maybe (Version, Maybe VersionStatus))?
-- it should be possible, albeit with a bit more work, to determine all revdeps
-- of a package which don't have any versions presently satisfying them.
-- (for an entry (a, b) of RevDeps, take (union a \ b))
type ReverseDisplay = Map PackageName (Version, Maybe VersionStatus)

data ReverseIndex = ReverseIndex {
    -- this field is a duplication of PackageIndex, so updates don't have
    -- to use much information outside of this component, resulting in huge
    -- happstack-state event files
    duplicatedIndex :: PackageIndex PackageId,

    -- The main reverse dependencies map
    reverseDependencies :: RevDeps,

    -- Generated from packageNameClosure.
    flattenedReverse :: Map PackageName (Set PackageName),

    -- Cached counts for reverse dependencies.
    reverseCount :: Map PackageName ReverseCount
} deriving (Eq, Show, Typeable)

emptyReverseIndex :: ReverseIndex
emptyReverseIndex = ReverseIndex (PackageIndex.fromList [])
                                 Map.empty Map.empty Map.empty

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
        duplicatedIndex = constructDupIndex index,
        reverseDependencies = deps,
        flattenedReverse = packageNameClosure deps
    }

constructDupIndex :: PackageIndex PkgInfo -> PackageIndex PackageId
constructDupIndex = PackageIndex.fromList
  . map packageId
  . PackageIndex.allPackages

updateReverseCount :: ReverseIndex -> ReverseIndex
updateReverseCount index =
    let deps = reverseDependencies index
        flat = flattenedReverse index
    in index {
        reverseCount = flip Map.mapWithKey deps $ \pkg (versions, _) -> ReverseCount {
            directReverseCount = Map.size . Map.unions $ Map.elems versions,
            flattenedReverseCount = maybe 0 Set.size $ Map.lookup pkg flat,
            versionReverseCount = Map.map Map.size versions
        }
    }

addPackage :: PackageId -> CombinedDeps
           -> ReverseIndex -> (ReverseIndex, Map PackageName [Version])
addPackage pkgid deps revs =
    let index = PackageIndex.insert pkgid (duplicatedIndex revs)
        (rd, rec) = registerPackage (getAllVersions index) pkgid deps (reverseDependencies revs)
    in (updateReverseCount $ revs {
        duplicatedIndex = index,
        reverseDependencies = rd,
        flattenedReverse = packageNameClosure rd
    }, rec)

removePackage :: PackageId -> CombinedDeps
              -> ReverseIndex -> (ReverseIndex, Map PackageName [Version])
removePackage pkgid deps revs =
    let index = PackageIndex.deletePackageId pkgid (duplicatedIndex revs)
        (rd, rec) = unregisterPackage (getAllVersions index) pkgid deps (reverseDependencies revs)
    in (updateReverseCount $ revs {
        duplicatedIndex = index,
        reverseDependencies = rd,
        flattenedReverse = packageNameClosure rd
    }, rec)

changePackage :: PackageId -> CombinedDeps -> CombinedDeps
              -> ReverseIndex -> (ReverseIndex, Map PackageName [Version])
changePackage pkgid deps deps' revs =
    let index = PackageIndex.insert pkgid (duplicatedIndex revs)
        allVersions = getAllVersions index
        (rd,  rec)  = unregisterPackage allVersions pkgid deps (reverseDependencies revs)
        (rd', rec') = registerPackage allVersions pkgid deps' rd
    in (updateReverseCount $ revs {
        duplicatedIndex = index,
        reverseDependencies = rd',
        flattenedReverse = packageNameClosure rd'
    }, Map.unionWith union rec rec')
--------------------------------------------------------------------------------
-- Managing the RevDeps index.

constructRevDeps :: PackageIndex PkgInfo -> RevDeps
constructRevDeps index = foldl' (\revs pkg -> fst $ registerPackage (getAllVersions index) (packageId pkg) (getAllDependencies pkg) revs) Map.empty $ PackageIndex.allPackages index

getAllVersions :: Package pkg => PackageIndex pkg -> PackageName -> [Version]
getAllVersions index = map packageVersion . PackageIndex.lookupPackageName index

-- | Given a package id, modify the entries of the package's dependencies in
-- the reverse dependencies mapping to include it.
registerPackage :: (PackageName -> [Version])
                -> PackageId -> CombinedDeps
                -> RevDeps -> (RevDeps, Map PackageName [Version])
registerPackage getVersions (PackageIdentifier name version) ranges revs =
    let deps = getLinkedNodes getVersions ranges
        revs'  = foldl' goRegister revs $ Map.toList $ Map.intersectionWith (,) ranges deps
        revs'' = backtrace revs'
    in (revs'', deps)
  where
    pkgMap = Map.singleton name $ Set.singleton version
    -- this takes each of the registered packages dependencies and puts an entry
    -- of it there. e.g. a new version of base would not have much work to do
    -- here because it has no dependencies
    goRegister prev (pkgname, (range, versions)) =
        -- revPackage encodes the dependency (name -> pkgname) in a way that can
        -- be inserted into pkgname's reverse dependency mapping
        let revPackage = Map.fromList $ map (\v -> (v, pkgMap)) versions
            revRange   = Map.singleton name (Map.singleton version range)
        in  Map.insertWith (\(small, small') (big, big') -> (Map.unionWith (Map.unionWith Set.union) big small, Map.unionWith (flip const) big' small')) pkgname (revPackage, revRange) prev
    -- this uses the package's existing reverse dependencies, regardless of
    -- version, to find reverse dependencies for this version in particular.
    -- e.g. a new version of base would have to recalculate the dependencies
    -- of nearly all of the packages in the index
    backtrace prev = case Map.lookup name prev of
        Nothing -> prev
        Just (vs, rs) ->
            let revVersion = Map.map (Set.fromList . map fst . filter (withinRange version . snd) . Map.toList) rs
            in  Map.insert name (Map.insertWith (\new old -> Map.unionWith Set.union old new) version revVersion vs, rs) prev

-- | Given a package id, modify the entries of the package's dependencies in
-- the reverse dependencies mapping to exclude it.
unregisterPackage :: (PackageName -> [Version])
                  -> PackageId -> CombinedDeps
                  -> RevDeps -> (RevDeps, Map PackageName [Version])
unregisterPackage getVersions (PackageIdentifier name version) ranges revs =
    let deps = getLinkedNodes getVersions ranges
        revs'  = foldl' goUnregister revs $ Map.toList $ Map.intersectionWith (,) ranges deps
        revs'' = backtrace revs'
    in (revs'', deps)
  where
    pkgMap = Map.singleton name $ Set.singleton version
    goUnregister prev (pkgname, (range, versions)) =
        let revPackage = Map.fromList $ map (\v -> (v, pkgMap)) versions
            revRange   = Map.singleton pkgname (Map.fromList $ map (\v -> (v, range)) versions)
        -- there are possibly better ways to go about this
        in  Map.differenceWith (\(a, b) (c, d) -> keepMaps $
              ( Map.differenceWith (\e f ->
                    keepMap $ Map.differenceWith (\g h ->
                        keepSet $ Set.difference g h)
                    e f)
                a c
              , Map.differenceWith (\e f ->
                    keepMap $ Map.difference e f)
                b d
              )) prev (Map.singleton pkgname (revPackage, revRange))
    backtrace prev = Map.update (\(vs, rs) -> keepMaps (Map.delete version vs, rs)) name prev

--------------------------------------------------------------------------------
-- Calculating dependencies and selecting versions

-- | Given a package, determine the packages on which it depends.
-- For all such packages, return the specific versions that satisfy the
-- dependency as indicated in the cabal file.
getLinkedNodes :: (PackageName -> [Version])
               -> CombinedDeps -> Map PackageName [Version]
getLinkedNodes getVersions pkgs = Map.mapWithKey (\pkg range -> selectVersions range $ getVersions pkg) pkgs

-- | Given a dependency (a package name and a version range), find all versions
-- in the current package index that satisfy it.
selectVersions :: VersionRange -> [Version] -> [Version]
selectVersions range versions= filter (flip withinRange range) versions

-- | Collect all dependencies specified in a package's cabal file, considering
-- all alternatives.
--
-- This unions all version ranges together from different branches, which is
-- imprecise but mostly good enough (a slightly better heuristic might be
-- intersecting within a block, but unioning blocks together).
getAllDependencies :: PkgInfo -> CombinedDeps
getAllDependencies pkg =
    let desc = pkgDesc pkg
    in Map.fromListWith unionVersionRanges $ toDepsList (maybeToList $ condLibrary desc)
                                          ++ toDepsList (map snd $ condExecutables desc)
  where toDepsList :: [CondTree v [Dependency] a] -> [(PackageName, VersionRange)]
        toDepsList l = [ (p, v) | Dependency p v <- concatMap harvestDependencies l ]

-- | Collect all dependencies from all branches of a condition tree.
harvestDependencies :: CondTree v [Dependency] a -> [Dependency]
harvestDependencies (CondNode _ deps comps) = deps ++ concatMap forComponent comps
  where forComponent (_, iftree, elsetree) = harvestDependencies iftree ++ maybe [] harvestDependencies elsetree

--------------------------------------------------------------------------------
-- Calculating ReverseDisplays
type VersionIndex = (PackageName -> (PreferredInfo, [Version]))

-- TODO: this should use the secondary PackageId -> VersionRange mapping in RevDeps,
-- so it gets all possible versions, not just those currently in the index.
perPackageReverse :: VersionIndex -> RevDeps -> PackageName -> ReverseDisplay
perPackageReverse indexFunc revs pkg = case Map.lookup pkg revs of
    Nothing   -> Map.empty
    Just (dict, _) -> constructReverseDisplay indexFunc (Map.unionsWith Set.union $ Map.elems dict)

perVersionReverse :: VersionIndex -> RevDeps -> PackageId -> ReverseDisplay
perVersionReverse indexFunc revs pkg = case Map.lookup (packageVersion pkg) . fst =<< Map.lookup (packageName pkg) revs of
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
-- will probably go towards rendering it in HTML/JSON/whathaveyou anyway. So this
-- is not used.
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
    getNodes :: (PackageName, (Map Version a, b)) -> [PackageId]
    getNodes (name, (versions, _)) = map (PackageIdentifier name) $ Map.keys versions

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
    lookupVersions pkgname = maybe Set.empty (Set.unions . map (Map.findWithDefault Set.empty $ updated) . Map.elems . fst) $ Map.lookup pkgname revs
    alterRevDisplay :: PackageName -> ReverseDisplay -> Maybe ReverseDisplay
    alterRevDisplay pkgname rev = keepMap $ updateReverseDisplay indexFunc updated (lookupVersions pkgname) rev

updateVersionReverse :: VersionIndex -> PackageName -> [PackageId] -> RevDeps -> Map PackageId ReverseDisplay -> Map PackageId ReverseDisplay
updateVersionReverse indexFunc updated deps revs pkgMap =
    foldl' (\revd pkg -> Map.alter (alterRevDisplay pkg . fromMaybe Map.empty) pkg revd) pkgMap deps
  where
    lookupVersions :: PackageId -> Set Version
    lookupVersions pkgid = maybe Set.empty (Map.findWithDefault Set.empty updated) $ Map.lookup (packageVersion pkgid) . fst =<< Map.lookup (packageName pkgid) revs
    alterRevDisplay :: PackageId -> ReverseDisplay -> Maybe ReverseDisplay
    alterRevDisplay pkgid rev = keepMap $ updateReverseDisplay indexFunc updated (lookupVersions pkgid) rev

--------------------------------------------------------------------------------
-- Flattening the graph
-- Exposing indirect dependencies is as simple as taking the set difference
-- of the edges of a node in the dependency graph G and its closure G+.

-- Collect all indirect versioned dependencies. This takes around 45 seconds
-- on the current package index (in ghci). It probably isn't worth exposing.
packageIdClosure :: RevDeps -> Map PackageId (Set PackageId)
packageIdClosure revs = Map.fromDistinctAscList $ transitiveClosure
    (concatMap getNodes $ Map.toList revs)
    (\pkg -> maybe [] (concatMap getEdges . Map.toList)
           $ Map.lookup (packageVersion pkg) . fst =<< Map.lookup (packageName pkg) revs)
  where
    getNodes :: (PackageName, (Map Version a, b)) -> [PackageId]
    getNodes (name, (versions, _)) = map (PackageIdentifier name) $ Map.keys versions

    getEdges :: (PackageName, Set Version) -> [PackageId]
    getEdges (name, versions) = map (PackageIdentifier name) $ Set.toList versions

-- Collect all indirect name dependencies. This takes around 2 seconds on the
-- current package index. It should be fine to reconstruct from scratch every
-- time the package index is updated, since code to incrementally update a
-- transitive closure can be messy and stateful and complicated.
packageNameClosure :: RevDeps -> Map PackageName (Set PackageName)
packageNameClosure revs = Map.fromDistinctAscList $ transitiveClosure
    (Map.keys revs)
    (\pkg -> maybe [] (concatMap Map.keys . Map.elems . fst)
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

keepMaps :: (Ord k, Ord k') => (Map k a, Map k' b) -> Maybe (Map k a, Map k' b)
keepMaps con@(c, c') = if Map.null c && Map.null c' then Nothing else Just con

keepSet :: Ord a => Set a -> Maybe (Set a)
keepSet con = if Set.null con then Nothing else Just con

--------------------------------------------------------------------------------
-- State
--
-- Last but agnostic of other ranking schemes,
-- methods for manipulating the global state.

deriveSafeCopy 0 'base ''ReverseIndex
deriveSafeCopy 0 'base ''ReverseCount

initialReverseIndex :: ReverseIndex
initialReverseIndex = emptyReverseIndex

getReverseIndex :: Query ReverseIndex ReverseIndex
getReverseIndex = ask

replaceReverseIndex :: ReverseIndex -> Update ReverseIndex ()
replaceReverseIndex = put

addReversePackage :: PackageId -> CombinedDeps -> Update ReverseIndex (Map PackageName [Version])
addReversePackage pkgid deps = get >>= \revs ->
    let (revs', rec) = addPackage pkgid deps revs
    in put revs' >> return rec

removeReversePackage :: PackageId -> CombinedDeps -> Update ReverseIndex (Map PackageName [Version])
removeReversePackage pkgid deps = get >>= \revs ->
    let (revs', rec) = removePackage pkgid deps revs
    in put revs' >> return rec

changeReversePackage :: PackageId -> CombinedDeps -> CombinedDeps -> Update ReverseIndex (Map PackageName [Version])
changeReversePackage pkgid deps deps' = get >>= \revs ->
    let (revs', rec) = changePackage pkgid deps deps' revs
    in put revs' >> return rec

getReverseCount :: PackageName -> Query ReverseIndex ReverseCount
getReverseCount pkg = asks $ Map.findWithDefault emptyReverseCount pkg . reverseCount

getFlattenedReverse :: PackageName -> Query ReverseIndex (Set PackageName)
getFlattenedReverse pkg = asks $ Map.findWithDefault Set.empty pkg . flattenedReverse

makeAcidic ''ReverseIndex ['getReverseIndex
                          ,'replaceReverseIndex
                          ,'addReversePackage
                          ,'removeReversePackage
                          ,'changeReversePackage
                          ,'getReverseCount
                          ,'getFlattenedReverse
                          ]

