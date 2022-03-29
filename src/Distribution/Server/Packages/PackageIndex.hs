{-# LANGUAGE CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Packages.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.Server.Packages.PackageIndex (
    -- * Package index data type
    PackageIndex,

    -- * Creating an index
    fromList,

    -- * Updates
    merge,
    insert,
    insertWith,
    deletePackageName,
    deletePackageId,

    -- * Queries
    indexSize,
    numPackageVersions,
    packageNames,

    -- ** Precise lookups
    lookupPackageName,
    lookupPackageId,
    lookupPackageForId,
    lookupDependency,

    -- ** Case-insensitive searches
    searchByName,
    SearchResult(..),
    searchByNameSubstring,

    -- ** Bulk queries
    allPackages,
    allPackagesByName
  ) where

import Distribution.Server.Prelude hiding (lookup)

import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.Merge

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Foldable as Foldable
import Data.List (groupBy, find, isInfixOf)
import Data.SafeCopy

import Distribution.Types.PackageName
import Distribution.Package
         ( PackageIdentifier(..)
         , Package(..), packageName, packageVersion )
import Distribution.Types.Dependency
import Distribution.Version ( withinRange )
import Distribution.Simple.Utils (lowercase)

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched efficiently by package name and version.
--
newtype PackageIndex pkg = PackageIndex
  -- A mapping from package names to a non-empty list of  versions of that
  -- package, in ascending order (most recent package last)
  -- TODO: Wouldn't it make more sense to store the most recent package first?
  --
  -- This allows us to find all versions satisfying a dependency.
  -- Most queries are a map lookup followed by a linear scan of the bucket.
  --
  (Map PackageName [pkg])

  deriving (Show, Read, Typeable, MemSize)

instance Eq pkg => Eq (PackageIndex pkg) where
  PackageIndex m1 == PackageIndex m2 = flip Foldable.all (mergeMaps m1 m2) $ \mr -> case mr of
      InBoth pkgs1 pkgs2 -> bagsEq pkgs1 pkgs2
      OnlyInLeft _       -> False
      OnlyInRight _      -> False
    where
      bagsEq []     [] = True
      bagsEq []     _  = False
      bagsEq (x:xs) ys = case suitable_ys of
        []                -> False
        (_y:suitable_ys') -> bagsEq xs (unsuitable_ys ++ suitable_ys')
        where (unsuitable_ys, suitable_ys) = break (==x) ys


instance Package pkg => Monoid (PackageIndex pkg) where
  mempty  = PackageIndex (Map.empty)
  mappend = (<>)
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 (<>) xs

instance Package pkg => Semigroup (PackageIndex pkg) where
  (<>) = merge

invariant :: Package pkg => PackageIndex pkg -> Bool
invariant (PackageIndex m) = all (uncurry goodBucket) (Map.toList m)
  where
    goodBucket _    [] = False
    goodBucket name (pkg0:pkgs0) = check (packageId pkg0) pkgs0
      where
        check pkgid []          = packageName pkgid == name
        check pkgid (pkg':pkgs) = packageName pkgid == name
                               && pkgid < pkgid'
                               && check pkgid' pkgs
          where pkgid' = packageId pkg'

--
-- * Internal helpers
--

mkPackageIndex :: Package pkg => Map PackageName [pkg] -> PackageIndex pkg
mkPackageIndex index = assert (invariant (PackageIndex index)) (PackageIndex index)

internalError :: String -> a
internalError name = error ("PackageIndex." ++ name ++ ": internal error")

-- | Lookup a name in the index to get all packages that match that name
-- case-sensitively.
--
lookup :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookup (PackageIndex m) name = fromMaybe [] $ Map.lookup name m

--
-- * Construction
--

-- | Build an index out of a bunch of packages.
--
-- If there are duplicates, later ones mask earlier ones.
--
fromList :: Package pkg => [pkg] -> PackageIndex pkg
fromList pkgs = mkPackageIndex
              . Map.map fixBucket
              . Map.fromListWith (++)
              $ [ (packageName pkg, [pkg])
                | pkg <- pkgs ]
  where
    fixBucket = -- out of groups of duplicates, later ones mask earlier ones
                -- but Map.fromListWith (++) constructs groups in reverse order
                map head
                -- Eq instance for PackageIdentifier is wrong, so use Ord:
              . groupBy (\a b -> EQ == comparing packageId a b)
                -- relies on sortBy being a stable sort so we
                -- can pick consistently among duplicates
              . sortBy (comparing packageId)

--
-- * Updates
--

-- | Merge two indexes.
--
-- Packages from the second mask packages of the same exact name
-- (case-sensitively) from the first.
--
merge :: Package pkg => PackageIndex pkg -> PackageIndex pkg -> PackageIndex pkg
merge i1@(PackageIndex m1) i2@(PackageIndex m2) =
  assert (invariant i1 && invariant i2) $
    mkPackageIndex (Map.unionWith mergeBuckets m1 m2)

-- | Elements in the second list mask those in the first.
mergeBuckets :: Package pkg => [pkg] -> [pkg] -> [pkg]
mergeBuckets []     ys     = ys
mergeBuckets xs     []     = xs
mergeBuckets xs@(x:xs') ys@(y:ys') =
      case packageId x `compare` packageId y of
        GT -> y : mergeBuckets xs  ys'
        EQ -> y : mergeBuckets xs' ys'
        LT -> x : mergeBuckets xs' ys

-- | Inserts a single package into the index.
--
-- This is equivalent to (but slightly quicker than) using 'mappend' or
-- 'merge' with a singleton index.
--
insert :: Package pkg => pkg -> PackageIndex pkg -> PackageIndex pkg
insert pkg (PackageIndex index) = mkPackageIndex $ -- or insertWith const
  Map.insertWith (\_ -> insertNoDup) (packageName pkg) [pkg] index
  where
    pkgid = packageId pkg
    insertNoDup []                = [pkg]
    insertNoDup pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
      LT -> pkg  : pkgs
      EQ -> pkg  : pkgs'  -- this replaces the package
      GT -> pkg' : insertNoDup pkgs'

-- | Inserts a single package into the index, combining an old and new value with a function.
-- This isn't in cabal's version of PackageIndex.
--
-- The merge function is called as (f newPkg oldPkg). Ensure that the result has the same
-- package id as the two arguments; otherwise newPkg is used.
--
insertWith :: Package pkg => (pkg -> pkg -> pkg) -> pkg -> PackageIndex pkg -> PackageIndex pkg
insertWith mergeFunc pkg (PackageIndex index) = mkPackageIndex $
    Map.insertWith (\_ -> insertMerge) (packageName pkg) [pkg] index
  where
    pkgid = packageId pkg
    insertMerge [] = [pkg]
    insertMerge pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
        LT -> pkg : pkgs
        EQ -> let merged = mergeFunc pkg pkg' in
              if packageId merged == pkgid then merged : pkgs'
                                           else pkg : pkgs'
        GT -> pkg' : insertMerge pkgs'

-- | Internal delete helper.
--
delete :: Package pkg => PackageName -> (pkg -> Bool) -> PackageIndex pkg -> PackageIndex pkg
delete name p (PackageIndex index) = mkPackageIndex $
  Map.update filterBucket name index
  where
    filterBucket = deleteEmptyBucket
                 . filter (not . p)
    deleteEmptyBucket []        = Nothing
    deleteEmptyBucket remaining = Just remaining

-- | Removes a single package from the index.
--
deletePackageId :: Package pkg => PackageIdentifier -> PackageIndex pkg -> PackageIndex pkg
deletePackageId pkgid =
  delete (packageName pkgid) (\pkg -> packageId pkg == pkgid)

-- | Removes all packages with this (case-sensitive) name from the index.
--
deletePackageName :: Package pkg => PackageName -> PackageIndex pkg -> PackageIndex pkg
deletePackageName name =
  delete name (\pkg -> packageName pkg == name)

--
-- * Bulk queries
--

-- | Get all the packages from the index.
--
allPackages :: Package pkg => PackageIndex pkg -> [pkg]
allPackages (PackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: Package pkg => PackageIndex pkg -> [[pkg]]
allPackagesByName (PackageIndex m) = Map.elems m

--
-- * Lookups
--

-- | Does a lookup by package id (name & version).
--
-- Since multiple package DBs mask each other case-sensitively by package name,
-- then we get back at most one package.
--
lookupPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> Maybe pkg
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (packageName pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name.
-- The returned list should be ordered (strictly ascending) by version number.
--
lookupPackageName :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookupPackageName index name = lookup index name

-- | Search by name of a package identifier, and further select a version if possible.
--
lookupPackageForId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> ([pkg], Maybe pkg)
lookupPackageForId index pkgid =
  let pkgs = lookupPackageName index (packageName pkgid)
  in (,) pkgs $ find ((==pkgid) . packageId) pkgs

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageIndex pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange _) =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name
        , packageVersion pkg `withinRange` versionRange ]

--
-- * Case insensitive name lookups
--

-- | Does a case-insensitive search by package name.
--
-- If there is only one package that compares case-insentiviely to this name
-- then the search is unambiguous and we get back all versions of that package.
-- If several match case-insentiviely but one matches exactly then it is also
-- unambiguous.
--
-- If however several match case-insentiviely and none match exactly then we
-- have an ambiguous result, and we get back all the versions of all the
-- packages. The list of ambiguous results is split by exact package name. So
-- it is a non-empty list of non-empty lists.
--
searchByName :: Package pkg => PackageIndex pkg -> String -> SearchResult [pkg]
searchByName (PackageIndex m) name =
  case [ pkgs | pkgs@(pn,_) <- Map.toList m
              , let name' = unPackageName pn
              , lowercase name' == lname ] of
    []              -> None
    [(_,pkgs)]      -> Unambiguous pkgs
    pkgss           -> case find ((mkPackageName name==) . fst) pkgss of
      Just (_,pkgs) -> Unambiguous pkgs
      Nothing       -> Ambiguous (map snd pkgss)
  where lname = lowercase name

data SearchResult a = None | Unambiguous a | Ambiguous [a] deriving (Show)

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: Package pkg => PackageIndex pkg -> String -> [pkg]
searchByNameSubstring (PackageIndex m) searchterm =
  [ pkg
  | (pn, pkgs) <- Map.toList m
  , let name = unPackageName pn
  , lsearchterm `isInfixOf` lowercase name
  , pkg <- pkgs ]
  where lsearchterm = lowercase searchterm

-- | Gets the number of packages in the index (number of names).
indexSize :: Package pkg => PackageIndex pkg -> Int
indexSize (PackageIndex m) = Map.size m

-- | The number of package versions
-- (i.e., we should have @length . allPackages == numPackageVersions@)
numPackageVersions ::PackageIndex pkg -> Int
numPackageVersions (PackageIndex m) = sum . map (length . snd) $ Map.toList m

-- | Get an ascending list of package names in the index.
packageNames :: Package pkg => PackageIndex pkg -> [PackageName]
packageNames (PackageIndex m) = Map.keys m

---------------------------------- State for PackageIndex
instance (Package pkg, SafeCopy pkg) => SafeCopy (PackageIndex pkg) where
  putCopy index = contain $ do
    safePut $ allPackages index
  getCopy = contain $ do
    packages <- safeGet
    return $ fromList packages
