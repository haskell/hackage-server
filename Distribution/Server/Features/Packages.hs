module Distribution.Server.Features.Packages (
    PackagesFeature(..),
    PackagesResource(..),
    PackageRender(..),
    initPackagesFeature,
    doPackageRender
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (userName)
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.State as State
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.ResourceTypes as Resource

import Happstack.Server
import Happstack.State (query)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Version (Version(..), VersionRange(..))
import Distribution.Server.Packages.ModuleForest
import Distribution.Text
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable as Foldable
import Control.Monad (guard, liftM2)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.List (maximumBy, sort, sortBy, find, partition)
import Data.Char (toLower)
import Data.Ord (comparing)
import qualified Network.URI as URI

-- the goal is to have the HTML modules import /this/ one, not the other way around
import qualified Distribution.Server.Pages.Recent as Pages

data PackagesFeature = PackagesFeature {
    packagesResource :: PackagesResource,
    -- recent caches. in lieu of a log feature
    cacheRecent :: Cache.GenCache (Response, Response), -- (html, rss)
    -- necessary information for the representation of a package resource
    packageRender :: PackageId -> IO (Maybe PackageRender)
    -- other informational hooks: perhaps a simplified CondTree so a browser script can dynamically change the package page based on flags
}

data PackagesResource = PackagesResource {
    -- replace with log feature
    packagesRecent :: Resource
}

instance HackageFeature PackagesFeature where
    getFeature pkgsf = HackageModule
      { featureName = "packages"
      , resources   = map ($packagesResource pkgsf) [packagesRecent]
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initPackagesFeature :: CoreFeature -> IO PackagesFeature
initPackagesFeature core = do
    recents <- Cache.newCacheable
    registerHook (packageIndexChange core) $ do
        state <- query State.GetPackagesState
        users <- query State.GetUserDb
        now   <- getCurrentTime
        let recentChanges = reverse $ sortBy (comparing pkgUploadTime) (PackageIndex.allPackages . State.packageList $ state)
        Cache.putCache recents (toResponse $ Resource.XHtml $ Pages.recentPage users recentChanges,
                                toResponse $ Pages.recentFeed users (fromJust $ URI.uriAuthority =<< URI.parseURI "http://hackage.haskell.org") now recentChanges)
    return PackagesFeature
      { packagesResource = PackagesResource
          { packagesRecent = (resourceAt "/recent") { resourceGet = [("html", Cache.respondCache recents fst), ("rss", Cache.respondCache recents snd)] }
          }
      , cacheRecent   = recents
      , packageRender = \pkg -> do
            state <- query State.GetPackagesState
            users <- query State.GetUserDb
            return $ doPackageRender (State.packageList state) users pkg
      }

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId :: PackageIdentifier,
    rendAllVersions :: [Version],
    rendDepends :: [[Dependency]],
    rendExecNames :: [String],
    rendLicenseName :: String,
    rendMaintainer :: Maybe String,
    rendCategory :: [String],
    rendRepoHeads :: [(RepoType, String, SourceRepo)],
    rendModules :: Maybe ModuleForest,
    rendHasTarball :: Bool,
    rendUploadInfo :: (UTCTime, String),
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther :: PackageDescription
}
doPackageRender :: PackageIndex PkgInfo -> Users.Users -> PackageId -> Maybe PackageRender
doPackageRender pkgIndex users (PackageIdentifier name version) = do
    let infos = PackageIndex.lookupPackageName pkgIndex name
    guard (not . null $ infos)
    info <- if version == Version [] [] then Just $ maximumBy (comparing packageVersion) infos
                                        else find ((==version) . packageVersion) infos
    let genDesc  = pkgDesc info
        flatDesc = flattenPackageDescription genDesc
        desc     = packageDescription genDesc
    return $ PackageRender
      { rendPkgId = pkgInfoId info
      , rendAllVersions = sort $ map packageVersion infos
      , rendDepends   = flatDependencies genDesc
      , rendExecNames = map exeName (executables flatDesc)
      , rendLicenseName = display (license desc) -- maybe make this a bit more human-readable (sans camel case)
      , rendMaintainer  = case maintainer desc of "None" -> Nothing; "none" -> Nothing; "" -> Nothing; person -> Just person
      , rendCategory = case category desc of [] -> []; str -> [str] -- TODO: split on commas and whatnot
      , rendRepoHeads = catMaybes (map rendRepo $ sourceRepos desc)
      , rendModules = fmap (moduleForest . exposedModules) (library flatDesc)
      , rendHasTarball = not . null $ pkgTarball info
      , rendUploadInfo = let (utime, uid) = pkgUploadData info in (utime, maybe "Unknown" (display . userName) $ Users.lookupId uid users)
      , rendOther = desc
      }
  where
    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

{-
Some quick notes about the kind of data needed in Distribution.Server.Pages.Package:

Package name (from package id): description (synopsis of desc)
prologue - take description from desc and try to parse it in haddock
properties data
    versions: linking to other versions with pdAllVersions pd (partitioned into less than + greater than) with current link bolded
    dependencies: from [[Dependencies]], disjunctive lists
    other standard fields relying on package descriptions: license, copyright, author, maintainer, stability, categories
        optionally: home page, bug trackers, any executables.
        These are all taken from the flattened description, although this only matters for execs and library      
    other hackage-specific tags: currently, upload information from PkgInfo
    server-side build information, currently unused
module data - moduleForest of exposedModules (library genDesc), using base doc URL passed to it - again, this is 
download section data - tarball url and cabal url (does not currently check for tarball existence)
reports section data - just a link
admin section data - likewise
-}

-----------------------------------------------------------------------
-- Flatten the dependencies of a GenericPackageDescription into
-- disjunctive normal form.
--
-- This could easily be in its own module.
flatDependencies :: GenericPackageDescription -> [[Dependency]]
flatDependencies pkg =
	map (map dependency . sortOn (packageNameLowerCase . fst)) $ sort $
	map get_deps $
	foldr reduceDisjunct [] $
	foldr intersectDisjunct head_deps $
		maybe id ((:) . fromCondTree) (condLibrary pkg) $
		map (fromCondTree . snd) (condExecutables pkg)
  where dependency (p, i) = Dependency p (toVersionRange i)
        packageNameLowerCase (PackageName name) = map toLower name

	-- put the constrained ones first, for sorting purposes
	get_deps m = ranges ++ others
	  where (others, ranges) = partition (simple . snd) (Map.toList m)

	simple (Interval NoLowerBound NoUpperBound) = True
	simple _ = False

	head_deps = fromDependencies (buildDepends (packageDescription pkg))

	fromDependencies :: [Dependency] -> Disjunct
	fromDependencies = foldr addDep unitDisjunct
	  where	addDep (Dependency p vr) =
			liftM2 (Map.insertWith intersectInterval p)
				(fromVersionRange vr)

	fromCondTree :: CondTree v [Dependency] a -> Disjunct
	fromCondTree (CondNode _ ds comps) =
		foldr intersectDisjunct (fromDependencies ds) $
			map fromComponent comps

	fromComponent (_, _, Nothing) = unitDisjunct
	fromComponent (_, then_part, Just else_part) =
		unionDisjunct (fromCondTree then_part)
			(fromCondTree else_part)

	reduceDisjunct c cs
	  | any (c `subConjunct`) cs = cs
	  | otherwise = c : filter (not . (`subConjunct` c)) cs

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])

-- Open and closed intervals in a totally ordered set

data Interval a = Interval (LowerBound a) (UpperBound a)
	deriving (Eq, Ord, Show)

data LowerBound a = NoLowerBound | LowerBound a Bool	-- True means inclusive
	deriving (Eq, Ord, Show)

data UpperBound a = UpperBound a Bool | NoUpperBound	-- True means exclusive
	deriving (Eq, Ord, Show)

toVersionRange :: Interval Version -> VersionRange
toVersionRange (Interval (LowerBound v1 True) (UpperBound v2 False))
  | v1 == v2 = ThisVersion v1
toVersionRange (Interval l u) = intersect (lowerBound l) (upperBound u)
  where lowerBound NoLowerBound = AnyVersion
	lowerBound (LowerBound v True) =
		UnionVersionRanges (ThisVersion v) (LaterVersion v)
	lowerBound (LowerBound v False) = LaterVersion v
	upperBound NoUpperBound = AnyVersion
	upperBound (UpperBound v True) = EarlierVersion v
	upperBound (UpperBound v False) =
		UnionVersionRanges (ThisVersion v) (EarlierVersion v)
	intersect AnyVersion r = r
	intersect r AnyVersion = r
	intersect r1 r2 = IntersectVersionRanges r1 r2

fromVersionRange :: VersionRange -> [Interval Version]
fromVersionRange AnyVersion = [Interval NoLowerBound NoUpperBound]
fromVersionRange (ThisVersion v) =
	[Interval (LowerBound v True) (UpperBound v False)]
fromVersionRange (LaterVersion v) =
	[Interval (LowerBound v False) NoUpperBound]
fromVersionRange (EarlierVersion v) =
	[Interval NoLowerBound (UpperBound v True)]
fromVersionRange (UnionVersionRanges (ThisVersion v1) (LaterVersion v2))
  | v1 == v2 = [Interval (LowerBound v1 True) NoUpperBound]
fromVersionRange (UnionVersionRanges (LaterVersion v2) (ThisVersion v1))
  | v1 == v2 = [Interval (LowerBound v1 True) NoUpperBound]
fromVersionRange (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2))
  | v1 == v2 = [Interval NoLowerBound (UpperBound v1 False)]
fromVersionRange (UnionVersionRanges (EarlierVersion v2) (ThisVersion v1))
  | v1 == v2 = [Interval NoLowerBound (UpperBound v1 False)]
fromVersionRange (UnionVersionRanges r1 r2) =
	fromVersionRange r1 ++ fromVersionRange r2
fromVersionRange (IntersectVersionRanges r1 r2) =
	filter (not . nullInterval) $
	liftM2 intersectInterval (fromVersionRange r1) (fromVersionRange r2)
-- temporary fix. TODO: change this entire module to use Distribution.Version.VersionInterval
fromVersionRange _ = []

intersectInterval :: Ord a => Interval a -> Interval a -> Interval a
intersectInterval (Interval l1 u1) (Interval l2 u2) =
	Interval (max l1 l2) (min u1 u2)

nullInterval :: Ord a => Interval a -> Bool
nullInterval (Interval (LowerBound v1 closed1) (UpperBound v2 open2)) =
	v1 > v2 || v1 == v2 && (not closed1 || open2)
nullInterval _ = False

subInterval :: Ord a => Interval a -> Interval a -> Bool
subInterval (Interval l1 u1) (Interval l2 u2) = l2 <= l1 && u1 <= u2

-- Constraints in disjunctive normal form

type Conjunct = Map PackageName (Interval Version)

nullConjunct :: Conjunct -> Bool
nullConjunct = Foldable.any nullInterval

intersectConjunct :: Conjunct -> Conjunct -> Conjunct
intersectConjunct = Map.unionWith intersectInterval

subConjunct :: Conjunct -> Conjunct -> Bool
subConjunct m1 m2 =
	Map.null (Map.difference m2 m1) &&
	Foldable.and (Map.intersectionWith subInterval m1 m2)

type Disjunct = [Conjunct]

unitDisjunct :: Disjunct
unitDisjunct = [Map.empty]

intersectDisjunct :: Disjunct -> Disjunct -> Disjunct
intersectDisjunct xs ys =
	filter (not . nullConjunct) (liftM2 intersectConjunct xs ys)

-- eliminate any Conjunct list that is more restrictive than another
unionDisjunct :: Disjunct -> Disjunct -> Disjunct
unionDisjunct xs ys = xs' ++ ys'
  where ys' = [y | y <- ys, not (or [subConjunct y x | x <- xs])]
	xs' = [x | x <- xs, not (or [subConjunct x y | y <- ys'])]

