-- Body of the HTML page for a package
module Distribution.Server.Pages.Package (
    PackageData(..),
    Build(..),
    packagePage
  ) where

import Distribution.Server.Pages.Package.HaddockParse	( parseParas )
import Distribution.Server.Pages.Package.HaddockLex	( tokenise )
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Pages.Template		( hackagePage )
import Distribution.Server.Types (PkgInfo(pkgDesc))


import Distribution.PackageDescription.Configuration
				( flattenPackageDescription )
import Distribution.Package
import Distribution.PackageDescription as P
import Distribution.Version
import Distribution.Text	( display )
import Text.XHtml		hiding ( p, (</>) )

import Control.Monad		( liftM2 )
import qualified Data.Foldable as Foldable
import Data.Char		( toLower, toUpper )
import Data.List		( delete, intersperse, partition, sort, sortBy )
import Data.Maybe
import Data.Map			( Map )
import qualified Data.Map as Map
import Data.Ord			( comparing )
import System.FilePath          ( (</>), (<.>) )

packagePage :: PkgInfo -> Html
packagePage pkg =
  let pkgData = emptyPackageData (pkgDesc pkg)
   in hackagePage (display $ packageId pkg) (pkgBody pkgData)

-- | Data about a package used in the package page.
data PackageData = PackageData
	{ pdDescription	:: GenericPackageDescription
		-- ^ description of a package, which may or may not be
		-- in the database.
	, pdTags :: [(String, String)]
		-- ^ Tags added to the package.
	, pdAllVersions	:: [Version]
		-- ^ all versions of the package in the database, in
		-- ascending order.
	, pdDependencies :: Map String [Version]
		-- ^ dependent packages from 'pdDescription', each paired
		-- with available versions of that package (if any).
	, pdDocURL	:: Maybe URL
		-- ^ URL of Haddock documentation for this version of the
		-- package (if available).
	, pdBuilds	:: [Build]
		-- ^ Successful builds of this version of the package
		-- (if available).
	, pdBuildFailures :: [Build]
		-- ^ Failed builds of this version of the package
		-- (if available).
	}

emptyPackageData :: GenericPackageDescription -> PackageData
emptyPackageData pkg = PackageData {
  pdDescription = pkg,
  pdTags = [],
  pdAllVersions = [],
  pdDependencies = Map.empty,
  pdDocURL = Nothing,
  pdBuilds = [],
  pdBuildFailures = []
}

-- | Record of a build of the package
data Build = Build
	{ buildDesc :: String	-- ^ description of build target
	, buildLog :: URL	-- ^ URL of build log
	}

-- | Body of the package page
pkgBody :: PackageData -> [Html]
pkgBody pd =
	(thediv ! [theclass "floatright"] << cabalLink) :
	(h2 << docTitle) :
	prologue pkg ++
	propertySection pd ++
	downloadSection pd
  where	pkg = packageDescription (pdDescription pd)
	pkgId = package pkg
	pname = pkgName pkgId
	pversion = display (pkgVersion pkgId)
	docTitle = "The " ++ pname ++ " package (version " ++ pversion ++ ")"
	cabalLink = anchor ! [href cabalHomeURL] <<
		(image ! [alt "Built with Cabal", src cabalLogoURL])

prologue :: PackageDescription -> [Html]
prologue pkg
  | not (null desc) = html_desc
  | not (null short) = [paragraph << short]
  | otherwise = []
  where desc = description pkg
	short = synopsis pkg
	html_desc = case parseParas (tokenise desc) of
	    Left _ -> [paragraph << p | p <- paragraphs desc]
	    Right doc -> [markup htmlMarkup doc]

-- Break text into paragraphs (separated by blank lines)
paragraphs :: String -> [String]
paragraphs = map unlines . paras . lines
  where paras xs = case dropWhile null xs of
		[] -> []
		xs' -> case break null xs' of
			(para, xs'') -> para : paras xs''

downloadSection :: PackageData -> [Html]
downloadSection pd = [
	h3 << "Downloads",
	ulist << [li << i | i <- download_items]]
  where	download_items =
		[[anchor ! [href downloadURL] << tarBall,
		 toHtml << " (Cabal source package)"],
		 [anchor ! [href cabalURL] << "package description",
		 toHtml " (included in the package)"]]
	downloadURL = packageFile pkgId
	cabalURL = cabalFile pkgId
	tarBall = display pkgId ++ ".tar.gz"
	pkgId = package (packageDescription (pdDescription pd))

propertySection :: PackageData -> [Html]
propertySection pd = [
	-- h3 << "Package properties",
	tabulate $
		(if null other_vs then []
		 else [("Other versions",
			commaList (map showVers other_vs))]) ++
		[("Dependencies", html_deps_list)] ++
		[(fname, f_value) |
			(fname, htmlField) <- showFields mb_doc,
			let f_value = htmlField pkg,
			not (isNoHtml f_value)] ++
		[(capitalize fname, toHtml f_value) |
			(fname, f_value) <- pdTags pd] ++
		case pdBuilds pd of
		    [] -> case pdBuildFailures pd of
			[] -> []
			bs -> [("Build failure", commaList (map showLog bs))]
		    bs ->
			[("Built on", commaList (map (toHtml . buildDesc) bs))]
	]
  where other_vs = delete pversion (pdAllVersions pd)
	vmap = pdDependencies pd
	mb_doc = pdDocURL pd
	showVers v =
		anchor ! [href (packageURL (PackageIdentifier pname v))] <<
			display v
	pkg = flattenPackageDescription (pdDescription pd)
	pkgId = package pkg
	pname = pkgName pkgId
	pversion = pkgVersion pkgId
	html_deps_list = foldr (+++) noHtml $
		intersperse (toHtml " " +++ bold (toHtml "or") +++ br) $
		map (showDependencies vmap) deps_list
	deps_list = flatDependencies (pdDescription pd)
	showLog (Build desc url) =
		toHtml (desc ++ " (") +++
		(anchor ! [href url] << "log") +++
		toHtml ")"
	capitalize (c:cs) = toUpper c : cs
	capitalize "" = ""

tabulate :: [(String, Html)] -> Html
-- tabulate items = dlist << concat [[dterm << t, ddef << d] | (t, d) <- items]
tabulate items = table ! [theclass "properties"] <<
	[tr ! [theclass (if odd n then "odd" else "even")] <<
		[th ! [theclass "horizontal"] << t, td << d] |
		(n, (t, d)) <- zip [(1::Int)..] items]

showDependencies :: Map String [Version] -> [Dependency] -> Html
showDependencies vmap deps = commaList (map (showDependency vmap) deps)

showDependency :: Map String [Version] -> Dependency -> Html
showDependency vmap (Dependency pname vs) =
	showPkg mb_vers +++ showVers vs
  where showPkg Nothing = toHtml pname
	showPkg (Just v) =
		anchor ! [href (packageURL (PackageIdentifier pname v))] <<
			pname
	showVers AnyVersion = noHtml
	showVers vs' = toHtml (" (" ++ display vs' ++ ")")
	mb_vers = maybeLast $ filter (`withinRange` vs) $
		Map.findWithDefault [] pname vmap

showFields :: Maybe URL -> [(String, PackageDescription -> Html)]
showFields mb_doc = [
	-- Cabal-Version
	("License",	toHtml . show . license),
	("Copyright",	toHtml . P.copyright),
	("Author",	toHtml . author),
	("Maintainer",	toHtml . maintainer),
	("Stability",	toHtml . stability),
	("Category",	toHtml . category),
	("Home page",	mkRef . homepage),
	("Exposed modules",
			commaList .
				map (maybe toHtml modLink mb_doc) .
				maybe [] exposedModules . library),
	("Executables",	commaList . map toHtml . map exeName . executables)
	]
  where mkRef "" = noHtml
	mkRef url = anchor ! [href url] << url
	modLink url modName = anchor ! [href mod_url] << modName
	  where mod_url = url ++ "/" ++ map toFile modName ++ ".html"
		toFile '.' = '-'
		toFile c   = c

commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

-- Flatten the dependencies of a GenericPackageDescription into
-- disjunctive normal form.
flatDependencies :: GenericPackageDescription -> [[Dependency]]
flatDependencies pkg =
	map (map dependency . sortOn (map toLower . fst)) $ sort $
	map get_deps $
	foldr intersectDisjunct head_deps $
		maybe id ((:) . fromCondTree) (condLibrary pkg) $
		map (fromCondTree . snd) (condExecutables pkg)
  where dependency (p, i) = Dependency p (toVersionRange i)

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

type Conjunct = Map String (Interval Version)

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


-- UTILS:

maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = "packages" </> display pkgId

-- | The name of the package file for a given package identifier
packageFile :: PackageIdentifier -> URL
packageFile pkgId = packageURL pkgId </> display pkgId <.> "tar.gz"

-- | The name of the Cabal file for a given package identifier
cabalFile :: PackageIdentifier -> URL
cabalFile pkgId = packageURL pkgId </> pkgName pkgId <.> "cabal"

cabalLogoURL :: URL
cabalLogoURL = "/built-with-cabal.png"

-- global URLs
cabalHomeURL :: URL
cabalHomeURL = "http://haskell.org/cabal/"
