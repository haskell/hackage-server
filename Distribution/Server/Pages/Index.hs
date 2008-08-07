-- Generate an HTML page listing all available packages

module Distribution.Server.Pages.Index (packageIndex) where

import Distribution.Server.Pages.Template	( hackagePage )

import Text.XHtml.Strict hiding ( p )
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
				( flattenPackageDescription )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Types (PkgInfo(..))
import Distribution.Simple.Utils (comparing, equating)

import Data.Char (toLower, toUpper, isSpace)
import Data.List (intersperse, sortBy, groupBy, nub, maximumBy)
import Data.Maybe ()


packageIndex :: PackageIndex.PackageIndex PkgInfo -> Html
packageIndex = formatPkgGroups
                 . map (flattenPackageDescription
                      . pkgDesc
                      . maximumBy (comparing packageVersion))
                 . PackageIndex.allPackagesByName

data Category = Category String | NoCategory
	deriving (Eq, Ord)

-- Packages, grouped by category and ordered by name with each category.
formatPkgGroups :: [PackageDescription] -> Html
formatPkgGroups pkgs = hackagePage "packages by category" docBody
  where docBody =
		(thediv ! [theclass "floatright"] << searchBox) :
		(h2 << "Packages by category") :
		-- table of contents
		paragraph ! [theclass "toc"] <<
			(bold << "Categories:" : toHtml " " :
			 intersperse (toHtml ", ") (map catLink cat_pkgs) ++
			 [toHtml "."]) :
		-- packages grouped by category
		[formatCategory cat +++
			formatPkgList (sortBy (comparing sortKey) sub_pkgs) |
			(cat, sub_pkgs) <- cat_pkgs]
	searchBox =
		[form ! [method "get", action "http://www.google.co.uk/search"] <<
			[input ! [thetype "hidden", name "hl", value "en"],
			 input ! [thetype "hidden", name "as_sitesearch", value "hackage.haskell.org/packages"],
			 input ! [thetype "text", size "20", name "as_q", value ""],
			 input ! [thetype "submit", value "Search package pages"]
			]]
	catLink (cat, sub_pkgs) =
		(anchor ! [href ("#" ++ catLabel catName)] << catName) +++
		spaceHtml +++
		toHtml ("(" ++ show (length sub_pkgs) ++ ")")
	  where catName = categoryName cat
	cat_pkgs = groupOnFstBy normalizeCategory $ [(capitalize cat, pkg) |
			pkg <- pkgs, cat <- categories pkg]
	sortKey pkg = map toLower $ pkgName $ package pkg
	formatCategory cat =
		h3 ! [theclass "category"] <<
			anchor ! [name (catLabel catName)] << catName
	  where catName = categoryName cat
	catLabel cat = "cat:" ++ cat
	categoryName (Category cat) = cat
	categoryName NoCategory = "Unclassified"
	capitalize (Category s) =
		Category (unwords [toUpper c : cs | (c:cs) <- words s])
	capitalize NoCategory = NoCategory

formatPkgList :: [PackageDescription] -> Html
formatPkgList pkgs = ulist ! [theclass "packages"] << map formatPkg pkgs

formatPkg :: PackageDescription -> Html
formatPkg pkg = li << (pkgLink : toHtml (" " ++ ptype) : defn)
  where pname = pkgName (package pkg)
	pkgLink = anchor ! [href (packageNameURL pname)] << pname
	defn
	  | null (synopsis pkg) = []
	  | otherwise = [toHtml (": " ++ trim (synopsis pkg))]
	ptype
	  | null (executables pkg) = "library"
	  | hasLibs pkg = "library and " ++ programs
	  | otherwise = programs
	  where programs
		  | length (executables pkg) > 1 = "programs"
		  | otherwise = "program"
	trim s
	  | length s < 90 = s
	  | otherwise = reverse (dropWhile (/= ',') (reverse (take 76 s))) ++ " ..."

categories :: PackageDescription -> [Category]
categories pkg
  | not (null cats) && not (cats `elem` blacklist) = split cats
  | not (null top_level_nodes) && length top_level_nodes < 3 &&
	all (`elem` allocatedTopLevelNodes) top_level_nodes =
	map Category top_level_nodes
  | otherwise = [NoCategory]
  where cats = trim (category pkg)
	-- trim will not be necessary with future releases of cabal
	trim = reverse . dropWhile isSpace . reverse
	split cs = case break (== ',') cs of
		(front, _:back) ->
			Category front : split (dropWhile isSpace back)
		(front, []) -> [Category front]
	-- if no category specified, use top-level of module hierarchy
	top_level_nodes =
		maybe [] (nub . map (takeWhile (/= '.')) . exposedModules)
		(library pkg)

-- categories we ignore
blacklist :: [String]
blacklist = ["Application", "Foreign binding", "Tool", "Type", "Various",
	"Unclassified"]

groupOnFstBy :: (Ord a, Ord c) => (a -> c) -> [(a, b)] -> [(a, [b])]
groupOnFstBy f xys = [(x, y : map snd xys') |
	(x, y) : xys' <- groupBy (equating (f . fst)) (sortBy (comparing sortKey) xys)]
  where sortKey (x, _) = (f x, x)

normalizeCategory :: Category -> Category
normalizeCategory (Category n) = Category (map toLower n)
normalizeCategory NoCategory = NoCategory

allocatedTopLevelNodes :: [String]
allocatedTopLevelNodes = [
	"Algebra", "Codec", "Control", "Data", "Database", "Debug",
	"Distribution", "DotNet", "Foreign", "Graphics", "Language",
	"Network", "Numeric", "Prelude", "Sound", "System", "Test", "Text"]

packageNameURL :: PackageName -> URL
packageNameURL pkg = "/packages/" ++ pkg

type PackageName = String --FIXME: Cabal-1.5 uses a newtype
