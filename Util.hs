module Util where

import Control.Exception	( bracket )
import Control.Monad		( unless, filterM, liftM )
import Data.Bits		( (.&.), complement )
import Data.Char		( isSpace )
import Data.List		( (\\), sort )
import Data.Map			( Map )
import qualified Data.Map as Map
import Data.Maybe		( listToMaybe )
import qualified Data.Set as Set ( fromList, toList )
import Distribution.Compat.ReadP( readP_to_S )
import Distribution.Package	( PackageIdentifier(..), Dependency(..) )
import Distribution.PackageDescription
import Distribution.PackageDescription
				( readPackageDescription )
import Distribution.Text	( display, parse )
import Distribution.Verbosity 	( silent )
import Distribution.Version	( Version )
import System.Directory		( copyFile, createDirectory, doesFileExist,
				  doesDirectoryExist, getDirectoryContents,
				  removeDirectoryRecursive )
import System.FilePath		( (</>), isPathSeparator )
import System.Posix.Files	( setFileMode )
import System.Posix.Process	( getProcessID )
import System.Posix.Types	( FileMode )
import Text.XHtml		( URL )

import PublicFile
import Locations		( archiveDir, pkgScriptURL )

-- | Registered top-level nodes in the class hierarchy.
allocatedTopLevelNodes :: [String]
allocatedTopLevelNodes = [
	"Algebra", "Codec", "Control", "Data", "Database", "Debug",
	"Distribution", "DotNet", "Foreign", "Graphics", "Language",
	"Network", "Numeric", "Prelude", "Sound", "System", "Test", "Text"]

-- | URL describing a package, including version.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = pkgScriptURL ++ "/" ++ display pkgId

-- | URL describing a package, including version.
packageNameURL :: String -> URL
packageNameURL pkg = pkgScriptURL ++ "/" ++ pkg

-- package utilities

-- | Available versions (if any) for each package mentioned in this one.
getDependencies :: GenericPackageDescription -> IO (Map String [Version])
getDependencies pkg = do
	with_vs <- mapM getVersions ps
	return (Map.fromList (filter (not . null . snd) with_vs))
  where ps = Set.toList $ Set.fromList $ map getPkg $
		buildDepends (packageDescription pkg) ++
		maybe [] depsCondTree (condLibrary pkg) ++
		concatMap (depsCondTree . snd) (condExecutables pkg)
	getPkg (Dependency n _) = n
	depsCondTree (CondNode _ deps comps) =
		deps ++ concatMap depsComponent comps
	depsComponent (_, t, me) =
		depsCondTree t ++ maybe [] depsCondTree me
	getVersions n = do
		vs <- availableVersions n
		return (n, vs)

-- | All package names available in the archive
availablePackages :: IO [String]
availablePackages = do
	let dir = localFile archiveDir
	subdirs <- dirContents dir
	flip filterM subdirs $ \ pname -> do
		doesDirectoryExist (dir </> pname)

-- | All versions of the package available in the archive, in ascending order.
availableVersions :: String -> IO [Version]
availableVersions pname = do
	dirs <- dirContents $ localFile (archiveDir `slash` pname)
	let vs = [v | dir <- dirs, (v, "") <- readP_to_S parse dir]
	liftM sort $ filterM versionExists vs
  where versionExists v =
		doesFileExist (localFile (cabalFile (PackageIdentifier pname v)))

readPackageId :: String -> Maybe PackageIdentifier
readPackageId str = listToMaybe [p |
	(p, rest) <- readP_to_S parse str,
	null (dropWhile isSpace rest)]

-- The tarball and .cabal file are placed in
--	<archiveDir>/<package>/<version>/<package>-<version>.tar.gz
--	<archiveDir>/<package>/<version>/<package>.cabal

packageDir :: PackageIdentifier -> PublicFile
packageDir pkgId =
	archiveDir `slash` pkgName pkgId `slash` display (pkgVersion pkgId)

-- | Load a package description from HackageDB.
loadPackageDescription :: PackageIdentifier -> IO GenericPackageDescription
loadPackageDescription pkgId =
	readPackageDescription silent (localFile (cabalFile pkgId))

-- | The name of the Cabal file for a given package identifier
cabalFile :: PackageIdentifier -> PublicFile
cabalFile pkgId = packageDir pkgId `slash` (pkgName pkgId ++ ".cabal")

-- | The name of the package file for a given package identifier
packageFile :: PackageIdentifier -> PublicFile
packageFile pkgId = packageDir pkgId `slash` (display pkgId ++ ".tar.gz")

maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse

-- file utilities

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x ys = front : case back of
	[] -> []
	(_:ys') -> splitOn x ys'
  where (front, back) = break (== x) ys

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory = bracket newTempDirectory removeDirectoryRecursive
  where newTempDirectory = do
		pid <- getProcessID
		let tmpDir = "/tmp/cabal-put." ++ show pid
		createDirectory tmpDir
		return tmpDir

ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists dir = do
	exists <- doesDirectoryExist dir
	unless exists $ do
		ensureDirectoryExists (dirname dir)
		createDirectory dir
		setFileMode dir dirMode

dirContents :: FilePath -> IO [FilePath]
dirContents dir = do
	isDir <- doesDirectoryExist dir
	if isDir
		then liftM (\\ [".", ".."]) $ getDirectoryContents dir
		else return []

myCopyFile :: FilePath -> FilePath -> IO ()
myCopyFile src dest = do
	copyFile src dest
	setFileMode dest fileMode

fileMask, fileMode, dirMode :: FileMode
fileMask = 0o002
fileMode = 0o666 .&. complement fileMask
dirMode = 0o2777 .&. complement fileMask

basename :: FilePath -> String
basename file = reverse (takeWhile (not . isPathSeparator) (reverse file))

dirname :: FilePath -> FilePath
dirname file = reverse (tail (dropWhile (not . isPathSeparator) (reverse file)))
