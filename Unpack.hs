-- Unpack a tarball containing a Cabal package
module Unpack (unpackPackage) where

import Control.Monad		( unless, when )
import Control.Monad.Error	( ErrorT(..), throwError )
import Control.Monad.Reader	( ReaderT(..), ask )
import Control.Monad.Writer	( WriterT(..), tell )
import Control.Monad.Trans      ( MonadIO(liftIO) )
import Data.ByteString.Lazy as BS
				( ByteString, writeFile )
import Data.List		( isSuffixOf, nub, (\\), partition )
import Distribution.Package	( PackageIdentifier(..) )
import Distribution.PackageDescription
				( GenericPackageDescription(..),
				  PackageDescription(..), exposedModules )
import Distribution.PackageDescription
				( parsePackageDescription )
import Distribution.PackageDescription.Configuration
				( flattenPackageDescription )
import Distribution.PackageDescription.Check
				( PackageCheck(..),
				  checkPackage, checkPackageFiles )
import Distribution.ParseUtils	( ParseResult(..),
				  locatedErrorMsg, showPWarning )
import Distribution.Text	( display, simpleParse )
import System.Cmd		( rawSystem )
import System.Exit		( ExitCode(..) )
import System.FilePath		( (</>), (<.>) )

import Control.Exception	( bracket )
import System.Directory
         ( createDirectory, doesFileExist, removeDirectoryRecursive )
import System.Random            ( getStdGen, setStdGen, RandomGen(next) )

-- | Upload or check a tarball containing a Cabal package.
-- Returns either an fatal error or a package description and a list
-- of warnings.
unpackPackage :: FilePath -> ByteString
              -> IO (Either String (GenericPackageDescription, [String]))
unpackPackage tarFile contents = runPutMonad $ do
	unless (".tar.gz" `isSuffixOf` tarFile) $
		die $ tarFile ++ " is not a gzipped tar file"
	let pkgIdStr = take (length tarFile - 7) tarFile

	PackageIdentifier pName pVersion <- case simpleParse pkgIdStr of
	    Just pkgId | display pkgId == pkgIdStr -> return pkgId
	    _ -> die $ "malformed package identifier " ++ pkgIdStr

	tmpDir <- getTmpDir
	-- save a copy of the tar file
	let tmpTarFile = tmpDir </> tarFile
	liftIO $ BS.writeFile tmpTarFile contents

	-- unpack package dir <package>-<version> the tarball
	systemOrFail
		"tar" ["-C", tmpDir, "-xzf", tmpTarFile, pkgIdStr]
		("could not extract " ++ pkgIdStr ++ " directory from " ++ tarFile)

	let srcCabalFile = pkgIdStr </> pName <.> "cabal"
	let tmpPkgDir = tmpDir </> pkgIdStr
	let tmpCabalFile = tmpDir </> srcCabalFile
	cabalIncluded <- liftIO $ doesFileExist tmpCabalFile
	when (not cabalIncluded) $
		die $ "could not extract " ++ srcCabalFile ++ " from " ++ tarFile

	-- Check that the name and version in Cabal file match
	pkgDesc <- do
		inp <- liftIO $ readFile tmpCabalFile
		case parsePackageDescription inp of
		    ParseFailed err -> die $ showError (locatedErrorMsg err)
		    ParseOk warnings pkgDesc -> do
			mapM_ (die . showPWarning srcCabalFile) warnings
			return pkgDesc
	let pkgId = package (packageDescription pkgDesc)
	when (pkgName pkgId /= pName) $
		die "package name in the cabal file does not match the file name"
	when (pkgVersion pkgId /= pVersion) $
		die "package version in the cabal file does not match the file name"

	let installedTarFile = packageFile pkgId
	let installedCabalFile = cabalFile pkgId

	-- Do not allow replacing of existing packages (for security)
	tarPresent <- liftIO $ doesFileExist installedTarFile
	cabalPresent <- liftIO $ doesFileExist installedCabalFile
	when (tarPresent && cabalPresent) $
		die "this version of the package is already present in the database"
	extraChecks pkgDesc tmpPkgDir

	return pkgDesc
  where showError (Nothing, msg) = msg
	showError (Just n, msg) = "line " ++ show n ++ ": " ++ msg

-- Miscellaneous checks on package description
extraChecks :: GenericPackageDescription -> FilePath -> PutMonad ()
extraChecks genPkgDesc pkgPath = do
	let pkgDesc = flattenPackageDescription genPkgDesc
	-- various checks
	fileChecks <- liftIO $ checkPackageFiles pkgDesc pkgPath
	let pureChecks = checkPackage genPkgDesc (Just pkgDesc)
	    checks = pureChecks ++ fileChecks
	    isDistError (PackageDistSuspicious {}) = False
	    isDistError _                          = True
	    (errors, warnings) = partition isDistError checks
	mapM_ (die . explanation) errors
	mapM_ (warn . explanation) warnings

	-- Check reasonableness of names of exposed modules
	let badTopLevel =
		maybe [] (nub . map (takeWhile (/= '.')) . exposedModules)
			(library pkgDesc) \\
		allocatedTopLevelNodes

	unless (null badTopLevel) $
		warn $ "Exposed modules use unallocated top-level names: " ++
				unwords badTopLevel

systemOrFail :: String -> [String] -> String -> PutMonad ()
systemOrFail cmd args errMsg = do
	status <- liftIO $ rawSystem cmd args
	case status of
	    ExitFailure _ -> die errMsg
	    ExitSuccess -> return ()

-- Monad for uploading packages:
--	ReaderT for the name of a temporary directory
--	WriterT for warning messages
--	ErrorT for fatal errors
type PutMonad = ReaderT FilePath (WriterT [String] (ErrorT String IO))

getTmpDir :: PutMonad FilePath
getTmpDir = ask

die :: String -> PutMonad a
die msg = throwError msg

warn :: String -> PutMonad ()
warn msg = tell [msg]

runPutMonad :: PutMonad a -> IO (Either String (a, [String]))
runPutMonad m = withTempDirectory $ runErrorT . runWriterT . runReaderT m

-- | Registered top-level nodes in the class hierarchy.
allocatedTopLevelNodes :: [String]
allocatedTopLevelNodes = [
	"Algebra", "Codec", "Control", "Data", "Database", "Debug",
	"Distribution", "DotNet", "Foreign", "Graphics", "Language",
	"Network", "Numeric", "Prelude", "Sound", "System", "Test", "Text"]


-- package utilities

-- The tarball and .cabal file are placed in
--	<archiveDir>/<package>/<version>/<package>-<version>.tar.gz
--	<archiveDir>/<package>/<version>/<package>.cabal

packageDir :: PackageIdentifier -> FilePath
packageDir pkgId =
	archiveDir </> pkgName pkgId </> display (pkgVersion pkgId)

-- | The name of the Cabal file for a given package identifier
cabalFile :: PackageIdentifier -> FilePath
cabalFile pkgId = packageDir pkgId </> (pkgName pkgId ++ ".cabal")

-- | The name of the package file for a given package identifier
packageFile :: PackageIdentifier -> FilePath
packageFile pkgId = packageDir pkgId </> display pkgId <.> "tar.gz"

-- file utilities

withTempDirectory :: (FilePath -> IO a) -> IO a
withTempDirectory = bracket newTempDirectory removeDirectoryRecursive
  where newTempDirectory = do
		gen <- getStdGen
                let (n, gen') = next gen
                setStdGen gen'
		let tmpDir = "/tmp/cabal-put." ++ show n
		createDirectory tmpDir
		return tmpDir

docRoot :: FilePath
docRoot = "/srv/www/hackage.haskell.org/public_html"

-- Package archive directory
archiveDir :: FilePath
archiveDir = docRoot </> "packages/archive"
