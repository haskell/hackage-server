-- Unpack a tarball containing a Cabal package
module Unpack (unpackPackage) where

import Control.Monad		( unless, when )
import Control.Monad.Error	( ErrorT(..), throwError )
import Control.Monad.Reader	( ReaderT(..), ask )
import Control.Monad.Writer	( WriterT(..), tell )
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
import Distribution.Text	( display )
import Network.CGI		( liftIO )
import System.Cmd		( rawSystem )
import System.Directory		( doesFileExist )
import System.Exit		( ExitCode(..) )
import System.FilePath		( (</>), (<.>) )
import System.Posix.Files	( setFileCreationMask )

import PublicFile
import Util

-- | Upload or check a tarball containing a Cabal package.
-- Returns either an fatal error or a package description and a list
-- of warnings.
unpackPackage :: FilePath -> ByteString -> Bool ->
	IO (Either String (GenericPackageDescription, [String]))
unpackPackage tarFile contents doInstall = runPutMonad $ do
	unless (".tar.gz" `isSuffixOf` tarFile) $
		die $ tarFile ++ " is not a gzipped tar file"
	let pkgIdStr = take (length tarFile - 7) tarFile

	liftIO $ setFileCreationMask fileMask

	PackageIdentifier pName pVersion <- case readPackageId pkgIdStr of
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

	let pkgDir = localFile (packageDir pkgId)
	let installedTarFile = localFile (packageFile pkgId)
	let installedCabalFile = localFile (cabalFile pkgId)

	-- Do not allow replacing of existing packages (for security)
	tarPresent <- liftIO $ doesFileExist installedTarFile
	cabalPresent <- liftIO $ doesFileExist installedCabalFile
	when (tarPresent && cabalPresent) $
		die "this version of the package is already present in the database"
	extraChecks pkgDesc tmpPkgDir

	-- Install the new package
	when doInstall $ liftIO $ do
		ensureDirectoryExists pkgDir
		myCopyFile tmpTarFile installedTarFile
		myCopyFile tmpCabalFile installedCabalFile

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
