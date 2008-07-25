{-# LANGUAGE PatternGuards #-}
-- Unpack a tarball containing a Cabal package
module Unpack (unpackPackage) where

--import qualified Distribution.Server.Tar as Tar

import Control.Monad		( unless, when )
import Control.Monad.Error	( ErrorT(..), throwError )
import Control.Monad.Reader	( ReaderT(..), ask )
import Control.Monad.Writer	( WriterT(..), tell )
import Control.Monad.Trans      ( MonadIO(liftIO) )
import Data.ByteString.Lazy as BS
				( ByteString, writeFile )
import Data.List		( nub, (\\), partition )
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
import System.FilePath		( (</>), (<.>), splitExtension )

import Control.Exception	( bracket )
import System.Directory
         ( createDirectory, doesFileExist, removeDirectoryRecursive )
import System.Random            ( getStdGen, setStdGen, RandomGen(next) )

-- | Upload or check a tarball containing a Cabal package.
-- Returns either an fatal error or a package description and a list
-- of warnings.
unpackPackage :: FilePath -> ByteString
              -> IO (Either String (GenericPackageDescription, [String]))
unpackPackage tarGzFile contents = runPutMonad $ do
	let (pkgIdStr, ext) = (base, tar ++ gz)
              where (tarFile, gz) = splitExtension tarGzFile
                    (base,   tar) = splitExtension tarFile
	unless (ext == ".tar.gz") $
		die $ tarGzFile ++ " is not a gzipped tar file"

	PackageIdentifier pName pVersion <- case simpleParse pkgIdStr of
	    Just pkgId | display pkgId == pkgIdStr -> return pkgId
	    _ -> die $ "malformed package identifier " ++ pkgIdStr

	tmpDir <- getTmpDir
	-- save a copy of the tar file
	let tmpTarFile = tmpDir </> tarGzFile
	liftIO $ BS.writeFile tmpTarFile contents

	-- unpack package dir <package>-<version> the tarball
	systemOrFail
		"tar" ["-C", tmpDir, "-xzf", tmpTarFile, pkgIdStr]
		("could not extract " ++ pkgIdStr ++ " directory from " ++ tarGzFile)

	let srcCabalFile = pkgIdStr </> pName <.> "cabal"
	let tmpPkgDir = tmpDir </> pkgIdStr
	let tmpCabalFile = tmpDir </> srcCabalFile
	cabalIncluded <- liftIO $ doesFileExist tmpCabalFile
	when (not cabalIncluded) $
		die $ "could not extract " ++ srcCabalFile ++ " from " ++ tarGzFile

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

  --FIXME: lookup in the existing index to check if it's already present
  --       but watch out for race conditions!
{-
	let installedTarFile = packageFile pkgId
	let installedCabalFile = cabalFile pkgId

	-- Do not allow replacing of existing packages (for security)
	tarPresent <- liftIO $ doesFileExist installedTarFile
	cabalPresent <- liftIO $ doesFileExist installedCabalFile
	when (tarPresent && cabalPresent) $
		die "this version of the package is already present in the database"
-}
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

{-
checkPackageTarSanity :: PackageIdentifier -> Tar.Entries -> Either String Tar.Entry
checkPackageTarSanity pkgid = check Nothing
  where
    expectedCabalPath = display pkgid
                    </> packageName pkgid <.> "cabal"

    check _        (Tar.Fail err)  = Left err
    check Nothing  Tar.Done        = Left "No .cabal file in the package"
    check (Just c) Tar.Done        = Right c
    check _        (Tar.Next e _)
      | Just err <- entryProblem e = Left err
    check c (Tar.Next e es)
      | Tar.fileName e == expectedCabalPath
      = case c of
          Nothing  -> check (Just e) es
          Just _   -> Left "multiple .cabal files"
      | otherwise  = check c es
    
    entryProblem e
      | Tar.fileType e /= Tar.NormalFile
     && Tar.fileType e /= Tar.Directory = Just "unexpected file kind in the tarball"
    --TODO: check sanity of file names, must start with pkgid/ or ./pkgid/
      | otherwise = Nothing
-}
