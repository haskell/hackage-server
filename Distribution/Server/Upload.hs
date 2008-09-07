{-# LANGUAGE PatternGuards #-}
-- Unpack a tarball containing a Cabal package
module Distribution.Server.Upload (unpackPackage) where

import qualified Distribution.Server.Util.Tar as Tar

import Distribution.Version
         ( Version(..) )
import Distribution.Package
         ( PackageIdentifier, packageVersion, packageName )
import Distribution.PackageDescription
         ( GenericPackageDescription(..), PackageDescription(..)
         , exposedModules )
import Distribution.PackageDescription
         ( parsePackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.PackageDescription.Check
         ( PackageCheck(..), checkPackage, checkPackageFiles )
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg, showPWarning )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( fromUTF8 )

import Data.List
         ( nub, (\\), partition, intercalate )
import Control.Monad
         ( unless, when )
import Control.Monad.Error () --intance Monad (Either String)
import Control.Monad.Writer
         ( WriterT(..), tell )
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8
         ( ByteString )
import System.FilePath
         ( (</>), (<.>), splitExtension, splitDirectories, normalise )
import qualified System.FilePath.Windows
         ( takeFileName )

-- | Upload or check a tarball containing a Cabal package.
-- Returns either an fatal error or a package description and a list
-- of warnings.
unpackPackage :: FilePath -> ByteString
              -> Either String
                        ((GenericPackageDescription, ByteString), [String])
unpackPackage tarGzFile contents = runUploadMonad $ do
  let (pkgidStr, ext) = (base, tar ++ gz)
        where (tarFile, gz) = splitExtension (portableTakeFileName tarGzFile)
              (base,   tar) = splitExtension tarFile
  unless (ext == ".tar.gz") $
    fail $ tarGzFile ++ " is not a gzipped tar file, it must have the .tar.gz extension"

  pkgid <- case simpleParse pkgidStr of
    Just pkgid
      | display pkgid == pkgidStr -> return pkgid

      | not . null . versionTags . packageVersion $ pkgid
      -> fail $ "Hackage no longer accepts packages with version tags: "
             ++ intercalate ", " (versionTags (packageVersion pkgid))
    _ -> fail $ "Invalid package id " ++ show pkgidStr
             ++ ". The tarball must use the name of the package."

  -- Check the tarball content for sanity and extract the .cabal file
  let checkEntry entry = checkTarFileType       entry
                      >> checkTarFilePath pkgid entry
      
      selectEntry entry = cabalFileName == normalise (Tar.fileName entry)
      cabalFileName     = display pkgid </> packageName pkgid <.> "cabal"
      entries           = Tar.read (GZip.decompress contents)
  cabalEntries <- extractTarEntries checkEntry selectEntry entries
  cabalEntry   <- case cabalEntries of
    [cabalEntry] -> return cabalEntry
    [] -> fail $ "The " ++ quote cabalFileName
              ++ " file is missing from the package tarball."
    _  -> fail $ "The tarball contains duplicate entries with the name "
              ++ quote cabalFileName ++ "."
  
  -- Parse the Cabal file
  let cabalFileContent = fromUTF8 (BS.unpack (Tar.fileContent cabalEntry))
  pkgDesc <- case parsePackageDescription cabalFileContent of
    ParseFailed err -> fail $ showError (locatedErrorMsg err)
    ParseOk warnings pkgDesc -> do
      mapM_ (fail . showPWarning cabalFileName) warnings
      return pkgDesc

  -- Check that the name and version in Cabal file match
  when (packageName pkgDesc /= packageName pkgid) $
    fail "Package name in the cabal file does not match the file name."
  when (packageVersion pkgDesc /= packageVersion pkgid) $
    fail "Package version in the cabal file does not match the file name."

  extraChecks pkgDesc

  return (pkgDesc, Tar.fileContent cabalEntry)

  where
    showError (Nothing, msg) = msg
    showError (Just n, msg) = "line " ++ show n ++ ": " ++ msg

-- | The issue is that browsers can upload the file name using either unix
-- or windows convention, so we need to take the basename using either
-- convention. Since windows allows the unix '/' as a separator then we can
-- use the Windows.takeFileName as a portable solution.
--
portableTakeFileName :: FilePath -> String
portableTakeFileName = System.FilePath.Windows.takeFileName

-- Miscellaneous checks on package description
extraChecks :: GenericPackageDescription -> UploadMonad ()
extraChecks genPkgDesc = do
  let pkgDesc = flattenPackageDescription genPkgDesc
  -- various checks

  --FIXME: do the content checks. The dev version of Cabal generalises
  -- checkPackageContent to work in any monad, we just need to provide
  -- a record of ops that will do checks inside the tarball. We should
  -- gather a map of files and dirs and have these just to map lookups:
  --
  -- > checkTarballContents = CheckPackageContentOps {
  -- >   doesFileExist      = Set.member fileMap,
  -- >   doesDirectoryExist = Set.member dirsMap
  -- > }
  -- > fileChecks <- checkPackageContent checkTarballContents pkgDesc

  let pureChecks = checkPackage genPkgDesc (Just pkgDesc)
      checks = pureChecks -- ++ fileChecks
      isDistError (PackageDistSuspicious {}) = False
      isDistError _                          = True
      (errors, warnings) = partition isDistError checks
  mapM_ (fail . explanation) errors
  mapM_ (warn . explanation) warnings

  -- Check reasonableness of names of exposed modules
  let badTopLevel =
          maybe [] (nub . map (takeWhile (/= '.')) . exposedModules)
                  (library pkgDesc) \\
          allocatedTopLevelNodes

  unless (null badTopLevel) $
          warn $ "Exposed modules use unallocated top-level names: " ++
                          unwords badTopLevel

-- Monad for uploading packages:
--      WriterT for warning messages
--      Either for fatal errors
type UploadMonad = WriterT [String] (Either String)

warn :: String -> UploadMonad ()
warn msg = tell [msg]

runUploadMonad :: UploadMonad a -> Either String (a, [String])
runUploadMonad m = runWriterT m

-- | Registered top-level nodes in the class hierarchy.
allocatedTopLevelNodes :: [String]
allocatedTopLevelNodes = [
        "Algebra", "Codec", "Control", "Data", "Database", "Debug",
        "Distribution", "DotNet", "Foreign", "Graphics", "Language",
        "Network", "Numeric", "Prelude", "Sound", "System", "Test", "Text"]

extractTarEntries :: (Tar.Entry -> UploadMonad ())
                  -> (Tar.Entry -> Bool)
                  -> Tar.Entries
                  -> UploadMonad [Tar.Entry]
extractTarEntries check select = extract []
  where
    extract _        (Tar.Fail err)           = fail err
    extract selected  Tar.Done                = return selected
    extract selected (Tar.Next entry entries) = do
      check entry 
      if select entry
        then extract (entry : selected) entries
        else extract          selected  entries

checkTarFileType :: Tar.Entry -> UploadMonad ()
checkTarFileType Tar.Entry { Tar.fileType = ftype }
  | ftype == Tar.NormalFile
 || ftype == Tar.Directory
 || ftype == Tar.HardLink
 || ftype == Tar.SymbolicLink
  = return ()

  | otherwise
  = fail $ "Bad file type in package tarball: " ++ show ftype
        ++ "\nFor portability, package tarballs should use the 'ustar' format "
        ++ "and only contain normal files, directories and file links. "
        ++ "Your tar program may be using non-standard extensions. For "
        ++ "example with GNU tar, use --format=ustar to get the portable "
        ++ "format."

checkTarFilePath :: PackageIdentifier -> Tar.Entry -> UploadMonad ()
checkTarFilePath pkgid = \entry -> do
  let dirs = splitDirectories (Tar.fileName entry)
  unless (all (/= "..") dirs) $
    fail $ "Bad file name in package tarball: " ++ quote (Tar.fileName entry)
        ++ "\nFor security reasons, files in package tarballs may not use"
        ++ " \"..\" components in their path." 
  unless (inPkgSubdir dirs) $
    fail $ "Bad file name in package tarball: " ++ quote (Tar.fileName entry)
        ++ "\nAll the file in the package tarball must be in the subdirectory "
        ++ quote pkgstr ++ "."
  where
    pkgstr = display pkgid
    inPkgSubdir (".":pkgstr':_) = pkgstr == pkgstr'
    inPkgSubdir (    pkgstr':_) = pkgstr == pkgstr'
    inPkgSubdir _               = False

quote :: String -> String
quote s = "'" ++ s ++ "'"
