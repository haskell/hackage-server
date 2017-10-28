-- Unpack a tarball containing a Cabal package
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Server.Packages.Unpack (
    CombinedTarErrs(..),
    checkEntries,
    checkUselessPermissions,
    unpackPackage,
    unpackPackageRaw,
  ) where

import Distribution.Server.Prelude

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Check as Tar

import Distribution.Version
         ( nullVersion, mkVersion )
import Distribution.Types.PackageName
         ( mkPackageName, unPackageName )
import Distribution.Package
         ( PackageIdentifier, packageVersion, packageName, PackageName )
import Distribution.PackageDescription
         ( GenericPackageDescription(..), PackageDescription(..)
         , allBuildInfo, allLibraries
         , mixins, signatures, specVersion
         )
import Distribution.PackageDescription.Parse
         ( parseGenericPackageDescription )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.PackageDescription.Check
         ( PackageCheck(..), checkPackage, CheckPackageContentOps(..)
         , checkPackageContent )
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg, showPWarning )
import Distribution.Text
         ( Text(..), display, simpleParse )
import Distribution.Server.Util.Parse
         ( unpackUTF8 )
import Distribution.Server.Util.ParseSpecVer
import Distribution.License
         ( License(..) )
import qualified Distribution.Compat.ReadP as Parse

import Control.Monad.Except
         ( ExceptT, runExceptT, MonadError, throwError )
import Control.Monad.Identity
         ( Identity(..) )
import Control.Monad.Writer
         ( WriterT(..), MonadWriter, tell )
import Data.Bits
         ( (.&.) )
import Data.ByteString.Lazy
         ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8
import Data.List
         ( nub, partition, intercalate, isPrefixOf )
import qualified Data.Map.Strict as Map
         ( fromList, lookup )
import Data.Time
         ( UTCTime(..), fromGregorian, addUTCTime )
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import qualified Data.Version
import qualified Distribution.Server.Util.GZip as GZip
import System.FilePath
         ( (</>), (<.>), splitDirectories, splitExtension, normalise )
import qualified System.FilePath.Windows
         ( takeFileName )
import qualified System.FilePath.Posix
         ( takeFileName, takeDirectory, addTrailingPathSeparator
         , dropTrailingPathSeparator )
import qualified Text.PrettyPrint as Disp
import Text.Printf
         ( printf )

-- Whether to allow upload of "all rights reserved" packages
allowAllRightsReserved :: Bool
allowAllRightsReserved = True

-- | Upload or check a tarball containing a Cabal package.
-- Returns either an fatal error or a package description and a list
-- of warnings.
unpackPackage :: UTCTime -> FilePath -> ByteString
              -> Either String
                        ((GenericPackageDescription, ByteString), [String])
unpackPackage now tarGzFile contents =
  runUploadMonad $ do
    (pkgId, tarIndex) <- tarPackageChecks False now tarGzFile contents
    (pkgDesc, warnings, cabalEntry) <- basicChecks pkgId tarIndex
    mapM_ throwError warnings
    extraChecks pkgDesc pkgId tarIndex
    return (pkgDesc, cabalEntry)

unpackPackageRaw :: FilePath -> ByteString
                 -> Either String
                           ((GenericPackageDescription, ByteString), [String])
unpackPackageRaw tarGzFile contents =
  runUploadMonad $ do
    (pkgId, tarIndex) <- tarPackageChecks True noTime tarGzFile contents
    (pkgDesc, _warnings, cabalEntry) <- basicChecks pkgId tarIndex
    return (pkgDesc, cabalEntry)
  where
    noTime = UTCTime (fromGregorian 1970 1 1) 0

data TaggedPackageId = TaggedPackageId {
        _taggedPkgName   :: PackageName,
        taggedPkgVersion :: Data.Version.Version
    }

instance Text TaggedPackageId where
    disp (TaggedPackageId n v)
        | v == Data.Version.Version [] [] = disp n
        | otherwise = disp n Disp.<> Disp.char '-' Disp.<> disp v

    parse = do
        n <- parse
        v <- (Parse.char '-' >> parse) Parse.<++ return (Data.Version.Version [] [])
        return (TaggedPackageId n v)

tarPackageChecks :: Bool -> UTCTime -> FilePath -> ByteString
                 -> UploadMonad (PackageIdentifier, TarIndex)
tarPackageChecks lax now tarGzFile contents = do
  let (pkgidStr, ext) = (base, tar ++ gz)
        where (tarFile, gz) = splitExtension (portableTakeFileName tarGzFile)
              (base,   tar) = splitExtension tarFile
  unless (ext == ".tar.gz") $
    throwError $ tarGzFile ++ " is not a gzipped tar file, it must have the .tar.gz extension"

  let versionTags (Data.Version.Version _ ts) = ts

  pkgid <- case (simpleParse pkgidStr, simpleParse pkgidStr) of
    (Just pkgid, Just tagged_pkgid)
      | (== nullVersion) . packageVersion $ pkgid
      -> throwError $ "Invalid package id " ++ quote pkgidStr
                   ++ ". It must include the package version number, and not just "
                   ++ "the package name, e.g. 'foo-1.0'."

      | display pkgid == pkgidStr -> return (pkgid :: PackageIdentifier)

      -- NB: we have to use 'TaggedPackageId' here, because the 'PackageId'
      -- parser will drop tags.
      | not . null . versionTags . taggedPkgVersion $ tagged_pkgid
      -> throwError $ "Hackage no longer accepts packages with version tags: "
                   ++ intercalate ", " (versionTags (taggedPkgVersion tagged_pkgid))

    _ -> throwError $ "Invalid package id " ++ quote pkgidStr
                   ++ ". The tarball must use the name of the package."

  -- Extract entries and check the tar format / portability
  let entries = tarballChecks lax now expectedDir
              $ Tar.read (GZip.decompressNamed tarGzFile contents)
      expectedDir = display pkgid

      selectEntry entry = case Tar.entryContent entry of
        Tar.NormalFile bs _         -> Just (normalise (Tar.entryPath entry), NormalFile bs)
        Tar.Directory               -> Just (normalise (Tar.entryPath entry), Directory)
        Tar.SymbolicLink linkTarget -> Just (normalise (Tar.entryPath entry), Link (Tar.fromLinkTarget linkTarget))
        Tar.HardLink     linkTarget -> Just (normalise (Tar.entryPath entry), Link (Tar.fromLinkTarget linkTarget))
        _                           -> Nothing
  files <- selectEntries explainTarError selectEntry entries
  return (pkgid, files)

type TarIndex = [(FilePath, File)]
data File = Directory | NormalFile ByteString | Link FilePath deriving Show

basicChecks :: PackageIdentifier
            -> TarIndex
            -> UploadMonad (GenericPackageDescription, [String], ByteString)
basicChecks pkgid tarIndex = do
  -- Extract the .cabal file from the tarball
  let cabalEntries = [ content | (fp, NormalFile content) <- tarIndex
                               , fp == cabalFileName ]
      name  = unPackageName (packageName pkgid)
      cabalFileName     = display pkgid </> name <.> "cabal"
  cabalEntry   <- case cabalEntries of
    -- NB: tar files *can* contain more than one entry for the same filename.
    -- (This was observed in practice with the package CoreErlang-0.0.1).
    -- In this case, after extracting the tar the *last* file in the archive
    -- wins. Since selectEntries returns results in reverse order we use the head:
    cabalEntry:_ -> -- We tend to keep hold of the .cabal file, but
                    -- cabalEntry itself is part of a much larger
                    -- ByteString (the whole tar file), so we make a
                    -- copy of it
                    return $ LBS.copy cabalEntry
    [] -> throwError $ "The " ++ quote cabalFileName
                    ++ " file is missing from the package tarball."

  when (startsWithBOM cabalEntry) $
    throwError $ "The cabal file starts with a Unicode byte order mark (BOM), "
              ++ "which causes problems for older versions of cabal. Please "
              ++ "save the package's cabal file as UTF8 without the BOM."

  -- Parse the Cabal file
  let cabalFileContent = unpackUTF8 cabalEntry
  (pkgDesc, warnings) <- case parseGenericPackageDescription cabalFileContent of
    ParseFailed err -> throwError $ showError (locatedErrorMsg err)
    ParseOk warnings pkgDesc ->
      return (pkgDesc, map (showPWarning cabalFileName) warnings)

  -- make sure the parseSpecVer heuristic agrees with the full parser
  let specVer' = parseSpecVerLazy cabalEntry
      specVer  = specVersion $ packageDescription pkgDesc

  when (specVer' < mkVersion [1] || specVer /= specVer') $
    throwError "The 'cabal-version' field could not be properly parsed."

  -- Check that the name and version in Cabal file match
  when (packageName pkgDesc /= packageName pkgid) $
    throwError "Package name in the cabal file does not match the file name."
  when (packageVersion pkgDesc /= packageVersion pkgid) $
    throwError "Package version in the cabal file does not match the file name."

  -- check for reserved/magic package names
  when (packageName pkgid `elem` reservedPkgNames) $
    throwError "Package name is reserved."

  return (pkgDesc, warnings, cabalEntry)

  where
    showError (Nothing, msg) = msg
    showError (Just n, msg) = "line " ++ show n ++ ": " ++ msg

    -- these names are reserved for the time being, as they have
    -- special meaning in cabal's UI
    reservedPkgNames = map mkPackageName ["all","any","none","setup","lib","exe","test"]

-- | The issue is that browsers can upload the file name using either unix
-- or windows convention, so we need to take the basename using either
-- convention. Since windows allows the unix '/' as a separator then we can
-- use the Windows.takeFileName as a portable solution.
--
portableTakeFileName :: FilePath -> String
portableTakeFileName = System.FilePath.Windows.takeFileName

tarOps :: PackageIdentifier -> TarIndex -> CheckPackageContentOps UploadMonad
tarOps pkgId tarIndex = CheckPackageContentOps {
  doesFileExist        = fileExist    . relative,
  doesDirectoryExist   = dirExist . relative . System.FilePath.Posix.addTrailingPathSeparator,
  getDirectoryContents = dirContents . System.FilePath.Posix.dropTrailingPathSeparator . relative,
  getFileContents      = fileContents . relative
}
  where
    -- The tar index has names like <pkgid>/foo.cabal, but the
    -- CheckPackageContentOps requests files without specifying the pkgid
    -- root. We convert the requested file paths into the tar index format.
    relative = normalise . (display pkgId </>)
    -- Build the map. In case of multiple intries for a file, we want the
    -- last entry in the tar file to win (per tar append-to-update semantics).
    -- Since the tarIndex list is the reversed tar file, we need to reverse it
    -- back since with Map.fromList later entries win.
    fileMap = Map.fromList (reverse tarIndex)

    resolvePath :: Int -> FilePath -> Either String (Maybe File)
    resolvePath 0 path =
      Left ("Too many links redirects when looking for file " ++ quote path)
    resolvePath n path =
      case Map.lookup path fileMap of
        Just (Link fp) -> resolvePath (n-1) fp
        Just entry -> Right (Just entry)
        Nothing -> Right Nothing

    fileExist path =
      case resolvePath 10 path of
        Left err -> throwError err
        Right (Just NormalFile{}) -> return True
        Right _ -> return False
    dirExist path =
      case resolvePath 10 path of
        Left err -> throwError err
        Right (Just Directory) -> return True
        -- Some .tar files miss some directory entries, though it has files in
        -- those directories. That's enough for the directory to be created,
        -- thus we should consider it to exist.
        _ -> return (any ((path `isPrefixOf`) . fst) tarIndex)
    -- O(n). Only used once to find all .cabal files in the package root. Some
    -- .tar files have duplicate entries for the same .cabal file, so we use
    -- nub.
    dirContents dir =
      return (nub [ fileName
                  | (fp, _) <- tarIndex
                  , System.FilePath.Posix.takeDirectory fp == dir
                  , let fileName = System.FilePath.Posix.takeFileName fp
                  , fileName /= "" ])
    fileContents path =
      case Map.lookup path fileMap of
        Just (NormalFile contents) ->
          return (Data.ByteString.Lazy.Char8.unpack contents)
        Just (Link fp) -> fileContents fp
        _ -> throwError ("getFileContents: file does not exist: " ++ path)

-- Miscellaneous checks on package description
extraChecks :: GenericPackageDescription
            -> PackageIdentifier
            -> TarIndex
            -> UploadMonad ()
extraChecks genPkgDesc pkgId tarIndex = do
  let pkgDesc = flattenPackageDescription genPkgDesc
  fileChecks <- checkPackageContent (tarOps pkgId tarIndex) pkgDesc

  let pureChecks = checkPackage genPkgDesc (Just pkgDesc)
      checks = pureChecks ++ fileChecks
      isDistError (PackageDistSuspicious     {}) = False -- just a warning
      isDistError (PackageDistSuspiciousWarn {}) = False -- just a warning
      isDistError _                              = True
      (errors, warnings) = partition isDistError checks
  mapM_ (throwError . explanation) errors
  mapM_ (warn . explanation) warnings

  -- Proprietary License check (only active in central-server branch)
  when (not allowAllRightsReserved && license pkgDesc == AllRightsReserved) $
    throwError $ "This server does not accept packages with 'license' "
              ++ "field set to AllRightsReserved."

  -- Check for an existing x-revision
  when (isJust (lookup "x-revision" (customFieldsPD pkgDesc))) $
    throwError $ "Newly uploaded packages must not specify the 'x-revision' "
              ++ "field in their .cabal file. This is only used for "
              ++ "post-release revisions."

  -- Check for experimental Backpack features
  let usesBackpackInc  = any (not . null . mixins) (allBuildInfo pkgDesc)
      usesBackpackSig  = any (not . null . signatures) (allLibraries pkgDesc)

  when (usesBackpackInc || usesBackpackSig) $
    throwError $ "Packages using experimental Backpack features "
              ++ "(i.e. mixins or signatures) are not yet allowed on Hackage. "
              ++ "Please use http://next.hackage.haskell.org:8080/ if you "
              ++ "want to help testing Backpack in the meantime."

  return ()

-- Monad for uploading packages:
--      WriterT for warning messages
--      Either for fatal errors
newtype UploadMonad a = UploadMonad (WriterT [String] (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadWriter [String], MonadError String)

warn :: String -> UploadMonad ()
warn msg = tell [msg]

runUploadMonad :: UploadMonad a -> Either String (a, [String])
runUploadMonad (UploadMonad m) = runIdentity . runExceptT . runWriterT $ m

selectEntries :: forall err a.
                 (err -> String)
              -> (Tar.Entry -> Maybe a)
              -> Tar.Entries err
              -> UploadMonad [a]
selectEntries formatErr select = extract []
  where
    extract :: [a] -> Tar.Entries err -> UploadMonad [a]
    extract _        (Tar.Fail err)           = throwError (formatErr err)
    extract selected  Tar.Done                = return selected
    extract selected (Tar.Next entry entries) =
      case select entry of
        Nothing    -> extract          selected  entries
        Just saved -> extract (saved : selected) entries

data CombinedTarErrs =
     FormatError      Tar.FormatError
   | PortabilityError Tar.PortabilityError
   | TarBombError     FilePath FilePath
   | FutureTimeError  FilePath UTCTime UTCTime
   | PermissionsError FilePath Tar.Permissions

tarballChecks :: Bool -> UTCTime -> FilePath
              -> Tar.Entries Tar.FormatError
              -> Tar.Entries CombinedTarErrs
tarballChecks lax now expectedDir =
    (if not lax then checkFutureTimes now else id)
  . checkTarbomb expectedDir
  . (if not lax then checkUselessPermissions else id)
  . (if lax then ignoreShortTrailer
            else fmapTarError (either id PortabilityError)
               . Tar.checkPortability)
  . fmapTarError FormatError
  where
    ignoreShortTrailer =
      Tar.foldEntries Tar.Next Tar.Done
                      (\e -> case e of
                               FormatError Tar.ShortTrailer -> Tar.Done
                               _                            -> Tar.Fail e)
    fmapTarError f = Tar.foldEntries Tar.Next Tar.Done (Tar.Fail . f)

checkFutureTimes :: UTCTime
                 -> Tar.Entries CombinedTarErrs
                 -> Tar.Entries CombinedTarErrs
checkFutureTimes now =
    checkEntries checkEntry
  where
    -- Allow 30s for client clock skew
    now' = addUTCTime 30 now
    checkEntry entry
      | entryUTCTime > now'
      = Just (FutureTimeError posixPath entryUTCTime now')
      where
        entryUTCTime = posixSecondsToUTCTime (realToFrac (Tar.entryTime entry))
        posixPath    = Tar.fromTarPathToPosixPath (Tar.entryTarPath entry)

    checkEntry _ = Nothing

checkTarbomb :: FilePath -> Tar.Entries CombinedTarErrs -> Tar.Entries CombinedTarErrs
checkTarbomb expectedTopDir =
    checkEntries checkEntry
  where
    checkEntry entry =
      case splitDirectories (Tar.entryPath entry) of
        (topDir:_) | topDir == expectedTopDir -> Nothing
        _ -> Just $ TarBombError (Tar.entryPath entry) expectedTopDir

checkUselessPermissions :: Tar.Entries CombinedTarErrs -> Tar.Entries CombinedTarErrs
checkUselessPermissions =
    checkEntries checkEntry
  where
    checkEntry entry =
      case Tar.entryContent entry of
        (Tar.NormalFile _ _) -> checkPermissions 0o644 (Tar.entryPermissions entry)
        (Tar.Directory) -> checkPermissions 0o755 (Tar.entryPermissions entry)
        _ -> Nothing
      where
        checkPermissions expected actual =
            if expected .&. actual /= expected
                then Just $ PermissionsError (Tar.entryPath entry) actual
                else Nothing


checkEntries :: (Tar.Entry -> Maybe e) -> Tar.Entries e -> Tar.Entries e
checkEntries checkEntry =
  Tar.foldEntries (\entry rest -> maybe (Tar.Next entry rest) Tar.Fail
                                        (checkEntry entry))
                  Tar.Done Tar.Fail

explainTarError :: CombinedTarErrs -> String
explainTarError (TarBombError filename expectedDir) =
    "Bad file name in package tarball: " ++ quote filename
 ++ "\nAll the file in the package tarball must be in the subdirectory "
 ++ quote expectedDir ++ "."
explainTarError (PortabilityError (Tar.NonPortableFormat Tar.GnuFormat)) =
    "This tarball is in the non-standard GNU tar format. "
 ++ "For portability and long-term data preservation, hackage requires that "
 ++ "package tarballs use the standard 'ustar' format. If you are using GNU "
 ++ "tar, use --format=ustar to get the standard portable format."
explainTarError (PortabilityError (Tar.NonPortableFormat Tar.V7Format)) =
    "This tarball is in the old Unix V7 tar format. "
 ++ "For portability and long-term data preservation, hackage requires that "
 ++ "package tarballs use the standard 'ustar' format. Virtually all tar "
 ++ "programs can now produce ustar format (POSIX 1988). For example if you "
 ++ "are using GNU tar, use --format=ustar to get the standard portable format."
explainTarError (PortabilityError (Tar.NonPortableFormat Tar.UstarFormat)) =
    error "explainTarError: impossible UstarFormat"
explainTarError (PortabilityError Tar.NonPortableFileType) =
    "The package tarball contains a non-portable entry type. "
 ++ "For portability, package tarballs should use the 'ustar' format "
 ++ "and only contain normal files, directories and file links."
explainTarError (PortabilityError (Tar.NonPortableEntryNameChar _)) =
    "The package tarball contains an entry with a non-ASCII file name. "
 ++ "For portability, package tarballs should contain only ASCII file names "
 ++ "(e.g. not UTF8 encoded Unicode)."
explainTarError (PortabilityError (err@Tar.NonPortableFileName {})) =
    show err
 ++ ". For portability, hackage requires that file names be valid on both Unix "
 ++ "and Windows systems, and not refer outside of the tarball."
explainTarError (FormatError formateror) =
    "There is an error in the format of the tar file: " ++ show formateror
 ++ ". Check that it is a valid tar file (e.g. 'tar -xtf thefile.tar'). "
 ++ "You may need to re-create the package tarball and try again."
explainTarError (FutureTimeError entryname time serverTime) =
    "The tarball entry " ++ quote entryname ++ " has a file timestamp that is "
 ++ "in the future (" ++ show time ++ " vs this server's time of " ++ show serverTime
 ++ "). This tends to cause problems for build systems and other tools, so hackage "
 ++ "does not allow it. This problem can be caused by having a misconfigured system "
 ++ "time, or by bugs in the tools (tarballs created by 'cabal sdist' on Windows "
 ++ "with cabal-install-1.18.0.2 or older have this problem)."
explainTarError (PermissionsError entryname mode) =
    "The tarball entry " ++ quote entryname ++ " has file permissions that are "
 ++ "broken: " ++ (showMode mode) ++ ". Permissions must be 644 at a minimum "
 ++ "for files and 755 for directories."
  where
    showMode :: Tar.Permissions -> String
    showMode m = printf "%.3o" (fromIntegral m :: Int)

quote :: String -> String
quote s = "'" ++ s ++ "'"

-- | Whether a UTF8 BOM is at the beginning of the input
startsWithBOM :: ByteString -> Bool
startsWithBOM bs = LBS.take 3 bs == LBS.pack [0xEF, 0xBB, 0xBF]
