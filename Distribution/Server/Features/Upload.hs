{-# LANGUAGE RecursiveDo, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Upload (
    UploadFeature(..),
    UploadResource(..),
    initUploadFeature,
    UploadResult(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Upload.State
import Distribution.Server.Features.Upload.Backup

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.Backup
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.Maybe (fromMaybe)
import Data.List (dropWhileEnd)
import Data.Time.Clock (getCurrentTime)
import Data.Function (fix)
import Data.ByteString.Lazy (ByteString)

import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Version (Version(..))
import Distribution.Text (display)
import qualified Distribution.Server.Util.GZip as GZip


data UploadFeature = UploadFeature {
    -- | The package upload `HackageFeature`.
    uploadFeatureInterface :: HackageFeature,

    -- | Upload resources.
    uploadResource     :: UploadResource,
    -- | The main upload routine. This uses extractPackage on a multipart
    -- request to get contextual information.
    uploadPackage      :: ServerPartE UploadResult,

    --TODO: consider moving the trustee and/or per-package maintainer groups
    --      lower down in the feature hierarchy; many other features want to
    --      use the trustee group purely for auth decisions
    -- | The group of Hackage trustees.
    trusteesGroup      :: UserGroup,
    -- | The group of package uploaders.
    uploadersGroup     :: UserGroup,
    -- | The group of maintainers for a given package.
    maintainersGroup   :: PackageName -> UserGroup,

    -- | Requiring being logged in as the maintainer of a package.
    guardAuthorisedAsMaintainer          :: PackageName -> ServerPartE (),
    -- | Requiring being logged in as the maintainer of a package or a trustee.
    guardAuthorisedAsMaintainerOrTrustee :: PackageName -> ServerPartE (),

    -- | Takes an upload request and, depending on the result of the
    -- passed-in function, either commits the uploaded tarball to the blob
    -- storage or throws it away and yields an error.
    extractPackage     :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
                       -> ServerPartE (Users.UserId, UploadResult, PkgTarball)
}

instance IsHackageFeature UploadFeature where
    getFeatureInterface = uploadFeatureInterface

data UploadResource = UploadResource {
    -- | The page for uploading a package, the same as `corePackagesPage`.
    uploadIndexPage :: Resource,
    -- | The page for deleting a package, the same as `corePackagePage`.
    --
    -- This is fairly dangerous and is not currently used.
    deletePackagePage  :: Resource,
    -- | The maintainers group for each package.
    maintainersGroupResource :: GroupResource,
    -- | The trustee group.
    trusteesGroupResource    :: GroupResource,
    -- | The allowed-uploaders group.
    uploadersGroupResource   :: GroupResource,

    -- | URI for `maintainersGroupResource` given a format and `PackageId`.
    packageMaintainerUri :: String -> PackageId -> String,
    -- | URI for `trusteesGroupResource` given a format.
    trusteeUri  :: String -> String,
    -- | URI for `uploadersGroupResource` given a format.
    uploaderUri :: String -> String
}

-- | The representation of an intermediate result in the upload process,
-- indicating a package which meets the requirements to go into Hackage.
data UploadResult = UploadResult {
    -- The parsed Cabal file.
    uploadDesc :: !GenericPackageDescription,
    -- The text of the Cabal file.
    uploadCabal :: !ByteString,
    -- Any warnings from unpacking the tarball.
    uploadWarnings :: ![String]
}

initUploadFeature :: ServerEnv
                  -> IO (UserFeature -> CoreFeature -> IO UploadFeature)
initUploadFeature env@ServerEnv{serverStateDir} = do
    -- Canonical state
    trusteesState    <- trusteesStateComponent    serverStateDir
    uploadersState   <- uploadersStateComponent   serverStateDir
    maintainersState <- maintainersStateComponent serverStateDir

    return $ \user@UserFeature{..} core@CoreFeature{..} -> do

      -- Recusively tie the knot: the feature contains new user group resources
      -- but we make the functions needed to create those resources along with
      -- the feature
      rec let (feature,
               trusteesGroupDescription, uploadersGroupDescription,
               maintainersGroupDescription)
                = uploadFeature env core user
                                trusteesState    trusteesGroup    trusteesGroupResource
                                uploadersState   uploadersGroup   uploadersGroupResource
                                maintainersState maintainersGroup maintainersGroupResource

          (trusteesGroup,  trusteesGroupResource) <-
            groupResourceAt "/packages/trustees"  trusteesGroupDescription

          (uploadersGroup, uploadersGroupResource) <-
            groupResourceAt "/packages/uploaders" uploadersGroupDescription

          pkgNames <- PackageIndex.packageNames <$> queryGetPackageIndex
          (maintainersGroup, maintainersGroupResource) <-
            groupResourcesAt "/package/:package/maintainers"
                             maintainersGroupDescription
                             (\pkgname -> [("package", display pkgname)])
                             (packageInPath coreResource)
                             pkgNames

      return feature

trusteesStateComponent :: FilePath -> IO (StateComponent AcidState HackageTrustees)
trusteesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageTrustees") initialHackageTrustees
  return StateComponent {
      stateDesc    = "Trustees"
    , stateHandle  = st
    , getState     = query st GetHackageTrustees
    , putState     = update st . ReplaceHackageTrustees . trusteeList
    , backupState  = \_ (HackageTrustees trustees) -> [csvToBackup ["trustees.csv"] $ groupToCSV trustees]
    , restoreState = HackageTrustees <$> groupBackup ["trustees.csv"]
    , resetState   = trusteesStateComponent
    }

uploadersStateComponent :: FilePath -> IO (StateComponent AcidState HackageUploaders)
uploadersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageUploaders") initialHackageUploaders
  return StateComponent {
      stateDesc    = "Uploaders"
    , stateHandle  = st
    , getState     = query st GetHackageUploaders
    , putState     = update st . ReplaceHackageUploaders . uploaderList
    , backupState  = \_ (HackageUploaders uploaders) -> [csvToBackup ["uploaders.csv"] $ groupToCSV uploaders]
    , restoreState = HackageUploaders <$> groupBackup ["uploaders.csv"]
    , resetState   = uploadersStateComponent
    }

maintainersStateComponent :: FilePath -> IO (StateComponent AcidState PackageMaintainers)
maintainersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PackageMaintainers") initialPackageMaintainers
  return StateComponent {
      stateDesc    = "Package maintainers"
    , stateHandle  = st
    , getState     = query st AllPackageMaintainers
    , putState     = update st . ReplacePackageMaintainers
    , backupState  = \_ (PackageMaintainers mains) -> [maintToExport mains]
    , restoreState = maintainerBackup
    , resetState   = maintainersStateComponent
    }

uploadFeature :: ServerEnv
              -> CoreFeature
              -> UserFeature
              -> StateComponent AcidState HackageTrustees    -> UserGroup -> GroupResource
              -> StateComponent AcidState HackageUploaders   -> UserGroup -> GroupResource
              -> StateComponent AcidState PackageMaintainers -> (PackageName -> UserGroup) -> GroupResource
              -> (UploadFeature,
                  UserGroup,
                  UserGroup,
                  PackageName -> UserGroup)

uploadFeature ServerEnv{serverBlobStore = store}
              CoreFeature{ coreResource
                         , queryGetPackageIndex
                         , updateAddPackage
                         }
              UserFeature{..}
              trusteesState    trusteesGroup    trusteesGroupResource
              uploadersState   uploadersGroup   uploadersGroupResource
              maintainersState maintainersGroup maintainersGroupResource
   = ( UploadFeature {..}
     , trusteesGroupDescription, uploadersGroupDescription, maintainersGroupDescription)
   where
    uploadFeatureInterface = (emptyHackageFeature "upload") {
        featureDesc = "Support for package uploads, and define groups for trustees, uploaders, and package maintainers"
      , featureResources =
            [ uploadIndexPage uploadResource
            , groupResource     maintainersGroupResource
            , groupUserResource maintainersGroupResource
            , groupResource     trusteesGroupResource
            , groupUserResource trusteesGroupResource
            , groupResource     uploadersGroupResource
            , groupUserResource uploadersGroupResource
            ]
      , featureState = [
            abstractAcidStateComponent trusteesState
          , abstractAcidStateComponent uploadersState
          , abstractAcidStateComponent maintainersState
          ]
      }

    uploadResource = UploadResource
          { uploadIndexPage      = (extendResource (corePackagesPage coreResource)) {
              resourcePost =
                [ ("txt", \_ -> uploadPlain)
                ]
            }
          , deletePackagePage    = (extendResource (corePackagePage coreResource))  { resourceDelete = [] }
          , maintainersGroupResource = maintainersGroupResource
          , trusteesGroupResource    = trusteesGroupResource
          , uploadersGroupResource   = uploadersGroupResource

          , packageMaintainerUri = \format pkgname -> renderResource
                                     (groupResource maintainersGroupResource) [display pkgname, format]
          , trusteeUri  = \format -> renderResource (groupResource trusteesGroupResource)  [format]
          , uploaderUri = \format -> renderResource (groupResource uploadersGroupResource) [format]
          }


    uploadPlain :: ServerPartE Response
    uploadPlain = nullDir >> do
      upResult <- uploadPackage
      ok $ toResponse $ unlines $ uploadWarnings upResult

    --------------------------------------------------------------------------------
    -- User groups and authentication
    trusteesGroupDescription :: UserGroup
    trusteesGroupDescription = UserGroup {
        groupDesc             = trusteeDescription,
        queryUserGroup        = queryState  trusteesState   GetTrusteesList,
        addUserToGroup        = updateState trusteesState . AddHackageTrustee,
        removeUserFromGroup   = updateState trusteesState . RemoveHackageTrustee,
        groupsAllowedToAdd    = [adminGroup],
        groupsAllowedToDelete = [adminGroup]
    }

    uploadersGroupDescription :: UserGroup
    uploadersGroupDescription = UserGroup {
        groupDesc             = uploaderDescription,
        queryUserGroup        = queryState  uploadersState   GetUploadersList,
        addUserToGroup        = updateState uploadersState . AddHackageUploader,
        removeUserFromGroup   = updateState uploadersState . RemoveHackageUploader,
        groupsAllowedToAdd    = [adminGroup],
        groupsAllowedToDelete = [adminGroup]
    }

    maintainersGroupDescription :: PackageName -> UserGroup
    maintainersGroupDescription name =
      fix $ \thisgroup ->
      UserGroup {
        groupDesc             = maintainerDescription name,
        queryUserGroup        = queryState  maintainersState $ GetPackageMaintainers name,
        addUserToGroup        = updateState maintainersState . AddPackageMaintainer name,
        removeUserFromGroup   = updateState maintainersState . RemovePackageMaintainer name,
        groupsAllowedToAdd    = [thisgroup, adminGroup],
        groupsAllowedToDelete = [thisgroup, adminGroup]
      }

    maintainerDescription :: PackageName -> GroupDescription
    maintainerDescription pkgname = GroupDescription
      { groupTitle = "Maintainers"
      , groupEntity = Just (pname, Just $ "/package/" ++ pname)
      , groupPrologue  = "Maintainers for a package can upload new versions and adjust other attributes in the package database."
      }
      where pname = display pkgname

    trusteeDescription :: GroupDescription
    trusteeDescription = nullDescription { groupTitle = "Package trustees", groupPrologue = "The role of trustees is to help to curate the whole package collection. Trustees have a limited ability to edit package information, for the entire package database (as opposed to package maintainers who have full control over individual packages). Trustees can edit .cabal files, edit other package metadata and upload documentation but they cannot upload new package versions." }

    uploaderDescription :: GroupDescription
    uploaderDescription = nullDescription { groupTitle = "Package uploaders", groupPrologue = "Package uploaders are allowed to upload packages. Note that if a package already exists then you also need to be in the maintainer group for that package." }

    guardAuthorisedAsMaintainer :: PackageName -> ServerPartE ()
    guardAuthorisedAsMaintainer pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname)]

    guardAuthorisedAsMaintainerOrTrustee :: PackageName -> ServerPartE ()
    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

    ----------------------------------------------------

    -- This is the upload function. It returns a generic result for multiple formats.
    uploadPackage :: ServerPartE UploadResult
    uploadPackage = do
        guardAuthorised_ [InGroup uploadersGroup]
        pkgIndex <- queryGetPackageIndex
        (uid, uresult, tarball) <- extractPackage $ \uid info ->
                                     processUpload pkgIndex uid info
        now <- liftIO getCurrentTime
        let (UploadResult pkg pkgStr _) = uresult
            pkgid      = packageId pkg
            cabalfile  = CabalFileText pkgStr
            uploadinfo = (now, uid)
        success <- updateAddPackage pkgid cabalfile uploadinfo (Just tarball)
        if success
          then do
             -- make package maintainers group for new package
            let existedBefore = packageExists pkgIndex pkgid
            when (not existedBefore) $
                liftIO $ addUserToGroup (maintainersGroup (packageName pkgid)) uid
            return uresult
          -- this is already checked in processUpload, and race conditions are highly unlikely but imaginable
          else errForbidden "Upload failed" [MText "Package already exists."]

    -- This is a processing funtion for extractPackage that checks upload-specific requirements.
    -- Does authentication, though not with requirePackageAuth, because it has to be IO.
    -- Some other checks can be added, e.g. if a package with a later version exists
    processUpload :: PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)
    processUpload state uid res = do
        let pkg = packageId (uploadDesc res)
        pkgGroup <- queryUserGroup (maintainersGroup (packageName pkg))
        case () of
          _ | packageExists state pkg && not (uid `Group.member` pkgGroup)
           -> uploadError (notMaintainer pkg)

            | packageIdExists state pkg
           -> uploadError versionExists

            | packageIdExistsModuloNormalisedVersion state pkg
           -> uploadError normVerExists

            | otherwise
           -> return Nothing
      where
        uploadError = return . Just . ErrorResponse 403 [] "Upload failed" . return . MText
        versionExists = "This version of the package has already been uploaded.\n\nAs a matter of "
                     ++ "policy we do not allow package tarballs to be changed after a release "
                     ++ "(so we can guarantee stable md5sums etc). The usual recommendation is "
                     ++ "to upload a new version, and if necessary blacklist the existing one. "
                     ++ "In extraordinary circumstances, contact the administrators."
        normVerExists = "A version of the package has already been uploaded that differs only in "
                     ++ "trailing zeros.\n\nAs a matter of policy, to avoid confusion, we no "
                     ++ "longer not allow uploading different package versions that differ only "
                     ++ "in trailing zeros. For example if version 1.2.0 has been uploaded then "
                     ++ "version 1.2 cannot subsequently be upload. "
                     ++ "If this is a major problem please contact the administrators."
        notMaintainer pkg = "You are not authorised to upload new versions of this package. The "
                     ++ "package '" ++ display (packageName pkg) ++ "' exists already and you "
                     ++ "are not a member of the maintainer group for this package.\n\n"
                     ++ "If you believe you should be a member of the maintainer group for this "
                     ++ "package, then ask an existing maintainer to add you to the group. If "
                     ++ "this is a package name clash, please pick another name or talk to the "
                     ++ "maintainers of the existing package."

    -- This function generically extracts a package, useful for uploading, checking,
    -- and anything else in the standard user-upload pipeline.
    extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
                   -> ServerPartE (Users.UserId, UploadResult, PkgTarball)
    extractPackage processFunc =
        withDataFn (lookInput "package") $ \input ->
            case inputValue input of -- HS6 this has been updated to use the new file upload support in HS6, but has not been tested at all
              (Right _) -> errBadRequest "Upload failed" [MText "package field in form data is not a file."]
              (Left file) ->
                  let fileName    = (fromMaybe "noname" $ inputFilename input)
                  in upload fileName file
      where
        upload name file =
         do -- initial check to ensure logged in.
            --FIXME: this should have been covered earlier
            uid <- guardAuthenticated
            now <- liftIO getCurrentTime
            let processPackage :: ByteString -> IO (Either ErrorResponse (UploadResult, BlobStorage.BlobId))
                processPackage content' = do
                    -- as much as it would be nice to do requirePackageAuth in here,
                    -- processPackage is run in a handle bracket
                    case Upload.unpackPackage now name content' of
                      Left err -> return . Left $ ErrorResponse 400 [] "Invalid package" [MText err]
                      Right ((pkg, pkgStr), warnings) -> do
                        let uresult = UploadResult pkg pkgStr warnings
                        res <- processFunc uid uresult
                        case res of
                            Nothing ->
                                do let decompressedContent = GZip.decompressNamed file content'
                                   blobIdDecompressed <- BlobStorage.add store decompressedContent
                                   return . Right $ (uresult, blobIdDecompressed)
                            Just err -> return . Left $ err
            mres <- liftIO $ BlobStorage.consumeFileWith store file processPackage
            case mres of
                Left  err -> throwError err
                Right ((res, blobIdDecompressed), blobId) -> do
                    infoGz <- liftIO $ blobInfoFromId store blobId
                    let tarball = PkgTarball {
                                      pkgTarballGz   = infoGz
                                    , pkgTarballNoGz = blobIdDecompressed
                                    }
                    return (uid, res, tarball)

-- | Whether a particular version of package exists in the package index, but
-- where we consider versions with trailing 0s to be equivalent, e.g. 1.0
--
packageIdExistsModuloNormalisedVersion :: (Package pkg, Package pkg')
                                       => PackageIndex pkg -> pkg' -> Bool
packageIdExistsModuloNormalisedVersion pkgs pkg =
    elem (normalisedPackageId pkg)
         (map normalisedPackageId
              (PackageIndex.lookupPackageName pkgs (packageName pkg)))
  where
    normalisedPackageId :: Package pkg  => pkg -> PackageId
    normalisedPackageId pkg' = case packageId pkg' of
      PackageIdentifier name ver -> PackageIdentifier name (normaliseVersion ver)

    normaliseVersion :: Version -> Version
    normaliseVersion (Version vs _) = Version (n vs) []
      where
        n vs' = case dropWhileEnd (== 0) vs' of
            []   -> [0]
            vs'' -> vs''
