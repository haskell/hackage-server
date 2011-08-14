module Distribution.Server.Features.Upload (
    UploadFeature,
    uploadResource,
    UploadResource(..),
    packageMaintainers,
    canUploadPackage,
    uploadPackage,
    initUploadFeature,

    getPackageGroup,
    withPackageAuth,
    withPackageNameAuth,
    withTrusteeAuth,
    UploadResult(..),
    extractPackage,
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Framework.BackupDump
import Distribution.Server.Packages.Backup
import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import Distribution.Server.Users.Backup
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Packages.PackageIndex (PackageIndex)

import Data.Monoid (mconcat)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..))
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Text (display, simpleParse)

data UploadFeature = UploadFeature {
    uploadResource   :: UploadResource,
    uploadPackage    :: ServerPartE UploadResult,
    packageMaintainers :: GroupGen,
    trusteeGroup :: UserGroup,
    canUploadPackage :: Filter (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
}

data UploadResource = UploadResource {
    uploadIndexPage :: Resource,
    deletePackagePage  :: Resource,
    packageGroupResource :: GroupResource,
    trusteeResource :: GroupResource,
    packageMaintainerUri :: String -> PackageId -> String,
    trusteeUri :: String -> String
}

data UploadResult = UploadResult {
    uploadDesc :: !GenericPackageDescription,
    uploadCabal :: !ByteString,
    uploadWarnings :: ![String]
}

instance IsHackageFeature UploadFeature where
    getFeatureInterface upload = (emptyHackageFeature "upload") {
        featureResources = map ($uploadResource upload)
            [uploadIndexPage,
             groupResource . packageGroupResource, groupUserResource . packageGroupResource,
             groupResource . trusteeResource, groupUserResource . trusteeResource]
        -- TODO: backup maintainer groups, trustees
      , featureDumpRestore = Just (dumpBackup, restoreBackup)
      }
      where
        dumpBackup    = do
            trustees <- query GetHackageTrustees
            PackageMaintainers mains <- query AllPackageMaintainers
            return [ csvToBackup ["trustees.csv"] $ groupToCSV trustees
                   , maintToExport mains ]
        restoreBackup = mconcat [maintainerBackup, groupBackup ["trustees.csv"] ReplaceHackageTrustees]

initUploadFeature :: ServerEnv -> CoreFeature -> UserFeature -> IO UploadFeature
initUploadFeature env core users = do
    -- some shared tasks
    let store = serverBlobStore env
        admins = adminGroup users
    (trustees, trustResource) <- groupResourceAt users "/packages/trustees" (getTrusteesGroup [admins])

    groupPkgs <- fmap (Map.keys . maintainers) $ query AllPackageMaintainers
    let getPkgMaintainers dpath =
            let pkgname = case simpleParse =<< lookup "package" dpath of
                    Just name -> name
                    Nothing   -> error "Invalid package name"
            in  makeMaintainersGroup [admins, trustees] pkgname
        groupPaths = map (\pkgname -> [("package", display pkgname)]) groupPkgs
    (pkgGroup, pkgResource) <- groupResourcesAt users
        "/package/:package/maintainers" getPkgMaintainers groupPaths
    uploadFilter <- newHook
    registerHook (newPackageHook core) $ \pkg -> do
        let group = pkgGroup [("package", display $ packageName pkg)]
        exists <- groupExists group 
        -- create a maintainer group with the uploader if one didn't exist previously
        -- 
        when (not exists) $ addUserList group (pkgUploadUser pkg)
    return $ fix $ \f -> UploadFeature
      { uploadResource = UploadResource
          { uploadIndexPage = (extendResource . corePackagesPage $ coreResource core) { resourcePost = [] }
          , deletePackagePage = (extendResource . corePackagePage $ coreResource core) { resourceDelete = [] }
          , packageGroupResource = pkgResource
          , trusteeResource = trustResource

          , packageMaintainerUri = \format pkgname -> renderResource (groupResource pkgResource) [display pkgname, format]
          , trusteeUri = \format -> renderResource (groupResource trustResource) [format]
          }
      , uploadPackage = doUploadPackage core users f store
      , packageMaintainers = pkgGroup
      , trusteeGroup = trustees
      , canUploadPackage = uploadFilter
      }

--------------------------------------------------------------------------------
-- User groups and authentication
getTrusteesGroup :: [UserGroup] -> UserGroup
getTrusteesGroup canModify = fix $ \u -> UserGroup {
    groupDesc = trusteeDescription,
    queryUserList = query $ GetHackageTrustees,
    addUserList = update . AddHackageTrustee,
    removeUserList = update . RemoveHackageTrustee,
    groupExists = return True,
    canAddGroup = [u] ++ canModify,
    canRemoveGroup = canModify
}

makeMaintainersGroup :: [UserGroup] -> PackageName -> UserGroup
makeMaintainersGroup canModify name = fix $ \u -> UserGroup {
    groupDesc = maintainerDescription name,
    queryUserList = query $ GetPackageMaintainers name,
    addUserList = update . AddPackageMaintainer name,
    removeUserList = update . RemovePackageMaintainer name,
    groupExists = fmap (Map.member name . maintainers) $ query AllPackageMaintainers,
    canAddGroup = [u] ++ canModify,
    canRemoveGroup = canModify
  }

maintainerDescription :: PackageName -> GroupDescription
maintainerDescription pkgname = GroupDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (pname, Just $ "/package/" ++ pname)
  , groupPrologue  = "Maintainers for a package can upload new versions and adjust other attributes in the package database."
  }
  where pname = display pkgname

trusteeDescription :: GroupDescription
trusteeDescription = nullDescription { groupTitle = "Package trustees", groupPrologue = "Package trustees are essentially maintainers for the entire package database. They can edit package maintainer groups and upload any package." }

withPackageAuth :: Package pkg => pkg -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
withPackageAuth pkg func = withPackageNameAuth (packageName pkg) func

withPackageNameAuth :: PackageName -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
withPackageNameAuth pkgname func = do
    userDb <- query $ GetUserDb
    groupSum <- getPackageGroup pkgname
    Auth.withHackageAuth userDb (Just groupSum) func

withTrusteeAuth :: (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
withTrusteeAuth func = do
    userDb <- query $ GetUserDb
    trustee <- query $ GetHackageTrustees
    Auth.withHackageAuth userDb (Just trustee) func

getPackageGroup :: MonadIO m => PackageName -> m Group.UserList
getPackageGroup pkg = do
    pkgm    <- query $ GetPackageMaintainers pkg
    trustee <- query $ GetHackageTrustees
    return $ Group.unions [trustee, pkgm]

----------------------------------------------------

-- This is the upload function. It returns a generic result for multiple formats.
doUploadPackage :: CoreFeature -> UserFeature -> UploadFeature -> BlobStorage -> ServerPartE UploadResult
doUploadPackage core userf upf store = do
    state <- fmap packageList $ query GetPackagesState
    let uploadFilter uid info = combineErrors $ runFilter'' (canUploadPackage upf) uid info
    (pkgInfo, uresult) <- extractPackage (\uid info -> combineErrors $ sequence
       [ processUpload state uid info
       , uploadFilter uid info
       , runUserFilter userf uid ]) store
    success <- liftIO $ doAddPackage core pkgInfo
    if success
      then do
         -- make package maintainers group for new package
        let existedBefore = packageExists state pkgInfo
        when (not existedBefore) $ do
            update $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
        return uresult
      -- this is already checked in processUpload, and race conditions are highly unlikely but imaginable
      else errForbidden "Upload failed" [MText "Package already exists."]
  where combineErrors = fmap (listToMaybe . catMaybes)

-- This is a processing funtion for extractPackage that checks upload-specific requirements.
-- Does authentication, though not with requirePackageAuth, because it has to be IO.
-- Some other checks can be added, e.g. if a package with a later version exists
processUpload :: PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)
processUpload state uid res = do
    let pkg = packageId (uploadDesc res)
    pkgGroup <- getPackageGroup $ packageName pkg
    if packageIdExists state pkg
        then uploadError "Package name and version already exist in the database" --allow trustees to do this?
        else if packageExists state pkg && not (uid `Group.member` pkgGroup)
            then uploadError "Not authorized to upload a new version of this package"
            else return Nothing
  where uploadError = return . Just . ErrorResponse 403 "Upload failed" . return . MText

-- This function generically extracts a package, useful for uploading, checking,
-- and anything else in the standard user-upload pipeline.
extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)) -> BlobStorage -> ServerPartE (PkgInfo, UploadResult)
extractPackage processFunc storage =
    withDataFn (lookInput "package") $ \input ->
        case inputValue input of -- HS6 this has been updated to use the new file upload support in HS6, but has not been tested at all
          (Right _) -> errBadRequest "Upload failed" [MText "package field in form data is not a file."]
          (Left file) -> 
              let fileName    = (fromMaybe "noname" $ inputFilename input)
              in upload fileName file
  where
    upload name file = query GetUserDb >>= \users -> 
                          -- initial check to ensure logged in.
                          Auth.withHackageAuth users Nothing $ \uid _ -> do
        let processPackage :: ByteString -> IO (Either ErrorResponse UploadResult)
            processPackage content' = do
                -- as much as it would be nice to do requirePackageAuth in here,
                -- processPackage is run in a handle bracket
                case Upload.unpackPackage name content' of
                  Left err -> return . Left $ ErrorResponse 400 "Invalid package" [MText err]
                  Right ((pkg, pkgStr), warnings) -> do
                    let uresult = UploadResult pkg pkgStr warnings
                    res <- processFunc uid uresult
                    case res of
                        Nothing -> return . Right $ uresult
                        Just err -> return . Left $ err
        mres <- liftIO $ BlobStorage.addFileWith storage file processPackage
        case mres of
            Left  err -> throwError err
            Right (res@(UploadResult pkg pkgStr _), blobId) -> do
                uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
                return $ (PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgDesc       = pkg,
                    pkgData       = pkgStr,
                    pkgTarball    = [(blobId, uploadData)],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }, res)

