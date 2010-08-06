module Distribution.Server.Features.Upload (
    UploadFeature(..),
    UploadResource(..),
    initUploadFeature,

    getPackageGroup,
    withPackageAuth,
    withPackageNameAuth,
    withTrusteeAuth,
    UploadResult(..),
    extractPackage,
    packageExists,
    packageIdExists,
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Error
import Distribution.Server.Types

import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Packages.Unpack as Upload
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.PackageIndex (PackageIndex)

import Happstack.Server
import Happstack.State
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (when, mzero)
import Control.Monad.Trans (MonadIO(..))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Text (display, simpleParse)

data UploadFeature = UploadFeature {
    uploadResource   :: UploadResource,
    uploadPackage    :: MServerPart UploadResult,
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

data UploadResult = UploadResult { uploadDesc :: !GenericPackageDescription, uploadCabal :: !ByteString, uploadWarnings :: ![String] }

instance HackageFeature UploadFeature where
    getFeature upload = HackageModule
      { featureName = "upload"
      , resources   = map ($uploadResource upload)
            [uploadIndexPage,
             groupResource . packageGroupResource, groupUserResource . packageGroupResource,
             groupResource . trusteeResource, groupUserResource . trusteeResource]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initUploadFeature :: Config -> CoreFeature -> UserFeature -> IO UploadFeature
initUploadFeature config core users = do
    -- some shared tasks
    let store = serverStore config
        admins = adminGroup users
    (trustees, trustResource) <- groupResourceAt  (groupIndex users) "/packages/trustees" (getTrusteesGroup [admins])
    let getPkgMaintainers dpath = case simpleParse =<< lookup "package" dpath :: Maybe PackageName of
            Nothing   -> mzero
            Just name -> getMaintainersGroup [admins, trustees] name
    -- FIXME: get the entire package list
    (pkgGroup, pkgResource) <- groupResourcesAt (groupIndex users) "/package/:package/maintainers" getPkgMaintainers []
    uploadFilter <- newHook
    return $ fix $ \f -> UploadFeature
      { uploadResource = UploadResource
          { uploadIndexPage = (extendResource . corePackagesPage $ coreResource core)
          , deletePackagePage = (extendResource . corePackagePage $ coreResource core)
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
    canAddGroup = [u] ++ canModify,
    canRemoveGroup = canModify
}

getMaintainersGroup :: [UserGroup] -> PackageName -> MIO UserGroup
getMaintainersGroup canModify name = do
    pkgstate <- query GetPackagesState
    case PackageIndex.lookupPackageName (packageList pkgstate) name of
      []   -> returnErrorIo 404 "Not found" [MText $ "No package with the name " ++ display name ++ " found"]
      pkgs -> do
        let pkgInfo = maximumBy (comparing packageVersion) pkgs -- is this really needed?
        returnOk . fix $ \u -> UserGroup {
            groupDesc = maintainerDescription pkgInfo,
            queryUserList = query $ GetPackageMaintainers name,
            addUserList = update . AddPackageMaintainer name,
            removeUserList = update . RemovePackageMaintainer name,
            canAddGroup = [u] ++ canModify,
            canRemoveGroup = canModify
        }

maintainerDescription :: PkgInfo -> GroupDescription
maintainerDescription pkgInfo = GroupDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (pname, Just $ "/package/" ++ pname)
  , groupPrologue  = "Maintainers for a package can upload new versions and adjust other attributes in the package database."
  }
  where pname = display (packageName pkgInfo)

trusteeDescription :: GroupDescription
trusteeDescription = nullDescription { groupTitle = "Package trustees", groupPrologue = "Package trustees are essential maintainers for the entire package database. They can edit package maintainer groups and upload any package." }

withPackageAuth :: Package pkg => pkg -> (Users.UserId -> Users.UserInfo -> MServerPart a) -> MServerPart a
withPackageAuth pkg func = withPackageNameAuth (packageName pkg) func

withPackageNameAuth :: PackageName -> (Users.UserId -> Users.UserInfo -> MServerPart a) -> MServerPart a
withPackageNameAuth pkgname func = do
    userDb <- query $ GetUserDb
    groupSum <- getPackageGroup pkgname
    Auth.withHackageAuth userDb (Just groupSum) Nothing func

withTrusteeAuth :: (Users.UserId -> Users.UserInfo -> MServerPart a) -> MServerPart a
withTrusteeAuth func = do
    userDb <- query $ GetUserDb
    trustee <- query $ GetHackageTrustees
    Auth.withHackageAuth userDb (Just trustee) Nothing func

getPackageGroup :: MonadIO m => PackageName -> m Group.UserList
getPackageGroup pkg = do
    pkgm    <- query $ GetPackageMaintainers pkg
    trustee <- query $ GetHackageTrustees
    return $ Group.unions [trustee, pkgm]

----------------------------------------------------
-- This is the upload function. It returns a generic result for multiple formats.
doUploadPackage :: CoreFeature -> UserFeature -> UploadFeature -> BlobStorage -> MServerPart UploadResult
doUploadPackage core userf upf store = do
    state <- fmap packageList $ query GetPackagesState
    let uploadFilter uid info = combineErrors $ runFilter'' (canUploadPackage upf) uid info
    res <- extractPackage (\uid info -> combineErrors $ sequence
       [ processUpload state uid info
       , uploadFilter uid info
       , runUserFilter userf uid ]) store
    case res of
        Left failed -> return $ Left failed
        Right (pkgInfo, uresult) -> do
            success <- update $ InsertPkgIfAbsent pkgInfo
            if success
              then do
                 -- make package maintainers group for new package
                let existedBefore = packageExists state pkgInfo
                when (not existedBefore) $ do
                    update $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
                    runHook' (newPackageHook core) pkgInfo
                liftIO $ runHook' (packageAddHook core) pkgInfo
                liftIO $ runHook (packageIndexChange core)
                returnOk uresult
              -- this is already checked in processUpload, and race conditions are highly unlikely but imaginable
              else returnError 403 "Upload failed" [MText "Package already exists."]
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

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   state pkg = not . null $ PackageIndex.lookupPackageName state (packageName pkg)
packageIdExists state pkg = maybe False (const True) $ PackageIndex.lookupPackageId state (packageId pkg)

-- This function generically extracts a package, useful for uploading, checking,
-- and anything else in the standard user-upload pipeline.
extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)) -> BlobStorage -> MServerPart (PkgInfo, UploadResult)
extractPackage processFunc storage =
    withDataFn (lookInput "package") $ \input ->
          let fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in upload fileName fileContent
  where
    upload name content = query GetUserDb >>= \users -> 
                          -- initial check to ensure logged in.
                          Auth.withHackageAuth users Nothing Nothing $ \uid _ -> do
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
        mres <- liftIO $ BlobStorage.addWith storage content processPackage
        case mres of
            Left  err -> returnError' err
            Right (res@(UploadResult pkg pkgStr _), blobId) -> do
                uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
                returnOk $ (PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgDesc       = pkg,
                    pkgData       = pkgStr,
                    pkgTarball    = [(blobId, uploadData)],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }, res)

