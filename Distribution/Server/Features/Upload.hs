{-# LANGUAGE DoRec, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Upload (
    UploadFeature(..),
    UploadResource(..),
    initUploadFeature,
    UploadResult(..),
    uploadsRestrictedToMaintainers,
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

import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime)
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)

import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Text (display, simpleParse)
import qualified Codec.Compression.GZip as GZip


data UploadFeature = UploadFeature {
    uploadFeatureInterface :: HackageFeature,

    uploadResource   :: UploadResource,
    uploadPackage    :: ServerPartE UploadResult,
    packageMaintainers :: GroupGen,
    trusteeGroup :: UserGroup,
    uploaderGroup :: UserGroup,
    canUploadPackage :: Filter (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)),

    getPackageGroup :: forall m. MonadIO m => PackageName -> m Group.UserList,
    withPackageAuth :: forall pkg a. Package pkg => pkg -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a,
    withPackageNameAuth :: forall a. PackageName -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a,
    withTrusteeAuth     :: forall a. (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a,

    extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)) -> ServerPartE (PkgInfo, UploadResult)
}

instance IsHackageFeature UploadFeature where
    getFeatureInterface = uploadFeatureInterface

data UploadResource = UploadResource {
    uploadIndexPage :: Resource,
    deletePackagePage  :: Resource,
    packageGroupResource :: GroupResource,
    trusteeResource :: GroupResource,
    uploaderResource :: GroupResource,
    packageMaintainerUri :: String -> PackageId -> String,
    trusteeUri :: String -> String,
    uploaderUri :: String -> String
}

data UploadResult = UploadResult {
    uploadDesc :: !GenericPackageDescription,
    uploadCabal :: !ByteString,
    uploadWarnings :: ![String]
}

initUploadFeature :: ServerEnv -> CoreFeature -> UserFeature -> IO UploadFeature
initUploadFeature env@ServerEnv{serverStateDir}
                  core@CoreFeature{..} user@UserFeature{..} = do

    -- Canonical state
    trusteesState    <- openLocalStateFrom
                          (serverStateDir </> "db" </> "HackageTrustees")
                          initialHackageTrustees
    uploadersState   <- openLocalStateFrom
                          (serverStateDir </> "db" </> "HackageUploaders")
                          initialHackageUploaders
    maintainersState <- openLocalStateFrom
                          (serverStateDir </> "db" </> "PackageMaintainers")
                          initialPackageMaintainers


    -- some shared tasks
    let admins = adminGroup
        UserResource{..} = userResource

    uploadFilter <- newHook

    -- Recusively tie the knot: the feature contains new user group resources
    -- but we make the functions needed to create those resources along with
    -- the feature
    rec let (feature,
             getTrusteesGroup, getUploadersGroup, makeMaintainersGroup)
              = uploadFeature env core user
                              trusteesState    trustees  trustResource
                              uploadersState   uploaders uploaderResource'
                              maintainersState pkgGroup  pkgResource
                              uploadFilter

        (trustees,  trustResource) <-
          groupResourceAt "/packages/trustees"  (getTrusteesGroup  [admins])
        (uploaders, uploaderResource') <-
          groupResourceAt "/packages/uploaders" (getUploadersGroup [admins])

        groupPkgs <- fmap (Map.keys . maintainers) $ query maintainersState AllPackageMaintainers
        --TODO: move this local function inside uploadFeature,
        --      like getTrusteesGroup, getUploadersGroup etc.
        let getPkgMaintainers dpath =
                let pkgname = case simpleParse =<< lookup "package" dpath of
                        Just name -> name
                        Nothing   -> error "Invalid package name"
                in  makeMaintainersGroup [admins, trustees] pkgname
            groupPaths = map (\pkgname -> [("package", display pkgname)]) groupPkgs
        (pkgGroup, pkgResource) <- groupResourcesAt
            "/package/:package/maintainers" getPkgMaintainers groupPaths

    registerHook newPackageHook $ \pkg -> do
        let group = pkgGroup [("package", display $ packageName pkg)]
        exists <- groupExists group
        -- create a maintainer group with the uploader if one didn't exist previously
        --
        when (not exists) $ addUserList group (pkgUploadUser pkg)

    return feature

uploadFeature :: ServerEnv
              -> CoreFeature
              -> UserFeature
              -> AcidState HackageTrustees    -> UserGroup -> GroupResource
              -> AcidState HackageUploaders   -> UserGroup -> GroupResource
              -> AcidState PackageMaintainers -> GroupGen  -> GroupResource
              -> Filter (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse))
              -> (UploadFeature,
                  [UserGroup] -> UserGroup,
                  [UserGroup] -> UserGroup,
                  [UserGroup] -> PackageName -> UserGroup)

uploadFeature ServerEnv{serverBlobStore = store}
              CoreFeature{..} UserFeature{..}
              trusteesState    trusteeGroup       trustResource
              uploadersState   uploaderGroup      uploaderResource'
              maintainersState packageMaintainers pkgResource
              canUploadPackage
   = ( UploadFeature {..}
     , getTrusteesGroup, getUploadersGroup, makeMaintainersGroup)
   where
    uploadFeatureInterface = (emptyHackageFeature "upload") {
        featureResources = map ($uploadResource)
            [uploadIndexPage,
             groupResource . packageGroupResource, groupUserResource . packageGroupResource,
             groupResource . trusteeResource,      groupUserResource . trusteeResource,
             groupResource . uploaderResource,     groupUserResource . uploaderResource]
      , featureCheckpoint = do
          createCheckpoint trusteesState
          createCheckpoint uploadersState
          createCheckpoint maintainersState
      , featureShutdown = do
          closeAcidState trusteesState
          closeAcidState uploadersState
          closeAcidState maintainersState
      , featureDumpRestore = Just (dumpBackup, restoreBackup, testRoundtrip)
      }

    dumpBackup    = do
        trustees  <- query trusteesState GetHackageTrustees
        uploaders <- query uploadersState GetHackageUploaders
        PackageMaintainers mains <- query maintainersState AllPackageMaintainers
        return [ csvToBackup ["trustees.csv"]  $ groupToCSV trustees
               , csvToBackup ["uploaders.csv"] $ groupToCSV uploaders
               , maintToExport mains ]
    restoreBackup = mconcat [ maintainerBackup maintainersState
                            , groupBackup uploadersState ["uploaders.csv"] ReplaceHackageUploaders
                            , groupBackup trusteesState  ["trustees.csv"]  ReplaceHackageTrustees ]
    testRoundtrip = testRoundtripByQuery (liftM2 (,) (query trusteesState GetHackageTrustees)
                                                     (query maintainersState AllPackageMaintainers))

    uploadResource = UploadResource
          { uploadIndexPage      = (extendResource (corePackagesPage coreResource)) { resourcePost = [] }
          , deletePackagePage    = (extendResource (corePackagePage coreResource))  { resourceDelete = [] }
          , packageGroupResource = pkgResource
          , trusteeResource      = trustResource
          , uploaderResource     = uploaderResource'

          , packageMaintainerUri = \format pkgname -> renderResource (groupResource pkgResource) [display pkgname, format]
          , trusteeUri  = \format -> renderResource (groupResource trustResource)     [format]
          , uploaderUri = \format -> renderResource (groupResource uploaderResource') [format]
          }

    --------------------------------------------------------------------------------
    -- User groups and authentication
    getTrusteesGroup :: [UserGroup] -> UserGroup
    getTrusteesGroup canModify = fix $ \u -> UserGroup {
        groupDesc = trusteeDescription,
        queryUserList  = query  trusteesState   GetHackageTrustees,
        addUserList    = update trusteesState . AddHackageTrustee,
        removeUserList = update trusteesState . RemoveHackageTrustee,
        groupExists    = return True,
        canAddGroup    = [u] ++ canModify,
        canRemoveGroup = canModify
    }

    getUploadersGroup :: [UserGroup] -> UserGroup
    getUploadersGroup canModify = UserGroup {
        groupDesc      = uploaderDescription,
        queryUserList  = query  uploadersState   GetHackageUploaders,
        addUserList    = update uploadersState . AddHackageUploader,
        removeUserList = update uploadersState . RemoveHackageUploader,
        groupExists    = return True,
        canAddGroup    = canModify,
        canRemoveGroup = canModify
    }

    makeMaintainersGroup :: [UserGroup] -> PackageName -> UserGroup
    makeMaintainersGroup canModify name = fix $ \u -> UserGroup {
        groupDesc      = maintainerDescription name,
        queryUserList  = query  maintainersState $ GetPackageMaintainers name,
        addUserList    = update maintainersState . AddPackageMaintainer name,
        removeUserList = update maintainersState . RemovePackageMaintainer name,
        groupExists    = fmap (Map.member name . maintainers) $ query maintainersState AllPackageMaintainers,
        canAddGroup    = [u] ++ canModify,
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

    uploaderDescription :: GroupDescription
    uploaderDescription = nullDescription { groupTitle = "Package uploaders", groupPrologue = "Package uploaders allowed to upload packages. If a package already exists then you also need to be in the maintainer group for that package." }

    withPackageAuth :: Package pkg => pkg -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
    withPackageAuth pkg func =
      withPackageNameAuth (packageName pkg) func

    withPackageNameAuth :: PackageName -> (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
    withPackageNameAuth pkgname func = do
        userDb <- queryGetUserDb
        groupSum <- getPackageGroup pkgname
        (uid, uinfo) <- guardAuthorised hackageRealm userDb groupSum
        func uid uinfo

    withTrusteeAuth :: (Users.UserId -> Users.UserInfo -> ServerPartE a) -> ServerPartE a
    withTrusteeAuth func = do
        userDb <- queryGetUserDb
        trustee <- query' trusteesState GetHackageTrustees
        (uid, uinfo) <- guardAuthorised hackageRealm userDb trustee
        func uid uinfo

    getPackageGroup :: MonadIO m => PackageName -> m Group.UserList
    getPackageGroup pkg = do
        pkgm    <- query' maintainersState (GetPackageMaintainers pkg)
        trustee <- query' trusteesState GetHackageTrustees
        return $ Group.unions [trustee, pkgm]

    ----------------------------------------------------

    -- This is the upload function. It returns a generic result for multiple formats.
    uploadPackage :: ServerPartE UploadResult
    uploadPackage = do
        users     <- queryGetUserDb
        uploaders <- query' uploadersState GetHackageUploaders
        void $ guardAuthorised hackageRealm users uploaders
        pkgIndex <- queryGetPackageIndex
        let uploadFilter uid info = combineErrors $ runFilter'' canUploadPackage uid info
        (pkgInfo, uresult) <- extractPackage (\uid info -> combineErrors $ sequence
           [ processUpload pkgIndex uid info
           , uploadFilter uid info
           , runUserFilter uid ])
        success <- liftIO $ doAddPackage pkgInfo
        if success
          then do
             -- make package maintainers group for new package
            let existedBefore = packageExists pkgIndex pkgInfo
            when (not existedBefore) $ do
                update' maintainersState $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
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
            else -- This check is disabled for now: As long as you are in
                 -- the uploaders group, you can upload any package
                if uploadsRestrictedToMaintainers && packageExists state pkg && not (uid `Group.member` pkgGroup)
                then uploadError "Not authorized to upload a new version of this package"
                else return Nothing
      where uploadError = return . Just . ErrorResponse 403 "Upload failed" . return . MText

    -- This function generically extracts a package, useful for uploading, checking,
    -- and anything else in the standard user-upload pipeline.
    extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)) -> ServerPartE (PkgInfo, UploadResult)
    extractPackage processFunc =
        withDataFn (lookInput "package") $ \input ->
            case inputValue input of -- HS6 this has been updated to use the new file upload support in HS6, but has not been tested at all
              (Right _) -> errBadRequest "Upload failed" [MText "package field in form data is not a file."]
              (Left file) ->
                  let fileName    = (fromMaybe "noname" $ inputFilename input)
                  in upload fileName file
      where
        upload name file =
         do users <- queryGetUserDb
            -- initial check to ensure logged in.
            (uid, _) <- guardAuthenticated hackageRealm users
            let processPackage :: ByteString -> IO (Either ErrorResponse (UploadResult, BlobStorage.BlobId))
                processPackage content' = do
                    -- as much as it would be nice to do requirePackageAuth in here,
                    -- processPackage is run in a handle bracket
                    case Upload.unpackPackage name content' of
                      Left err -> return . Left $ ErrorResponse 400 "Invalid package" [MText err]
                      Right ((pkg, pkgStr), warnings) -> do
                        let uresult = UploadResult pkg pkgStr warnings
                        res <- processFunc uid uresult
                        case res of
                            Nothing ->
                                do let decompressedContent = GZip.decompress content'
                                   blobIdDecompressed <- BlobStorage.add store decompressedContent
                                   return . Right $ (uresult, blobIdDecompressed)
                            Just err -> return . Left $ err
            mres <- liftIO $ BlobStorage.addFileWith store file processPackage
            case mres of
                Left  err -> throwError err
                Right ((res@(UploadResult pkg pkgStr _), blobIdDecompressed), blobId) -> do
                    uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
                    return $ (PkgInfo {
                        pkgInfoId     = packageId pkg,
                        pkgData       = CabalFileText pkgStr,
                        pkgTarball    = [(PkgTarball { pkgTarballGz = blobId,
                                                       pkgTarballNoGz = blobIdDecompressed },
                                          uploadData)],
                        pkgUploadData = uploadData,
                        pkgDataOld    = []
                    }, res)

--FIXME: reverse this
uploadsRestrictedToMaintainers :: Bool
uploadsRestrictedToMaintainers = False

