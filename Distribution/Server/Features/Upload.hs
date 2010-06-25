module Distribution.Server.Features.Upload (
    UploadFeature(..),
    UploadResource(..),
    initUploadFeature,

    getPackageGroup,
    requirePackageAuth,
    UploadResult(..),
    UploadFailed(..),
    extractPackage,
    packageExists,
    packageIdExists,
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Hook
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
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO(..))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Package
import Distribution.PackageDescription (GenericPackageDescription, 
                                        packageDescription, synopsis, description)
import Distribution.Text (display, simpleParse)

data UploadFeature = UploadFeature {
    uploadResource   :: UploadResource,
    uploadPackage    :: BlobStorage -> ServerPart (Either UploadFailed UploadResult),
    maintainersGroup :: DynamicPath -> IO (Maybe UserGroup),
    trusteeGroup :: UserGroup
}

data UploadResource = UploadResource {
    uploadIndexPage :: Resource,
    deletePackagePage :: Resource,
    deindexPackagePage :: Resource
}

data UploadResult = UploadResult { uploadDesc :: !GenericPackageDescription, uploadCabal :: !ByteString, uploadWarnings :: ![String] }
data UploadFailed = UploadFailed { failedCode :: Int, failedMessage :: String }

instance HackageFeature UploadFeature where
    getFeature upload = HackageModule
      { featureName = "upload"
      , resources   = map ($uploadResource upload) [uploadIndexPage, deletePackagePage]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initUploadFeature :: CoreFeature -> IO UploadFeature
initUploadFeature core = do
    return $ UploadFeature
      { uploadResource = UploadResource
          { uploadIndexPage = (extendResource . corePackagesPage $ coreResource core) { resourcePost = [("txt", textUploadPackage)] }
            -- deleting package versions for good, admin-only, only in the most spammy of circumstances
          , deletePackagePage = (extendResource . corePackagePage $ coreResource core) { resourceDelete = [] }
            -- deprecating - removing packages from the central index, though still keeping it around, in
            -- case any dependencies exist. cabal-install should not pick solutions involving it
            -- if it can be avoided. GET = form to deprecate (html-only), PUT = do deprecation (or POST?)
          , deindexPackagePage = (resourceAt "/package/:package/indexed.:format") { resourceGet = [], resourcePut = [] }
          }
      , uploadPackage = doUploadPackage (packageIndexChange core)
      , maintainersGroup = getMaintainersGroup
      , trusteeGroup = UserGroup {
            groupDesc = trusteeDescription,
            queryUserList = query $ GetHackageTrustees,
            addUserList = update . AddHackageTrustee,
            removeUserList = update . RemoveHackageTrustee
        }
      }
  where
    textUploadPackage config _ = do
        res <- doUploadPackage (packageIndexChange core) (serverStore config)
        case res of
            Left (UploadFailed code err) -> resp code $ toResponse err
            Right uresult -> ok $ toResponse $ unlines (uploadWarnings uresult)

getMaintainersGroup :: DynamicPath -> IO (Maybe UserGroup)
getMaintainersGroup dpath = case fmap pkgName (simpleParse =<< lookup "package" dpath) of
  Nothing -> return Nothing
  Just name -> do 
    pkgstate <- query GetPackagesState
    case PackageIndex.lookupPackageName (packageList pkgstate) name of
      []   -> return Nothing
      pkgs -> do
        let pkgInfo = maximumBy (comparing packageVersion) pkgs -- is this really needed?
        return . Just $ UserGroup {
            groupDesc = maintainerDescription pkgInfo,
            queryUserList = query $ GetPackageMaintainers name,
            addUserList = update . AddPackageMaintainer name,
            removeUserList = update . RemovePackageMaintainer name
        }

-- Authentication and user groups
maintainerDescription :: PkgInfo -> GroupDescription
maintainerDescription pkgInfo = GroupDescription
  { groupTitle = "Maintainers for " ++ pname
  , groupShort = short
  , groupEntityURL = "/package/" ++ pname
  , groupPrologue  = description pkg
  }
  where
    pkg = packageDescription (pkgDesc pkgInfo)
    short = synopsis pkg
    pname = display (packageName pkgInfo)

trusteeDescription :: GroupDescription
trusteeDescription = nullDescription
  { groupTitle = "Package trustees"
  , groupEntityURL = "/packages"
  }

requirePackageAuth :: (MonadIO m, Package pkg) => pkg -> ServerPartT m (Users.UserId, Users.UserInfo)
requirePackageAuth pkg = do
    userDb <- query $ GetUserDb
    groupSum <- getPackageGroup pkg
    Auth.requireHackageAuth userDb (Just groupSum) Nothing

getPackageGroup :: (MonadIO m, Package pkg) => pkg -> m Group.UserList
getPackageGroup pkg = do
    pkgm    <- query $ GetPackageMaintainers (packageName pkg)
    trustee <- query $ GetHackageTrustees
    return $ Group.unions [trustee, pkgm]

-- This is the upload function. It returns a generic result for multiple formats.
doUploadPackage :: HookList (IO ()) -> BlobStorage -> ServerPart (Either UploadFailed UploadResult)
doUploadPackage hook store = do
    state <- fmap packageList $ query GetPackagesState
    res <- extractPackage (processUpload state) store
    case res of
        Left failed -> return $ Left failed
        Right (pkgInfo, uresult) -> do
            success <- update $ InsertPkgIfAbsent pkgInfo
            if success
              then do
                 -- make package maintainers group for new package
                unless (packageExists state pkgInfo) $ update $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
                liftIO $ runZeroHook hook
                return . Right $ uresult
              -- this is already checked in processUpload, and race conditions are highly unlikely but imaginable
              else return . Left $ UploadFailed 403 "Package already exists."

-- This is a processing funtion for extractPackage that checks upload-specific requirements.
-- Does authentication, though not with requirePackageAuth, because it has to be IO.
-- Some other checks can be added, e.g. if a package with a later version exists
processUpload :: PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe UploadFailed)
processUpload state uid res = do
    let pkg = packageId (uploadDesc res)
    pkgGroup <- getPackageGroup pkg
    if packageIdExists state pkg
        then return . Just $ UploadFailed 403 "Package name and version already exist in the database" --allow trustees to do this?
        else if packageExists state pkg && not (uid `Group.member` pkgGroup)
            then return . Just $ UploadFailed 403 "Not authorized to upload a new version of this package"
            else return Nothing

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   state pkg = not . null $ PackageIndex.lookupPackageName state (packageName pkg)
packageIdExists state pkg = maybe False (const True) $ PackageIndex.lookupPackageId state (packageId pkg)

-- This function generically extracts a package, useful for uploading, checking,
-- and anything else in the standard user-upload pipeline.
extractPackage :: (Users.UserId -> UploadResult -> IO (Maybe UploadFailed)) -> BlobStorage
                -> ServerPart (Either UploadFailed (PkgInfo, UploadResult))
extractPackage processFunc storage =
    withDataFn (lookInput "package") $ \input ->
          let fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in upload fileName fileContent
  where
    upload name content = do
        -- initial check to ensure logged in.
        users <- query GetUserDb
        (uid, _) <- Auth.requireHackageAuth users Nothing Nothing
        let processPackage :: ByteString -> IO (Either UploadFailed UploadResult)
            processPackage content' = do
                -- as much as it would be nice to do requirePackageAuth in here,
                -- processPackage is run in a handle bracket
                case Upload.unpackPackage name content' of
                  Left err -> return . Left $ UploadFailed 400 err
                  Right ((pkg, pkgStr), warnings) -> do
                    let uresult = UploadResult pkg pkgStr warnings
                    res <- processFunc uid uresult
                    case res of
                        Nothing -> return . Right $ uresult
                        Just err -> return . Left $ err
        mres <- liftIO $ BlobStorage.addWith storage content processPackage
        case mres of
            Left  err -> return $ Left err
            Right (res@(UploadResult pkg pkgStr _), blobId) -> do
                uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
                return . Right $ (PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgDesc       = pkg,
                    pkgData       = pkgStr,
                    pkgTarball    = [(blobId, uploadData)],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }, res)


{-
Old monolithic function

doUploadPackage :: HookList (IO ()) -> Config -> DynamicPath -> ServerPart Response
doUploadPackage hook config _ =
  methodSP POST $
    withDataFn (lookInput "package") $ \input ->
          let {-withEntry = do str <- look "entry"
                               case simpleParse str of
                                 Nothing -> fail "no parse"
                                 Just x  -> return (x::UploadLog.Entry)-}
              fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in upload fileName fileContent
  where
    upload :: FilePath -> ByteString -> ServerPart Response
    upload name content = do
        -- initial check to ensure logged in.
        users <- query GetUserDb
        state <- query GetPackagesState
        uid <- Auth.requireHackageAuth users Nothing Nothing
        let processPackage :: ByteString -> IO (Either String ((GenericPackageDescription, ByteString), [String]))
            processPackage content' = do
                -- as much as it would be nice to do requirePackageAuth in here,
                -- processPackage is run in a handle bracket
                case Upload.unpackPackage name content' of
                  Left err -> return . Left $ err
                  Right unpacked@((pkg, _), _) -> do
                    -- authentication that queries user group state
                    pkgGroup <- getPackageGroup pkg
                    if not (packageIdExists state pkg)
                      then return . Left $ "Package name and version already exist in the database" --allow trustees to do this?
                      else if not (packageExists state pkg) || uid `Group.member` pkgGroup
                        then return . Left $ "Not authorized to upload a new version of this package"
                        else return . Right $ unpacked
        res <- liftIO $ BlobStorage.addWith (serverStore config) content processPackage
        case res of
            Left  err -> return $ toResponse err --TODO: get specific response codes (without making a content-typed response)
            Right (((pkg, pkgStr), warnings), blobId) -> do
                let pkgExists = packageExists state pkg
                -- this should be the same as uid.
                user <- uploadingUser state pkg
                uploadData <- do now <- liftIO getCurrentTime
                                 return (now, user)
                success <- update $ InsertPkgIfAbsent PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgDesc       = pkg,
                    pkgData       = pkgStr,
                    pkgTarball    = [(blobId, uploadData)],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }
                -- Note: even if rejected for indexing (race condition), the package is still in the store,
                -- and removing it might make happstack-state replaying difficult. Is this approach okay?
                if success
                   then do
                     -- make package maintainers group for new package
                     unless pkgExists $ update $ AddPackageMaintainer (packageName pkg) user
                     liftIO $ runZeroHook hook
                     ok $ toResponse $ unlines warnings
                   else forbidden $ toResponse "Package already exists."

    -- A new package may be upped by anyone; an existing package may only be uploaded by a maintainer of
    -- that package or a trustee.
    --
    -- This function is currently unused because processPackage takes care of this, albeit by routing
    -- around requirePackageAuth, since IO.
    uploadingUser state pkg =
      if packageExists state pkg
        then requirePackageAuth pkg
        else query GetUserDb >>= \users -> Auth.requireHackageAuth users Nothing Nothing
-}
