module Distribution.Server.Features.Upload (
    UploadFeature(..),
    initUploadFeature,
    requirePackageAuth
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
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Packages.Unpack as Upload
import qualified Distribution.Server.PackageIndex as PackageIndex

import Happstack.Server
import Happstack.State
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (msum, unless)
import Control.Monad.Trans (MonadIO(..))
import Data.List (maximumBy)
import Data.Ord (comparing)
import Distribution.Package
import Distribution.PackageDescription (packageDescription, synopsis, description)
import Distribution.Text (display, simpleParse)

data UploadFeature = UploadFeature {
    uploadResource   :: UploadResource,
    maintainersGroup :: DynamicPath -> IO (Maybe UserGroup),
    trusteeGroup :: UserGroup
}

data UploadResource = UploadResource {
    uploadIndexPage :: Resource
}

instance HackageFeature UploadFeature where
    getFeature upload = HackageModule
      { featureName = "upload"
      , resources   = map ($uploadResource upload) [uploadIndexPage]
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initUploadFeature :: CoreFeature -> IO UploadFeature
initUploadFeature core = do
    return $ UploadFeature
      { uploadResource = UploadResource
          { uploadIndexPage = (extendResource . corePackagesPage $ coreResource core) { resourcePost = [("", uploadPackage (packageIndexChange core))] }
          }
      , maintainersGroup = getMaintainersGroup
      , trusteeGroup = UserGroup {
            groupDesc = trusteeDescription,
            queryUserList = query $ GetHackageTrustees,
            addUserList = update . AddHackageTrustee,
            removeUserList = update . RemoveHackageTrustee
        }
      }

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

requirePackageAuth :: (MonadIO m, Package pkg) => pkg -> ServerPartT m Users.UserId
requirePackageAuth pkg = do
    userDb <- query $ GetUserDb
    groupSum <- getPackageGroup pkg
    Auth.requireHackageAuth userDb (Just groupSum) Nothing

getPackageGroup :: (MonadIO m, Package pkg) => pkg -> m Group.UserList
getPackageGroup pkg = do
    pkgm    <- query $ GetPackageMaintainers (packageName pkg)
    trustee <- query $ GetHackageTrustees
    return $ Group.unions [trustee, pkgm]


-- TODO: deal with multiple formats. Instead of returning a ServerPart Response,
-- the uploadPackage function could return a, say, ServerPart UploadResult.
--
-- Also, examine race conditions here. Are any of them major inconveniences or security risks?
uploadPackage :: HookList (IO ()) -> Config -> DynamicPath -> ServerPart Response
uploadPackage hook config _ =
  methodSP POST $
    withDataFn (lookInput "package") $ \input ->
          let {-withEntry = do str <- look "entry"
                               case simpleParse str of
                                 Nothing -> fail "no parse"
                                 Just x  -> return (x::UploadLog.Entry)-}
              fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in msum [ upload fileName fileContent ]
  where
    upload name content = do
        -- initial check to ensure logged in.
        users <- query GetUserDb
        state <- query GetPackagesState
        uid <- Auth.requireHackageAuth users Nothing Nothing
        let processPackage content' = do
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

    packageExists   state pkg = not . null $ PackageIndex.lookupPackageName (packageList state) (packageName pkg)
    packageIdExists state pkg = maybe False (const True) $ PackageIndex.lookupPackageId (packageList state) (packageId pkg)

