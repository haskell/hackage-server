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
import Distribution.PackageDescription (packageDescription, synopsis)
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
          { uploadIndexPage = (resourceAt "/packages/") { resourcePost = [("", uploadPackage (packageIndexChange core))] }
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
  , groupPrologue  = [] --prologue (desciption pkg)?
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
    userDb  <- query $ GetUserDb
    pkgm    <- query $ GetPackageMaintainers (packageName pkg)
    trustee <- query $ GetHackageTrustees
    let groupSum = Group.unions [trustee, pkgm]
    Auth.requireHackageAuth userDb (Just groupSum) Nothing

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
      --TODO: check if the package is in the index already, before we embark
      -- on writing the tarball into the store and validating etc.
      res <- liftIO $ BlobStorage.addWith (serverStore config) content
                        (Upload.unpackPackage name)
      case res of
        Left  err -> badRequest $ toResponse err
        Right (((pkg, pkgStr), warnings), blobId) -> do
          state  <- query GetPackagesState
          let pkgExists = packageExists state pkg
          user <- uploadingUser state (packageId pkg)
          uploadData <- do now <- liftIO getCurrentTime
                           return (now, user)
          success <- update $ Insert PkgInfo {
            pkgInfoId     = packageId pkg,
            pkgDesc       = pkg,
            pkgData       = pkgStr,
            pkgTarball    = [(blobId, uploadData)], --does this merge properly?
            pkgUploadData = uploadData,
            pkgDataOld    = [] -- what about this?
          }
          if success
             then do
               -- Update the package maintainers group.
               unless pkgExists $ update $ AddPackageMaintainer (packageName pkg) user
               liftIO $ runZeroHook hook
               ok $ toResponse $ unlines warnings
             else forbidden $ toResponse "Package already exists."

    -- Auth group for uploading a package.
    -- A new package may be uped by anyone
    -- An existing package may only be uploaded by a maintainer of
    -- that package or a trustee.
    uploadingUser state pkg =
      if packageExists state pkg
        then requirePackageAuth pkg
        else query GetUserDb >>= \users -> Auth.requireHackageAuth users Nothing Nothing

    packageExists state pkg = not . null $ PackageIndex.lookupPackageName (packageList state) (packageName pkg)

