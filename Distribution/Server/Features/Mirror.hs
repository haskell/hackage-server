module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    MirrorResource(..),
    initMirrorFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Hook

import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import Distribution.Server.Packages.Types
--import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Backup.Export
import Distribution.Server.Backup.Import
import Distribution.Server.Users.UserBackup

import Distribution.Simple.Utils (fromUTF8)
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg, showPWarning)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time.Format (readsTime)
import System.Locale (defaultTimeLocale)

import Happstack.Server
import Happstack.State
import Data.Maybe (fromMaybe)
import Control.Monad (liftM3)
import Control.Monad.Trans (MonadIO(..))
import Distribution.Package
import Distribution.Text (simpleParse)


data MirrorFeature = MirrorFeature {
    mirrorResource :: MirrorResource,
    mirrorGroup :: UserGroup
}
data MirrorResource = MirrorResource {
    mirrorPackageTarball :: Resource,
    mirrorCabalFile :: Resource
}

instance HackageFeature MirrorFeature where
    getFeature _ = HackageModule
      { featureName = "mirror"
      , resources   = []
      , dumpBackup    = Just $ \_ -> do
            clients <- query GetMirrorClients
            return [csvToBackup ["clients.csv"] $ groupToCSV clients]
      , restoreBackup = Just $ \_-> groupBackup ["clients.csv"] ReplaceMirrorClients
      }
-------------------------------------------------------------------------


initMirrorFeature :: CoreFeature -> IO MirrorFeature
initMirrorFeature core = do
    let coreR  = coreResource core
        change = packageIndexChange core
    return MirrorFeature
      { mirrorResource = MirrorResource
          { mirrorPackageTarball = (extendResource $ corePackageTarball coreR) { resourcePut = [("", packagePut change)] }
          , mirrorCabalFile = (extendResource $ coreCabalFile coreR) { resourcePut = [("", cabalPut change)] }
          }
      , mirrorGroup = UserGroup {
            groupDesc = nullDescription { groupTitle = "Mirror clients", groupEntityURL = "" },
            queryUserList = query GetMirrorClients,
            addUserList = update . AddMirrorClient,
            removeUserList = update . RemoveMirrorClient
        }
      }
  where
    packagePut hook config _ = do
        requireMirrorAuth
        withUploadInfo "tarball" $ \input uploadData -> do
            let fileName = (fromMaybe "noname" $ inputFilename input)
                fileContent = inputValue input
            -- augment unpackPackage to ensure that dpath matches it...
            res <- liftIO $ BlobStorage.addWith (serverStore config) fileContent (return . Upload.unpackPackage fileName)
            case res of
                Left err -> return . toResponse $ err
                Right (((pkg, pkgStr), warnings), blobId) -> do
                    update $ MergePkg PkgInfo {
                        pkgInfoId     = packageId pkg,
                        pkgDesc       = pkg,
                        pkgData       = pkgStr,
                        pkgTarball    = [(blobId, uploadData)],
                        pkgUploadData = uploadData,
                        pkgDataOld    = []
                    }
                    liftIO $ runZeroHook hook
                    return . toResponse $ unlines warnings

    cabalPut hook _ _ = do
        requireMirrorAuth
        withUploadInfo "cabal" $ \input uploadData -> do
            let fileName = (fromMaybe "noname" $ inputFilename input)
                fileContent = inputValue input
            case parsePackageDescription (fromUTF8 . BS.unpack $ fileContent) of
                ParseFailed err -> return . toResponse $ show (locatedErrorMsg err)
                ParseOk warnings pkg -> do
                    update $ MergePkg PkgInfo {
                        pkgInfoId     = packageId pkg,
                        pkgDesc       = pkg,
                        pkgData       = fileContent,
                        pkgTarball    = [],
                        pkgUploadData = uploadData,
                        pkgDataOld    = []
                    }
                    liftIO $ runZeroHook hook
                    return . toResponse $ unlines $ map (showPWarning fileName) warnings

    requireMirrorAuth = do
        ulist <- query GetMirrorClients
        users <- query GetUserDb
        Auth.requireHackageAuth users (Just ulist) (Just DigestAuth) --force digest here

    withUploadInfo :: String -> (Input -> UploadInfo -> ServerPart Response) -> ServerPart Response
    withUploadInfo fileField func = withDataFn (liftM3 (,,) (lookInput fileField) (look "date") (look "user")) $ \(input, mdate, muser) ->
        case (readsTime defaultTimeLocale "%c" mdate, simpleParse muser :: Maybe UserName) of
            ([(udate, "")], Just _) -> do
                -- TODO: create a deleted user here if necessary, and pass it to the func
                -- right now just use a dummy uid
                func input (udate, UserId 0)
            _ -> badRequest $ toResponse ()

-------------------------------------------------------------------------

