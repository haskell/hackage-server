module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    MirrorResource(..),
    initMirrorFeature
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Resource
import Distribution.Server.Types

import Distribution.Server.Users.State
import Distribution.Server.Packages.Types
--import Distribution.Server.Auth.Types
import Distribution.Server.Users.Backup
import Distribution.Server.Users.Types
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Backup.Export

import Distribution.Simple.Utils (fromUTF8)
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg, showPWarning)

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time.Format (readsTime)
import System.Locale (defaultTimeLocale)

import Happstack.Server
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (MonadIO(..))
import Distribution.Package
import Distribution.Text (simpleParse)

data MirrorFeature = MirrorFeature {
    mirrorResource :: MirrorResource,
    mirrorGroup :: UserGroup
}
data MirrorResource = MirrorResource {
    mirrorPackageTarball :: Resource,
    mirrorCabalFile :: Resource,
    mirrorGroupResource :: GroupResource
}

instance HackageFeature MirrorFeature where
    getFeature mirror = HackageModule
      { featureName = "mirror"
      , resources   = map ($mirrorResource mirror) [mirrorPackageTarball, mirrorCabalFile]
      , dumpBackup    = Just $ \_ -> do
            clients <- query GetMirrorClients
            return [csvToBackup ["clients.csv"] $ groupToCSV clients]
      , restoreBackup = Just $ \_-> groupBackup ["clients.csv"] ReplaceMirrorClients
      }

-------------------------------------------------------------------------
initMirrorFeature :: Config -> CoreFeature -> UserFeature -> IO MirrorFeature
initMirrorFeature config core users = do
    let coreR  = coreResource core
        store  = serverStore config
        mirrorers = UserGroup {
            groupDesc = nullDescription { groupTitle = "Mirror clients" },
            queryUserList = query GetMirrorClients,
            addUserList = update . AddMirrorClient,
            removeUserList = update . RemoveMirrorClient,
            groupExists = return True,
            canRemoveGroup = [adminGroup users],
            canAddGroup = [adminGroup users]
        }
    (mirrorers', mirrorR) <- groupResourceAt (groupIndex users) "/packages/mirrorers" mirrorers
    return MirrorFeature
      { mirrorResource = MirrorResource
          { mirrorPackageTarball = (extendResource $ corePackageTarball coreR) { resourcePut = [("", packagePut core store)] }
          , mirrorCabalFile = (extendResource $ coreCabalFile coreR) { resourcePut = [("", cabalPut core)] }
          , mirrorGroupResource = mirrorR
          }
      , mirrorGroup = mirrorers'
      }
  where
    -- result: error from unpacking, bad request error, or warning lines
    packagePut _ store _ = do
        requireMirrorAuth
        withUploadInfo "package" $ \input uploadData -> do
            let fileName = (fromMaybe "noname" $ inputFilename input)
                fileContent = undefined -- inputValue input -- HS6 - this needs to be updated to use the new file upload support in HS6
            -- augment unpackPackage to ensure that dpath matches it...
            res <- liftIO $ BlobStorage.addWith store fileContent (return . Upload.unpackPackage fileName)
            case res of
                Left err -> badRequest . toResponse $ err
                Right (((pkg, pkgStr), warnings), blobId) -> do
                    -- doMergePackage runs the package hooks
                    -- if the upload feature is enabled, it adds
                    -- the user to the package's maintainer group
                    -- the mirror client should probably do this itself,
                    -- if it's able (if it's a trustee).
                    liftIO $ doMergePackage core $ PkgInfo {
                        pkgInfoId     = packageId pkg,
                        pkgDesc       = pkg,
                        pkgData       = pkgStr,
                        pkgTarball    = [(blobId, uploadData)],
                        pkgUploadData = uploadData,
                        pkgDataOld    = []
                    }
                    return . toResponse $ unlines warnings

    -- return: error from parsing, bad request error, or warning lines
    cabalPut _ _ = do
        requireMirrorAuth
        withUploadInfo "cabal" $ \input uploadData -> do
            let fileName = (fromMaybe "noname" $ inputFilename input)
                fileContent = undefined -- inputValue input -- HS6 - this needs to be updated to use the new file upload support in HS6
            case parsePackageDescription (fromUTF8 . BS.unpack $ fileContent) of
                ParseFailed err -> badRequest . toResponse $ show (locatedErrorMsg err)
                ParseOk warnings pkg -> do
                    liftIO $ doMergePackage core $ PkgInfo {
                        pkgInfoId     = packageId pkg,
                        pkgDesc       = pkg,
                        pkgData       = fileContent,
                        pkgTarball    = [],
                        pkgUploadData = uploadData,
                        pkgDataOld    = []
                    }
                    return . toResponse $ unlines $ map (showPWarning fileName) warnings

    requireMirrorAuth = do
        ulist <- query GetMirrorClients
        userdb <- query GetUserDb
        Auth.requireHackageAuth userdb (Just ulist) Nothing

    withUploadInfo :: String -> (Input -> UploadInfo -> ServerPart Response) -> ServerPart Response
    withUploadInfo fileField func = do
        mres <- getDataFn ((,,) <$> (lookInput fileField) <*> (look "date") <*> (look "user"))
        case mres of
          (Left errs) -> badRequest $ toResponse $ unlines ("Invalid input." : errs)
          Right (input, mdate, muser) -> do
           case (readsTime defaultTimeLocale "%c" mdate, simpleParse muser :: Maybe UserName) of
            ([(udate, "")], Just uname) -> do
                -- This is a lot for a simple PUT to be doing. Ideally, with a more
                -- advanced mirror client, it would find the user id itself,
                -- create an historical account if one didn't exist, add it to
                -- the necessary user groups (if a package trustee), then PUT
                -- the package or cabal file.
                -- This would require the server to expose:
                -- 1. id data (/users/ids and /users/id/:id)
                -- 2. historical account registration
                -- 
                -- Presently, it creates a deleted user if necessary, and passes
                -- it to func
                uid <- update $ RequireUserName uname
                func input (udate, uid)
            _ -> badRequest $ toResponse ()

