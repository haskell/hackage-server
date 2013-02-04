{-# LANGUAGE DoRec, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    MirrorResource(..),
    initMirrorFeature
  ) where

import Distribution.Server.Framework hiding (formatTime)

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.State hiding (lookupUserName)
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Backup
import Distribution.Server.Users.Types
import Distribution.Server.Users.Users
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.Unpack as Upload
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg, showPWarning)

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, parseTime)
import System.Locale (defaultTimeLocale)
import qualified Codec.Compression.GZip as GZip

import Distribution.Package
import Distribution.Text


data MirrorFeature = MirrorFeature {
    mirrorFeatureInterface :: HackageFeature,
    mirrorResource :: MirrorResource,
    mirrorGroup :: UserGroup
}

instance IsHackageFeature MirrorFeature where
    getFeatureInterface = mirrorFeatureInterface

data MirrorResource = MirrorResource {
    mirrorPackageTarball :: Resource,
    mirrorPackageUploadTime :: Resource,
    mirrorPackageUploader :: Resource,
    mirrorCabalFile :: Resource,
    mirrorGroupResource :: GroupResource
}

-------------------------------------------------------------------------
initMirrorFeature :: ServerEnv -> CoreFeature -> UserFeature -> IO MirrorFeature
initMirrorFeature env@ServerEnv{serverStateDir, serverVerbosity = verbosity}
                  core user@UserFeature{..} = do
    loginfo verbosity "Initialising mirror feature, start"

    -- Canonical state
    mirrorersState <- mirrorersStateComponent serverStateDir

    -- Tie the knot with a do-rec
    rec let (feature, mirrorersGroupDesc)
              = mirrorFeature env core user
                              mirrorersState mirrorersG mirrorR

        (mirrorersG, mirrorR) <- groupResourceAt "/packages/mirrorers" mirrorersGroupDesc

    loginfo verbosity "Initialising mirror feature, end"
    return feature

mirrorersStateComponent :: FilePath -> IO (StateComponent MirrorClients)
mirrorersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "MirrorClients") initialMirrorClients
  return StateComponent {
      stateDesc    = "Mirror clients"
    , acidState    = st
    , getState     = query st GetMirrorClients
    , putState     = update st . ReplaceMirrorClients . mirrorClients
    , backupState  = \(MirrorClients clients) -> [csvToBackup ["clients.csv"] $ groupToCSV clients]
    , restoreState = MirrorClients <$> groupBackup ["clients.csv"]
    , resetState   = mirrorersStateComponent
    }

mirrorFeature :: ServerEnv
              -> CoreFeature
              -> UserFeature
              -> StateComponent MirrorClients
              -> UserGroup
              -> GroupResource
              -> (MirrorFeature, UserGroup)

mirrorFeature ServerEnv{serverBlobStore = store}
              CoreFeature{ coreResource = coreResource@CoreResource{ packageInPath
                                                                   , packageTarballInPath
                                                                   }
                         , doMergePackage
                         , updateReplacePackageUploadTime
                         , updateReplacePackageUploader
                         , lookupPackageId
                         }
              UserFeature{..}
              mirrorersState mirrorGroup mirrorGroupResource
  = (MirrorFeature{..}, mirrorersGroupDesc)
  where
    mirrorFeatureInterface = (emptyHackageFeature "mirror") {
        featureDesc = "Support direct (PUT) tarball uploads and overrides"
      , featureResources =
          map ($mirrorResource) [
              mirrorPackageTarball
            , mirrorPackageUploadTime
            , mirrorPackageUploader
            , mirrorCabalFile
            ] ++
            [ groupResource     mirrorGroupResource
            , groupUserResource mirrorGroupResource
            ]
      , featureState = [abstractStateComponent mirrorersState]
      }

    mirrorResource = MirrorResource {
        mirrorPackageTarball = (extendResource $ corePackageTarball coreResource) {
            resourceDesc = [ (PUT, "Upload or replace a package tarball") ]
          , resourcePut  = [ ("", tarballPut) ]
          }
      , mirrorPackageUploadTime = (extendResourcePath "/upload-time" $ corePackagePage coreResource) {
            resourceDesc = [ (GET, "Get a package upload time")
                           , (PUT, "Replace package upload time")
                           ]
          , resourceGet  = [ ("", uploadTimeGet) ]
          , resourcePut  = [ ("", uploadTimePut) ]
          }
      , mirrorPackageUploader = (extendResourcePath "/uploader" $ corePackagePage coreResource) {
            resourceDesc = [ (GET, "Get a package uploader (username)")
                           , (PUT, "Replace a package uploader")
                           ]
          , resourceGet  = [ ("", uploaderGet) ]
          , resourcePut  = [ ("", uploaderPut) ]
          }
      , mirrorCabalFile = (extendResource $ coreCabalFile coreResource) {
            resourceDesc = [ (PUT, "Replace a package tarball" ) ]
          , resourcePut  = [ ("", cabalPut) ]
          }
      , mirrorGroupResource
      }

    mirrorersGroupDesc = UserGroup {
        groupDesc      = nullDescription { groupTitle = "Mirror clients" },
        queryUserList  = queryState  mirrorersState   GetMirrorClientsList,
        addUserList    = updateState mirrorersState . AddMirrorClient,
        removeUserList = updateState mirrorersState . RemoveMirrorClient,
        groupExists    = return True,
        canRemoveGroup = [adminGroup],
        canAddGroup    = [adminGroup]
    }


    -- result: error from unpacking, bad request error, or warning lines
    --
    -- curl -u admin:admin \
    --      -X PUT \
    --      -H "Content-Type: application/x-gzip" \
    --      --data-binary @$1 \
    --      http://localhost:8080/package/$PACKAGENAME/$PACKAGEID.tar.gz
    tarballPut :: DynamicPath -> ServerPart Response
    tarballPut dpath = runServerPartE $ do
        uid         <- guardAuthorised [InGroup mirrorGroup]
        pkgid       <- packageTarballInPath dpath
        fileContent <- expectCompressedTarball
        time        <- liftIO getCurrentTime
        let uploadData = (time, uid)
        res <- liftIO $ BlobStorage.addWith store fileContent $ \fileContent' ->
                 let filename = display pkgid <.> "tar.gz"
                 in case Upload.unpackPackageRaw filename fileContent' of
                      Left err -> return $ Left err
                      Right x ->
                          do let decompressedContent = GZip.decompress fileContent'
                             blobIdDecompressed <- BlobStorage.add store decompressedContent
                             return $ Right (x, blobIdDecompressed)
        case res of
            Left err -> badRequest (toResponse err)
            Right ((((pkg, pkgStr), warnings), blobIdDecompressed), blobId) -> do
                -- doMergePackage runs the package hooks
                -- if the upload feature is enabled, it adds
                -- the user to the package's maintainer group
                -- the mirror client should probably do this itself,
                -- if it's able (if it's a trustee).
                liftIO $ doMergePackage $ PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgData       = CabalFileText pkgStr,
                    pkgTarball    = [(PkgTarball { pkgTarballGz = blobId,
                                                   pkgTarballNoGz = blobIdDecompressed },
                                      uploadData)],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }
                return . toResponse $ unlines warnings


    uploaderGet dpath = runServerPartE $ do
      pkg    <- packageInPath dpath >>= lookupPackageId
      userdb <- queryGetUserDb
      return $ toResponse $ display (idToName userdb (pkgUploadUser pkg))

    uploaderPut :: DynamicPath -> ServerPart Response
    uploaderPut dpath = runServerPartE $ do
        guardAuthorised_ [InGroup mirrorGroup]
        pkgid <- packageInPath dpath
        nameContent <- expectTextPlain
        let uname = UserName (unpackUTF8 nameContent)
        (uid, _) <- lookupUserName uname
        mb_err <- updateReplacePackageUploader pkgid uid
        maybe (return ()) (\err -> errNotFound err []) mb_err
        return $ toResponse "Updated uploader OK"

    uploadTimeGet :: DynamicPath -> ServerPart Response
    uploadTimeGet dpath = runServerPartE $ do
      pkg <- packageInPath dpath >>= lookupPackageId
      return $ toResponse $ formatTime defaultTimeLocale "%c" (pkgUploadTime pkg)

    -- curl -H 'Content-Type: text/plain' -u admin:admin -X PUT -d "Tue Oct 18 20:54:28 UTC 2010" http://localhost:8080/package/edit-distance-0.2.1/upload-time
    uploadTimePut :: DynamicPath -> ServerPart Response
    uploadTimePut dpath = runServerPartE $ do
        guardAuthorised_ [InGroup mirrorGroup]
        pkgid <- packageInPath dpath
        timeContent <- expectTextPlain
        case parseTime defaultTimeLocale "%c" (unpackUTF8 timeContent) of
          Nothing -> badRequest $ toResponse "Could not parse upload time"
          Just t  -> do
            mb_err <- updateReplacePackageUploadTime pkgid t
            maybe (return ()) (\err -> errNotFound err []) mb_err
            return $ toResponse "Updated upload time OK"

    -- return: error from parsing, bad request error, or warning lines
    cabalPut :: DynamicPath -> ServerPart Response
    cabalPut dpath = runServerPartE $ do
        uid <- guardAuthorised [InGroup mirrorGroup]
        pkgid :: PackageId <- packageInPath dpath
        fileContent <- expectTextPlain
        time <- liftIO getCurrentTime
        let uploadData = (time, uid)
        case parsePackageDescription . unpackUTF8 $ fileContent of
            ParseFailed err -> badRequest (toResponse $ show (locatedErrorMsg err))
            ParseOk warnings pkg -> do
                liftIO $ doMergePackage $ PkgInfo {
                    pkgInfoId     = packageId pkg,
                    pkgData       = CabalFileText fileContent,
                    pkgTarball    = [],
                    pkgUploadData = uploadData,
                    pkgDataOld    = []
                }
                let filename = display pkgid <.> "cabal"
                return . toResponse $ unlines $ map (showPWarning filename) warnings

