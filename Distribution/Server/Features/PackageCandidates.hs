{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageCandidates (
    PackageCandidatesFeature(..),
    PackageCandidatesResource(..),
    initPackageCandidatesFeature,

    CandidateRender(..),
    CandPkgInfo(..),
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Backup

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users
import Distribution.Server.Features.TarIndexCache

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render
import Distribution.Server.Packages.ChangeLog
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Framework.BackupRestore (restoreBackupUnimplemented)

import Distribution.Server.Util.ServeTarball (serveTarEntry, serveTarball)
import qualified Data.TarIndex as TarIndex

import Distribution.Text
import Distribution.Package

import Data.Version
import Text.XHtml.Strict (unordList, h3, (<<), toHtml)
import Data.Function (fix)
import Data.List (find)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Error (ErrorT(..))

data PackageCandidatesFeature = PackageCandidatesFeature {
    candidatesFeatureInterface :: HackageFeature,
    candidatesCoreResource     :: CoreResource,
    candidatesResource         :: PackageCandidatesResource,

    -- queries
    queryGetCandidateIndex :: MonadIO m => m (PackageIndex CandPkgInfo),

    postCandidate         :: ServerPartE Response,
    postPackageCandidate  :: DynamicPath -> ServerPartE Response,
    putPackageCandidate   :: DynamicPath -> ServerPartE Response,
    doDeleteCandidate     :: DynamicPath -> ServerPartE Response,
    uploadCandidate       :: (PackageId -> Bool) -> ServerPartE CandPkgInfo,
    publishCandidate      :: DynamicPath -> Bool -> ServerPartE UploadResult,
    checkPublish          :: PackageIndex PkgInfo -> CandPkgInfo -> Maybe ErrorResponse,
    candidateRender       :: CandPkgInfo -> IO CandidateRender,

    lookupCandidateName :: PackageName -> ServerPartE [CandPkgInfo],
    lookupCandidateId   :: PackageId -> ServerPartE CandPkgInfo
}

instance IsHackageFeature PackageCandidatesFeature where
    getFeatureInterface = candidatesFeatureInterface

    -- There can also be build reports as well as documentation for proposed
    -- versions.
    -- These features check for existence of a package in the *main* index,
    -- but it should be possible to hijack their indices to support candidates,
    -- perhaps by them having a Filter for whether a package-version exists
    -- (since they don't need any other info than the PackageId).
    -- Unfortunately, some problems exist when both a candidate and actual version
    -- of the same package exist simultaneously, so may want to hook into
    -- UploadFeature's canUploadPackage to ensure this won't happen, and to
    -- force deletion on publication.

{-
  Mapping:
  candidatesPage      -> corePackagesPage
  candidatePage       -> corePackagePage
  candidateCabal      -> coreCabalFile
  candidateTarball    -> corePackageTarball

  candidatesUri       -> indexPackageUri
  candidateUri        -> corePackageUri
  candidateTarballUri -> coreTarballUri
  candidateCabalUri   -> coreCabalUri
-}

data PackageCandidatesResource = PackageCandidatesResource {
    packageCandidatesPage :: Resource,
    publishPage           :: Resource,
    packageCandidatesUri  :: String -> PackageName -> String,
    publishUri            :: String -> PackageId -> String,

    -- TODO: Why don't the following entries have a corresponding entry
    -- in CoreResource?
    candidateContents     :: Resource,
    candidateChangeLog    :: Resource,
    candidateChangeLogUri :: PackageId -> String
}

-- candidates can be published at any time; there can be multiple candidates per package
-- they can be deleted, but it's not required

data CandidateRender = CandidateRender {
    candPackageRender :: PackageRender,
    renderWarnings :: [String],
    hasIndexedPackage :: Bool
}


-- URI generation (string-based), using maps; user groups
initPackageCandidatesFeature :: ServerEnv
                             -> UserFeature
                             -> CoreFeature
                             -> UploadFeature
                             -> TarIndexCacheFeature
                             -> IO PackageCandidatesFeature
initPackageCandidatesFeature env@ServerEnv{serverStateDir} user core upload tarIndexCache = do
    candidatesState <- candidatesStateComponent serverStateDir
    return $ candidatesFeature env user core upload tarIndexCache candidatesState

candidatesStateComponent :: FilePath -> IO (StateComponent CandidatePackages)
candidatesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "CandidatePackages") initialCandidatePackages
  return StateComponent {
      stateDesc    = "Candidate packages"
    , getState     = query st GetCandidatePackages
    , putState     = update st . ReplaceCandidatePackages
    , acidState    = st
    , resetState   = candidatesStateComponent
      -- TODO: backup
    , backupState  = backupCandidates
    , restoreState = restoreBackupUnimplemented
  }

candidatesFeature :: ServerEnv
                  -> UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> TarIndexCacheFeature
                  -> StateComponent CandidatePackages
                  -> PackageCandidatesFeature
candidatesFeature ServerEnv{serverBlobStore = store}
                  UserFeature{..}
                  CoreFeature{ coreResource=core@CoreResource{packageInPath, packageTarballInPath}
                             , queryGetPackageIndex
                             , doAddPackage
                             }
                  UploadFeature{..}
                  TarIndexCacheFeature{cachedPackageTarIndex}
                  candidatesState
  = PackageCandidatesFeature{..}
  where
    candidatesFeatureInterface = (emptyHackageFeature "candidates") {
        featureDesc = "Support for package candidates"
      , featureResources =
          map ($candidatesCoreResource) [
              corePackagesPage
            , corePackagePage
            , coreCabalFile
            , corePackageTarball
            ] ++
          map ($candidatesResource) [
              publishPage
            , candidateContents
            , candidateChangeLog
            ]
      , featureState = [abstractStateComponent candidatesState]
      }

    queryGetCandidateIndex :: MonadIO m => m (PackageIndex CandPkgInfo)
    queryGetCandidateIndex = return . candidateList =<< queryState candidatesState GetCandidatePackages

    candidatesCoreResource = fix $ \r -> CoreResource {
-- TODO: There is significant overlap between this definition and the one in Core
        corePackagesPage = resourceAt "/packages/candidates/.:format"
      , corePackagePage = (resourceAt "/package/:package/candidate.:format") {
            resourceDesc = [(GET, "Show basic package candidate page")]
          , resourceGet  = [("html", basicCandidatePage r)]
          }
      , coreCabalFile = (resourceAt "/package/:package/candidate/:cabal.cabal") {
            resourceDesc = [(GET, "Candidate .cabal file")]
          , resourceGet  = [("cabal", serveCandidateCabal)]
          }
      , corePackageTarball = (resourceAt "/package/:package/candidate/:tarball.tar.gz") {
            resourceDesc = [(GET, "Candidate tarball")]
          , resourceGet  = [("tarball", serveCandidateTarball)]
          }
      , indexPackageUri = \format ->
          renderResource (corePackagesPage r) [format]
      , corePackageIdUri = \format pkgid ->
          renderResource (corePackagePage r) [display pkgid, format]
      , corePackageNameUri = \format pkgname ->
          renderResource (corePackagePage r) [display pkgname, format]
      , coreTarballUri = \pkgid ->
          renderResource (corePackageTarball r) [display pkgid, display pkgid]
      , coreCabalUri = \pkgid ->
          renderResource (coreCabalFile r) [display pkgid, display (packageName pkgid)]
      , packageInPath
      , packageTarballInPath
      , guardValidPackageId   = void . lookupCandidateId
      , guardValidPackageName = void . lookupCandidateName
      }

    candidatesResource = fix $ \r -> PackageCandidatesResource {
        packageCandidatesPage = resourceAt "/package/:package/candidates/.:format"
      , publishPage = resourceAt "/package/:package/candidate/publish.:format"
      , candidateContents = (resourceAt "/package/:package/candidate/src/..") {
            resourceGet = [("", serveContents)]
          }
      , candidateChangeLog = (resourceAt "/package/:package/candidate/changelog") {
            resourceGet = [("changelog", serveChangeLog)]
          }
      , packageCandidatesUri = \format pkgname ->
          renderResource (packageCandidatesPage r) [display pkgname, format]
      , publishUri = \format pkgid ->
          renderResource (publishPage r) [display pkgid, format]
      , candidateChangeLogUri = \pkgid ->
          renderResource (candidateChangeLog candidatesResource) [display pkgid, display (packageName pkgid)]
      }

    basicCandidatePage :: CoreResource -> DynamicPath -> ServerPart Response
    basicCandidatePage r dpath = runServerPartE $ do --TODO: use something else for nice html error pages
      pkg <- packageInPath dpath >>= lookupCandidateId
      ok . toResponse . Resource.XHtml . toHtml $
        [ h3 << "Downloads"
        , toHtml (section pkg)
        , h3 << "Warnings"
        , case candWarnings pkg of
             [] -> toHtml "No warnings"
             warnings -> unordList warnings
        ]
      where section cand = basicPackageSection (coreCabalUri r)
                                               (coreTarballUri r)
                                               (candPkgInfo cand)

    postCandidate :: ServerPartE Response
    postCandidate = do
        pkgInfo <- uploadCandidate (const True)
        seeOther (corePackageIdUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- POST to /:package/candidates/
    postPackageCandidate :: DynamicPath -> ServerPartE Response
    postPackageCandidate dpath = do
      name <- packageInPath dpath
      pkgInfo <- uploadCandidate ((==name) . packageName)
      seeOther (corePackageIdUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- PUT to /:package-version/candidate
    -- FIXME: like delete, PUT shouldn't redirect
    putPackageCandidate :: DynamicPath -> ServerPartE Response
    putPackageCandidate dpath = do
      pkgid <- packageInPath dpath
      guard (packageVersion pkgid /= Version [] [])
      pkgInfo <- uploadCandidate (==pkgid)
      seeOther (corePackageIdUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- FIXME: DELETE should not redirect, but rather return ServerPartE ()
    doDeleteCandidate :: DynamicPath -> ServerPartE Response
    doDeleteCandidate dpath = do
      candidate <- packageInPath dpath >>= lookupCandidateId
      guardAuthorisedAsMaintainer (packageName candidate)
      void $ updateState candidatesState $ DeleteCandidate (packageId candidate)
      seeOther (packageCandidatesUri candidatesResource "" $ packageName candidate) $ toResponse ()

    serveCandidateTarball :: DynamicPath -> ServerPart Response
    serveCandidateTarball dpath = runServerPartE $ do
      pkg <- packageTarballInPath dpath >>= lookupCandidateId
      case pkgTarball (candPkgInfo pkg) of
        [] -> mzero --candidate's tarball does not exist
        ((tb, _):_) -> do
            let blobId = pkgTarballGz tb
            file <- liftIO $ BlobStorage.fetch store blobId
            ok $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime $ candPkgInfo pkg)

    --withFormat :: DynamicPath -> (String -> a) -> a
    --TODO: use something else for nice html error pages
    serveCandidateCabal :: DynamicPath -> ServerPart Response
    serveCandidateCabal dpath = runServerPartE $ do
      pkg <- packageInPath dpath >>= lookupCandidateId
      guard (lookup "cabal" dpath == Just (display $ packageName pkg))
      return $ toResponse (Resource.CabalFile (cabalFileByteString $ pkgData $ candPkgInfo pkg))

    uploadCandidate :: (PackageId -> Bool) -> ServerPartE CandPkgInfo
    uploadCandidate isRight = do
        regularIndex <- queryGetPackageIndex
        -- ensure that the user has proper auth if the package exists
        (pkgInfo, uresult) <- extractPackage (\uid info -> combineErrors $ sequence
          [ processCandidate isRight regularIndex uid info
          , runUserFilter uid])
        let candidate = CandPkgInfo {
                candPkgInfo = pkgInfo,
                candWarnings = uploadWarnings uresult,
                candPublic = True -- do withDataFn
            }
        void $ updateState candidatesState $ AddCandidate candidate
        let group = maintainerGroup (packageName pkgInfo)
        exists <- liftIO $ Group.groupExists group
        when (not exists) $ liftIO $ Group.addUserList group (pkgUploadUser pkgInfo)
        return candidate
      where combineErrors = fmap (listToMaybe . catMaybes)

    -- | Helper function for uploadCandidate.
    processCandidate :: (PackageId -> Bool) -> PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)
    processCandidate isRight state uid res = do
        let pkg = packageId (uploadDesc res)
        if not (isRight pkg)
          then uploadFailed "Name of package or package version does not match"
          else do
            pkgGroup <- Group.queryUserList (maintainerGroup (packageName pkg))
            if packageExists state pkg && not (uid `Group.member` pkgGroup)
              then uploadFailed "Not authorized to upload a candidate for this package"
              else return Nothing
      where uploadFailed = return . Just . ErrorResponse 403 "Upload failed" . return . MText

    publishCandidate :: DynamicPath -> Bool -> ServerPartE UploadResult
    publishCandidate dpath doDelete = do
      packages <- queryGetPackageIndex
      candidate <- packageInPath dpath >>= lookupCandidateId
      -- check authorization to upload - must already be a maintainer
      uid <- guardAuthorised [InGroup (maintainerGroup (packageName candidate))]
      -- check if package or later already exists
      case checkPublish packages candidate of
        Just failed -> throwError failed
        Nothing -> do
          -- run filters
          let pkgInfo = candPkgInfo candidate
              uresult = UploadResult (pkgDesc pkgInfo) (cabalFileByteString $ pkgData pkgInfo) (candWarnings candidate)
              uploadFilter = combineErrors $ runFilter'' canUploadPackage uid uresult
          merror <- liftIO $ combineErrors $ sequence [runUserFilter uid, uploadFilter]
          case merror of
            Just failed -> throwError failed
            Nothing -> do
              uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
              let pkgInfo' = PkgInfo {
                      pkgInfoId     = packageId candidate,
                      pkgData       = pkgData pkgInfo,
                      pkgTarball    = case pkgTarball pkgInfo of
                          ((blobId, _):_) -> [(blobId, uploadData)]
                          [] -> [], -- this shouldn't happen, but let's keep this part total anyway
                      pkgUploadData = uploadData,
                      pkgDataOld    = []
                  }
              success <- liftIO $ doAddPackage pkgInfo'
              --FIXME: share code here with upload
              -- currently we do not create the initial maintainer group etc.
              if success
                then do
                  -- delete when requested: "moving" the resource
                  -- should this be required? (see notes in PackageCandidatesResource)
                  when doDelete $ updateState candidatesState $ DeleteCandidate (packageId candidate)
                  return uresult
                else errForbidden "Upload failed" [MText "Package already exists."]
      where combineErrors = fmap (listToMaybe . catMaybes)


    -- | Helper function for publishCandidate that ensures it's safe to insert into the main index.
    checkPublish :: PackageIndex PkgInfo -> CandPkgInfo -> Maybe ErrorResponse
    checkPublish packages candidate = do
        let pkgs = PackageIndex.lookupPackageName packages (packageName candidate)
            candVersion = packageVersion candidate
        case find ((== candVersion) . packageVersion) pkgs of
            Just {} -> Just $ ErrorResponse 403 "Publish failed" [MText "Package name and version already exist in the database"]
            Nothing  -> Nothing

    ------------------------------------------------------------------------------

    candidateRender :: CandPkgInfo -> IO CandidateRender
    candidateRender cand = do
           users  <- queryGetUserDb
           index  <- queryGetPackageIndex
           mChangeLog <- packageChangeLog (candPkgInfo cand)
           let showChangeLogLink = case mChangeLog of Right _ -> True ; _ -> False
           render <- doPackageRender users (candPkgInfo cand) showChangeLogLink
           return $ CandidateRender {
             candPackageRender = render { rendPkgUri = rendPkgUri render ++ "/candidate" },
             renderWarnings = candWarnings cand,
             hasIndexedPackage = not . null $ PackageIndex.lookupPackageName index (packageName cand)
           }

    ------------------------------------------------------------------------------

    -- Find all candidates for a package (there may be none)
    -- It is not an error if a package has no candidates, but it is an error
    -- when the package itself does not exist. We therefore check the Core
    -- package database to check if the package exists.
    lookupCandidateName :: PackageName -> ServerPartE [CandPkgInfo]
    lookupCandidateName pkgname = do
      guardValidPackageName core pkgname
      state <- queryState candidatesState GetCandidatePackages
      return $ PackageIndex.lookupPackageName (candidateList state) pkgname

    -- TODO: Unlike the corresponding function in core, we don't return the
    -- "latest" candidate when Version is empty. Should we?
    -- (If we change that, we should move the 'guard' to 'guardValidPackageId')
    lookupCandidateId :: PackageId -> ServerPartE CandPkgInfo
    lookupCandidateId pkgid = do
      guard (pkgVersion pkgid /= Version [] [])
      state <- queryState candidatesState GetCandidatePackages
      case PackageIndex.lookupPackageId (candidateList state) pkgid of
        Just pkg -> return pkg
        _ -> errNotFound "Candidate not found" [MText $ "No such candidate version for " ++ display (packageName pkgid)]

{-------------------------------------------------------------------------------
  TODO: everything below is an (almost) direct duplicate of corresponding
  functionality in PackageContents. We could factor this out, although there
  isn't any "interesting" code here.
-------------------------------------------------------------------------------}

    --TODO: use something other than runServerPartE for nice html error pages

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPart Response
    serveChangeLog dpath = runServerPartE $ do
      pkg        <- packageInPath dpath >>= lookupCandidateId
      mChangeLog <- liftIO $ packageChangeLog (candPkgInfo pkg)
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (fp, etag, offset, name) ->
          liftIO $ serveTarEntry fp offset name etag

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPart Response
    serveContents dpath = runServerPartE $ do
      pkg      <- packageInPath dpath >>= lookupCandidateId
      mTarball <- liftIO $ packageTarball (candPkgInfo pkg)
      case mTarball of
        Left err ->
          errNotFound "Could not serve package contents" [MText err]
        Right (fp, etag, index) ->
          serveTarball ["index.html"] (display (packageId pkg)) fp index etag

    packageTarball :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarIndex))
    packageTarball PkgInfo{pkgTarball = (pkgTarball, _) : _} = do
      let blobid = pkgTarballNoGz pkgTarball
          fp     = BlobStorage.filepath store blobid
          etag   = blobETag blobid
      index <- cachedPackageTarIndex pkgTarball
      return $ Right (fp, etag, index)
    packageTarball _ =
      return $ Left "No tarball found"

    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarEntryOffset, FilePath))
    packageChangeLog pkgInfo = runErrorT $ do
      (fp, etag, index) <- ErrorT $ packageTarball pkgInfo
      (offset, fname)   <- ErrorT $ return (findChangeLog pkgInfo index)
      return (fp, etag, offset, fname)
