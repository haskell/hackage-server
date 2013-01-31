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

    withCandidatePath :: forall a. DynamicPath -> (CandidatePackages -> CandPkgInfo -> ServerPartE a) -> ServerPartE a,
    withCandidate     :: forall a. PackageId -> (CandidatePackages -> Maybe CandPkgInfo -> [CandPkgInfo] -> ServerPartE a) -> ServerPartE a,
    withCandidates    :: forall a. PackageName -> (CandidatePackages -> [CandPkgInfo] -> ServerPartE a) -> ServerPartE a
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
    , backupState  = \_ -> []
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
                  CoreFeature{ coreResource=CoreResource{packageInPath, packageTarballInPath}
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
--------------------------------------------------------------------------------
-- TODO: Either add support for these or else split CoreResource
        coreIndexPage       = error "coreIndexPage not supported for candidates"
      , coreIndexTarball    = error "coreIndexTarball not supported for candidates"
      , corePackageRedirect = error "corePackageRedirect not supported for candidates"
      , indexTarballUri     = error "indexTarballUri not supported for candidates"
      , corePackageName     = error "corePackageName not supported for candidates"
--------------------------------------------------------------------------------
-- TODO: There is significant overlap between this definition and the one in Core
      , corePackagesPage = resourceAt "/packages/candidates/.:format"
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
      , corePackageUri  = \format pkgid ->
          renderResource (corePackagePage r) [display pkgid, format]
      , coreTarballUri = \pkgid ->
          renderResource (corePackageTarball r) [display pkgid, display pkgid]
      , coreCabalUri = \pkgid ->
          renderResource (coreCabalFile r) [display pkgid, display (packageName pkgid)]
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
      pkgid <- packageInPath dpath
      withCandidate pkgid $ \_ mpkg _ ->
        ok . toResponse . Resource.XHtml $ case mpkg of
          Nothing  -> toHtml $ "A candidate for " ++ display pkgid ++ " doesn't exist"
          Just pkg -> toHtml [ h3 << "Downloads"
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
        seeOther (corePackageUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- POST to /:package/candidates/
    postPackageCandidate :: DynamicPath -> ServerPartE Response
    postPackageCandidate dpath = do
      name <- packageInPath dpath
      pkgInfo <- uploadCandidate ((==name) . packageName)
      seeOther (corePackageUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- PUT to /:package-version/candidate
    -- FIXME: like delete, PUT shouldn't redirect
    putPackageCandidate :: DynamicPath -> ServerPartE Response
    putPackageCandidate dpath = do
      pkgid <- packageInPath dpath
      guard (packageVersion pkgid /= Version [] [])
      pkgInfo <- uploadCandidate (==pkgid)
      seeOther (corePackageUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- FIXME: DELETE should not redirect, but rather return ServerPartE ()
    doDeleteCandidate :: DynamicPath -> ServerPartE Response
    doDeleteCandidate dpath = withCandidatePath dpath $ \_ candidate -> do
        withPackageAuth candidate $ \_ _ -> do
        void $ updateState candidatesState $ DeleteCandidate (packageId candidate)
        seeOther (packageCandidatesUri candidatesResource "" $ packageName candidate) $ toResponse ()

    serveCandidateTarball :: DynamicPath -> ServerPart Response
    serveCandidateTarball dpath = runServerPartE $ do
      pkgid <- packageTarballInPath dpath
      withCandidate pkgid $ \_ mpkg _ -> case mpkg of
        Nothing -> mzero -- candidate  does not exist
        Just pkg -> case pkgTarball (candPkgInfo pkg) of
            [] -> mzero --candidate's tarball does not exist
            ((tb, _):_) -> do
                let blobId = pkgTarballGz tb
                file <- liftIO $ BlobStorage.fetch store blobId
                ok $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime $ candPkgInfo pkg)

    --withFormat :: DynamicPath -> (String -> a) -> a
    serveCandidateCabal :: DynamicPath -> ServerPart Response
    serveCandidateCabal dpath =
        runServerPartE $ --TODO: use something else for nice html error pages
        withCandidatePath dpath $ \_ pkg -> do
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
                candInfoId = packageId pkgInfo,
                candPkgInfo = pkgInfo,
                candWarnings = uploadWarnings uresult,
                candPublic = True -- do withDataFn
            }
        void $ updateState candidatesState $ AddCandidate candidate
        let group = packageMaintainers [("package", display $ packageName pkgInfo)]
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
            pkgGroup <- getPackageGroup (packageName pkg)
            if packageExists state pkg && not (uid `Group.member` pkgGroup)
              then uploadFailed "Not authorized to upload a candidate for this package"
              else return Nothing
      where uploadFailed = return . Just . ErrorResponse 403 "Upload failed" . return . MText

    publishCandidate :: DynamicPath -> Bool -> ServerPartE UploadResult
    publishCandidate dpath doDelete = do
        packages <- queryGetPackageIndex
        withCandidatePath dpath $ \_ candidate -> do
        -- check authorization to upload - must already be a maintainer
        withPackageAuth candidate $ \uid _ -> do
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
    withCandidatePath :: DynamicPath -> (CandidatePackages -> CandPkgInfo -> ServerPartE a) -> ServerPartE a
    withCandidatePath dpath func = do
      pkgid <- packageInPath dpath
      withCandidate pkgid $ \state mpkg _ -> case mpkg of
        Nothing  -> errNotFound "Package not found" [MText $ "Candidate for " ++ display pkgid ++ " does not exist"]
        Just pkg -> func state pkg

    withCandidate :: PackageId -> (CandidatePackages -> Maybe CandPkgInfo -> [CandPkgInfo] -> ServerPartE a) -> ServerPartE a
    withCandidate pkgid func = do
        state <- queryState candidatesState GetCandidatePackages
        let pkgs = PackageIndex.lookupPackageName (candidateList state) (packageName pkgid)
        func state (find ((==pkgid) . packageId) pkgs) pkgs

    withCandidates :: PackageName -> (CandidatePackages -> [CandPkgInfo] -> ServerPartE a) -> ServerPartE a
    withCandidates name func = withCandidate (PackageIdentifier name $ Version [] []) $ \state _ infos -> func state infos

    withCandidatePath' :: DynamicPath -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withCandidatePath' dpath k = withCandidatePath dpath $ \_ pkg -> k (candPkgInfo pkg)

{-------------------------------------------------------------------------------
  TODO: everything below is an (almost) direct duplicate of corresponding
  functionality in PackageContents. We could factor this out, although there
  isn't any "interesting" code here.
-------------------------------------------------------------------------------}

    --TODO: use something other than runServerPartE for nice html error pages

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPart Response
    serveChangeLog dpath = runServerPartE $ withCandidatePath' dpath $ \pkg -> do
      mChangeLog <- liftIO $ packageChangeLog pkg
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (fp, etag, offset, name) ->
          liftIO $ serveTarEntry fp offset name etag

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPart Response
    serveContents dpath = runServerPartE $ withCandidatePath' dpath $ \pkg -> do
      mTarball <- liftIO $ packageTarball pkg
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
