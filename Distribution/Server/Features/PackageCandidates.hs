{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, LambdaCase #-}
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
import Distribution.Server.Packages.Readme
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Features.Security.Migration

import Distribution.Server.Util.ServeTarball
import Distribution.Server.Util.Markdown (renderMarkdown, supposedToBeMarkdown)
import Distribution.Server.Pages.Template (hackagePage)

import Distribution.Text
import Distribution.Package
import Distribution.Version

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as BS (ByteString, toStrict)
import qualified Text.XHtml.Strict as XHtml
import           Text.XHtml.Strict ((<<), (!))
import Data.Aeson (Value (..), object, toJSON, (.=))

import Data.Function (fix)
import Data.List (find, intersperse)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as Vec


data PackageCandidatesFeature = PackageCandidatesFeature {
    candidatesFeatureInterface :: HackageFeature,
    candidatesCoreResource     :: CoreResource,
    candidatesResource         :: PackageCandidatesResource,

    -- queries
    queryGetCandidateIndex :: forall m. MonadIO m => m (PackageIndex CandPkgInfo),

    postCandidate         :: ServerPartE Response,
    postPackageCandidate  :: DynamicPath -> ServerPartE Response,
    putPackageCandidate   :: DynamicPath -> ServerPartE Response,
    doDeleteCandidate     :: DynamicPath -> ServerPartE Response,
    doDeleteCandidates    :: DynamicPath -> ServerPartE Response,
    uploadCandidate       :: (PackageId -> Bool) -> ServerPartE CandPkgInfo,
    publishCandidate      :: DynamicPath -> Bool -> ServerPartE UploadResult,
    checkPublish          :: forall m. MonadIO m => Users.UserId -> PackageIndex PkgInfo -> CandPkgInfo -> m (Maybe ErrorResponse),
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
    deletePage            :: Resource,
    deleteCandidatesPage  :: Resource,
    packageCandidatesUri  :: String -> PackageName -> String,
    publishUri            :: String -> PackageId -> String,
    deleteUri             :: String -> PackageId -> String,
    deleteCandidatesUri   :: String -> PackageName -> String,

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
                             -> IO (UserFeature
                                 -> CoreFeature
                                 -> UploadFeature
                                 -> TarIndexCacheFeature
                                 -> IO PackageCandidatesFeature)
initPackageCandidatesFeature env@ServerEnv{serverStateDir} = do
    candidatesState <- candidatesStateComponent False serverStateDir

    return $ \user core upload tarIndexCache -> do
      -- one-off migration
      CandidatePackages{candidateMigratedPkgTarball = migratedPkgTarball} <-
        queryState candidatesState GetCandidatePackages
      unless migratedPkgTarball $ do
        migrateCandidatePkgTarball_v1_to_v2 env candidatesState
        updateState candidatesState SetMigratedPkgTarball

      let feature = candidatesFeature env
                                      user core upload tarIndexCache
                                      candidatesState
      return feature

candidatesStateComponent :: Bool -> FilePath -> IO (StateComponent AcidState CandidatePackages)
candidatesStateComponent freshDB stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "CandidatePackages")
                           (initialCandidatePackages freshDB)
  return StateComponent {
      stateDesc    = "Candidate packages"
    , stateHandle  = st
    , getState     = query st GetCandidatePackages
    , putState     = update st . ReplaceCandidatePackages
    , resetState   = candidatesStateComponent True
    , backupState  = \_ -> backupCandidates
    , restoreState = restoreCandidates
  }

candidatesFeature :: ServerEnv
                  -> UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> TarIndexCacheFeature
                  -> StateComponent AcidState CandidatePackages
                  -> PackageCandidatesFeature
candidatesFeature ServerEnv{serverBlobStore = store}
                  UserFeature{..}
                  CoreFeature{ coreResource=core@CoreResource{packageInPath, packageTarballInPath}
                             , queryGetPackageIndex
                             , updateAddPackage
                             }
                  UploadFeature{..}
                  TarIndexCacheFeature{packageTarball, findToplevelFile}
                  candidatesState
  = PackageCandidatesFeature{..}
  where
    candidatesFeatureInterface = (emptyHackageFeature "candidates") {
        featureDesc = "Support for package candidates"
      , featureResources =
          map ($ candidatesCoreResource) [
              corePackagesPage
            , corePackagePage
            , coreCabalFile
            , corePackageTarball
            ] ++
          map ($ candidatesResource) [
              packageCandidatesPage
            , publishPage
            , candidateContents
            , candidateChangeLog
            ]
      , featureState = [abstractAcidStateComponent candidatesState]
      }

    queryGetCandidateIndex :: MonadIO m => m (PackageIndex CandPkgInfo)
    queryGetCandidateIndex = return . candidateList =<< queryState candidatesState GetCandidatePackages

    candidatesCoreResource = fix $ \r -> CoreResource {
-- TODO: There is significant overlap between this definition and the one in Core
        corePackagesPage = (resourceAt "/packages/candidates/.:format") {
            resourceDesc = [(GET, "List all available package candidates")]
          , resourceGet  = [("json", serveCandidatesJson)]
          , resourcePost = [("txt", \_ -> postCandidatePlain)]
          }
      , corePackagePage = resourceAt "/package/:package/candidate.:format"
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
      , lookupPackageName     = fmap (map candPkgInfo) . lookupCandidateName
      , lookupPackageId       = fmap candPkgInfo . lookupCandidateId
      }

    candidatesResource = fix $ \r -> PackageCandidatesResource {
        packageCandidatesPage = (resourceAt "/package/:package/candidates/.:format") {
            resourceDesc = [(GET, "List available candidates for a single package")]
          , resourceGet  = [("json", servePackageCandidatesJson)]
          }
      , publishPage = resourceAt "/package/:package/candidate/publish.:format"
      , deletePage = resourceAt "/package/:package/candidate/delete.:format"
      , deleteCandidatesPage = resourceAt "/package/:package/candidates/delete.:format"
      , candidateContents = (resourceAt "/package/:package/candidate/src/..") {
            resourceGet = [("", serveContents)]
          }
      , candidateChangeLog = (resourceAt "/package/:package/candidate/changelog.:format") {
            resourceGet = [("txt",  serveChangeLogText)
                          ,("html", serveChangeLogHtml)]
          }
      , packageCandidatesUri = \format pkgname ->
          renderResource (packageCandidatesPage r) [display pkgname, format]
      , publishUri = \format pkgid ->
          renderResource (publishPage r) [display pkgid, format]
      , deleteUri = \format pkgid ->
          renderResource (deletePage r) [display pkgid, format]
      , deleteCandidatesUri = \format pkgname ->
          renderResource (deleteCandidatesPage r) [display pkgname, format]
      , candidateChangeLogUri = \pkgid ->
          renderResource (candidateChangeLog candidatesResource) [display pkgid, display (packageName pkgid)]
      }

    -- GET /package/:package/candidates/
    servePackageCandidatesJson :: DynamicPath -> ServerPartE Response
    servePackageCandidatesJson dpath = do
        pkgname <- packageInPath dpath
        pkgs <- lookupCandidateName pkgname

        users  <- queryGetUserDb
        let lupUserName uid = (uid, fmap Users.userName (Users.lookupUserId uid users))

        let pvs = [ object [ T.pack "version"  .= (T.pack . display . packageVersion . candInfoId) p
                           , T.pack "sha256"   .= (blobInfoHashSHA256 . pkgTarballGz . fst) tarball
                           , T.pack "time"     .= (fst . snd) tarball
                           , T.pack "uploader" .= (lupUserName . snd . snd) tarball
                           ]
                  | p <- pkgs
                  , let tarball = Vec.last . pkgTarballRevisions . candPkgInfo $ p
                  ]

        return . toResponse . toJSON $ pvs

    -- GET /packages/candidates/
    serveCandidatesJson :: DynamicPath -> ServerPartE Response
    serveCandidatesJson _ = do
        cands <- queryGetCandidateIndex

        let pkgss :: [[CandPkgInfo]]
            pkgss = PackageIndex.allPackagesByName cands

        return . toResponse $ toJSON (map cpiToJSON pkgss)
      where
        cpiToJSON :: [CandPkgInfo] -> Value
        cpiToJSON [] = Null -- should never happen
        cpiToJSON pkgs = object [ T.pack "name" .= pn, T.pack "candidates" .= pvs ]
          where
            pn = T.pack . display . pkgName . candInfoId . head $ pkgs
            pvs = [ object [ T.pack "version" .= (T.pack . display . packageVersion . candInfoId) p
                           , T.pack "sha256"  .= (blobInfoHashSHA256 . pkgTarballGz . fst) tarball
                           ]
                  | p <- pkgs
                  , let tarball = Vec.last . pkgTarballRevisions . candPkgInfo $ p
                  ]

    postCandidate :: ServerPartE Response
    postCandidate = do
        pkgInfo <- uploadCandidate (const True)
        seeOther (corePackageIdUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    postCandidatePlain :: ServerPartE Response
    postCandidatePlain = do
        pkgInfo <- uploadCandidate (const True)
        ok $ toResponse $ unlines $ candWarnings pkgInfo

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
      guard (packageVersion pkgid /= nullVersion)
      pkgInfo <- uploadCandidate (==pkgid)
      seeOther (corePackageIdUri candidatesCoreResource "" $ packageId pkgInfo) (toResponse ())

    -- FIXME: DELETE should not redirect, but rather return ServerPartE ()
    doDeleteCandidate :: DynamicPath -> ServerPartE Response
    doDeleteCandidate dpath = do
      candidate <- packageInPath dpath >>= lookupCandidateId
      guardAuthorisedAsMaintainerOrTrustee (packageName candidate)
      void $ updateState candidatesState $ DeleteCandidate (packageId candidate)
      seeOther (packageCandidatesUri candidatesResource "" $ packageName candidate) $ toResponse ()

    doDeleteCandidates :: DynamicPath -> ServerPartE Response
    doDeleteCandidates dpath = do
      pkgname <- packageInPath dpath
      guardAuthorisedAsMaintainerOrTrustee pkgname
      void $ updateState candidatesState $ DeleteCandidates pkgname
      seeOther (packageCandidatesUri candidatesResource "" $ pkgname) $ toResponse ()

    serveCandidateTarball :: DynamicPath -> ServerPartE Response
    serveCandidateTarball dpath = do
      pkgid <- packageTarballInPath dpath
      guard (pkgVersion pkgid /= nullVersion)
      pkg   <- lookupCandidateId pkgid
      case pkgLatestTarball (candPkgInfo pkg) of
        Nothing -> errNotFound "Tarball not found"
                     [MText "No tarball exists for this package version."]
        Just (tarball, (uploadtime, _uid), _revNo) -> do
          let blobId = blobInfoId $ pkgTarballGz tarball
          cacheControl [Public, NoTransform, maxAgeMinutes 10]
                       (BlobStorage.blobETag blobId)
          file <- liftIO $ BlobStorage.fetch store blobId
          return $ toResponse $ Resource.PackageTarball file blobId uploadtime

    --withFormat :: DynamicPath -> (String -> a) -> a
    --TODO: use something else for nice html error pages
    serveCandidateCabal :: DynamicPath -> ServerPartE Response
    serveCandidateCabal dpath = do
      pkg <- packageInPath dpath >>= lookupCandidateId
      guard (lookup "cabal" dpath == Just (display $ packageName pkg))
      let (fileRev, (utime, _uid)) = pkgLatestRevision (candPkgInfo pkg)
          cabalfile = Resource.CabalFile (cabalFileByteString fileRev) utime
      return $ toResponse cabalfile

    uploadCandidate :: (PackageId -> Bool) -> ServerPartE CandPkgInfo
    uploadCandidate isRight = do
        guardAuthorised_ [InGroup uploadersGroup]
        regularIndex <- queryGetPackageIndex
        -- ensure that the user has proper auth if the package exists
        (uid, uresult, tarball) <- extractPackage $ \uid info ->
                                     processCandidate isRight regularIndex uid info
        now <- liftIO getCurrentTime
        let (UploadResult pkg pkgStr _) = uresult
            pkgid      = packageId pkg
            cabalfile  = CabalFileText pkgStr
            uploadinfo = (now, uid)
            candidate = CandPkgInfo {
                candPkgInfo = PkgInfo {
                    pkgInfoId     = pkgid,
                    pkgMetadataRevisions = Vec.singleton (cabalfile, uploadinfo),
                    pkgTarballRevisions  = Vec.singleton (tarball, uploadinfo)
                  },
                candWarnings = uploadWarnings uresult,
                candPublic = True -- do withDataFn
            }
        void $ updateState candidatesState $ AddCandidate candidate
        let group = maintainersGroup (packageName pkgid)
        liftIO $ Group.addUserToGroup group uid
        return candidate

    -- | Helper function for uploadCandidate.
    processCandidate :: (PackageId -> Bool) -> PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe ErrorResponse)
    processCandidate isRight state uid res = do
        let pkg = packageId (uploadDesc res)
        if not (isRight pkg)
          then uploadFailed [MText "Name of package or package version does not match"]
          else do
            pkgGroup <- Group.queryUserGroup (maintainersGroup (packageName pkg))
            if (not (Group.null pkgGroup) || packageExists state pkg)
                && not (uid `Group.member` pkgGroup)
              then uploadFailed (notMaintainer pkg)
              else return Nothing
      where
        -- TODO: try to share more code with "Upload" module
        uploadFailed = return . Just . ErrorResponse 403 [] "Upload failed"

        notMaintainer pkg = [ MText $
                        "You are not authorised to upload candidates of this package. The "
                     ++ "package '" ++ display (packageName pkg) ++ "' exists already and you "
                     ++ "are not a member of the maintainer group for this package.\n\n"
                     ++ "If you believe you should be a member of the "
                     , MLink "maintainer group for this package"
                            ("/package/" ++ display (packageName pkg) ++ "/maintainers")
                     , MText $  ", then ask an existing maintainer to add you to the group. If "
                     ++ "this is a package name clash, please pick another name or talk to the "
                     ++ "maintainers of the existing package."
                     ]

    publishCandidate :: DynamicPath -> Bool -> ServerPartE UploadResult
    publishCandidate dpath doDelete = do
      packages <- queryGetPackageIndex
      candidate <- packageInPath dpath >>= lookupCandidateId
      -- check authorization to upload - must already be a maintainer
      uid <- guardAuthorisedAsMaintainer (packageName candidate)
      -- check if package or later already exists
      checkPublish uid packages candidate >>= \case
        Just failed -> throwError failed
        Nothing -> do
          -- run filters
          let pkgInfo = candPkgInfo candidate
              uresult = UploadResult (pkgDesc pkgInfo)
                                     (cabalFileByteString (pkgLatestCabalFileText pkgInfo))
                                     (candWarnings candidate)
          time <- liftIO getCurrentTime
          let uploadInfo = (time, uid)
              getTarball (tarball, _uploadInfo, _revNo) = tarball
          success <- updateAddPackage (packageId candidate)
                                      (pkgLatestCabalFileText pkgInfo)
                                      uploadInfo
                                      (fmap getTarball $ pkgLatestTarball pkgInfo)
          --FIXME: share code here with upload
          -- currently we do not create the initial maintainer group etc.
          if success
            then do
              -- delete when requested: "moving" the resource
              -- should this be required? (see notes in PackageCandidatesResource)
              when doDelete $ updateState candidatesState $ DeleteCandidate (packageId candidate)
              return uresult
            else errForbidden "Upload failed" [MText "Package already exists."]


    -- | Helper function for publishCandidate that ensures it's safe to insert into the main index.
    --
    -- TODO: share code w/ 'Distribution.Server.Features.Upload.processUpload'
    checkPublish :: forall m. MonadIO m => Users.UserId -> PackageIndex PkgInfo -> CandPkgInfo -> m (Maybe ErrorResponse)
    checkPublish uid packages candidate
      | Just _ <- find ((== candVersion) . packageVersion) pkgs
        = return $ Just $ ErrorResponse 403 [] "Publish failed" [MText "Package name and version already exist in the database"]

      | packageExists packages candidate = return Nothing

        -- check for case-clashes with already published packages
      | otherwise = case PackageIndex.searchByName packages (unPackageName candName) of
          PackageIndex.Unambiguous (mp:_) -> do
            group <- liftIO $ (Group.queryUserGroup . maintainersGroup . packageName) mp
            if not $ uid `Group.member` group
              then return $ Just $ ErrorResponse 403 [] "Publish failed" (caseClash [mp])
              else return Nothing

          PackageIndex.Unambiguous [] -> return Nothing -- can this ever occur?

          PackageIndex.Ambiguous mps -> do
            let matchingPackages = concat . map (take 1) $ mps
            groups <- mapM (liftIO . Group.queryUserGroup . maintainersGroup . packageName) matchingPackages
            if not . any (uid `Group.member`) $ groups
              then return $ Just $ ErrorResponse 403 [] "Publish failed" (caseClash matchingPackages)
              else return Nothing

          -- no case-neighbors
          PackageIndex.None -> return Nothing
      where
        pkgs = PackageIndex.lookupPackageName packages candName
        candVersion = packageVersion candidate
        candName    = packageName candidate

        caseClash pkgs' = [MText $
                         "Package(s) with the same name as this package, modulo case, already exist: "
                         ]
                      ++ intersperse (MText ", ") [ MLink pn ("/package/" ++ pn)
                                                  | pn <- map (display . packageName) pkgs' ]
                      ++ [MText $
                         ".\n\nYou may only upload new packages which case-clash with existing packages "
                      ++ "if you are a maintainer of one of the existing packages. Please pick another name."]

    ------------------------------------------------------------------------------

    candidateRender :: CandPkgInfo -> IO CandidateRender
    candidateRender cand = do
        users  <- queryGetUserDb
        index  <- queryGetPackageIndex
        let pkg = candPkgInfo cand
        changeLog <- findToplevelFile pkg isChangeLogFile
                 >>= either (\_ -> return Nothing) (return . Just)
        readme    <- findToplevelFile pkg isReadmeFile
                 >>= either (\_ -> return Nothing) (return . Just)
        let render = doPackageRender users pkg
        return $ CandidateRender {
          candPackageRender = render { rendPkgUri    = rendPkgUri render ++ "/candidate"
                                     , rendChangeLog = changeLog
                                     , rendReadme    = readme},
          renderWarnings    = candWarnings cand,
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
      guard (pkgVersion pkgid /= nullVersion)
      state <- queryState candidatesState GetCandidatePackages
      case PackageIndex.lookupPackageId (candidateList state) pkgid of
        Just pkg -> return pkg
        _ -> errNotFound "Candidate not found" [MText $ "No such candidate version for " ++ display (packageName pkgid)]

{-------------------------------------------------------------------------------
  TODO: everything below is an (almost) direct duplicate of corresponding
  functionality in PackageContents. We could factor this out, although there
  isn't any "interesting" code here, except differences in http cache control.
-------------------------------------------------------------------------------}

    -- result: changelog or not-found error
    serveChangeLogText :: DynamicPath -> ServerPartE Response
    serveChangeLogText dpath = do
      pkg        <- packageInPath dpath >>= lookupCandidateId
      mChangeLog <- liftIO $ findToplevelFile (candPkgInfo pkg) isChangeLogFile
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (tarfile, etag, offset, filename) -> do
          cacheControl [Public, maxAgeMinutes 5] etag
          liftIO $ serveTarEntry tarfile offset filename
          -- TODO: We've already loaded the contents; refactor

    serveChangeLogHtml :: DynamicPath -> ServerPartE Response
    serveChangeLogHtml dpath = do
      pkg     <- packageInPath dpath >>= lookupCandidateId
      mReadme <- liftIO $ findToplevelFile (candPkgInfo pkg) isChangeLogFile
      case mReadme of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (tarfile, etag, offset, filename) -> do
          contents <- either (\err -> errInternalError [MText err])
                             (return . snd)
                  =<< liftIO (loadTarEntry tarfile offset)
          cacheControl [Public, maxAgeDays 30] etag
          return $ toResponse $ Resource.XHtml $
            let title = "Changelog for " ++ display (packageId pkg) in
            hackagePage title
              [ XHtml.h2 << title
              , XHtml.thediv ! [XHtml.theclass "embedded-author-content"]
                            << if supposedToBeMarkdown filename
                                 then renderMarkdown filename contents
                                 else XHtml.thediv ! [XHtml.theclass "preformatted"]
                                                  << unpackUtf8 contents
              ]


    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPartE Response
    serveContents dpath = do
      pkg      <- packageInPath dpath >>= lookupCandidateId
      mTarball <- liftIO $ packageTarball (candPkgInfo pkg)
      case mTarball of
        Left err ->
          errNotFound "Could not serve package contents" [MText err]
        Right (fp, etag, index) ->
          serveTarball (display (packageId pkg) ++ " candidate source tarball")
                       ["index.html"] (display (packageId pkg)) fp index
                       [Public, maxAgeMinutes 5] etag

unpackUtf8 :: BS.ByteString -> String
unpackUtf8 = T.unpack
           . T.decodeUtf8With T.lenientDecode
           . BS.toStrict
