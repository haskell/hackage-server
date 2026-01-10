{-# LANGUAGE RankNTypes, FlexibleContexts,
             NamedFieldPuns, RecordWildCards, PatternGuards #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Distribution.Server.Features.Documentation (
    DocumentationFeature(..),
    DocumentationResource(..),
    initDocumentationFeature
  ) where

import Distribution.Server.Features.Security.SHA256       (sha256)
import Distribution.Server.Framework

import Distribution.Server.Features.Documentation.State
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users
import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache
import Distribution.Server.Features.BuildReports
import Distribution.Version (Version, nullVersion)

import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Framework.BlobStorage (BlobId)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.ServeTarball as ServerTarball
import qualified Distribution.Server.Util.DocMeta as DocMeta
import qualified Distribution.Server.Util.GZip as Gzip
import Distribution.Server.Features.BuildReports.BuildReport (PkgDetails(..), BuildStatus(..))
import Data.TarIndex (TarIndex)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Distribution.Text
import Distribution.Package
import qualified Distribution.Parsec as P

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.Search as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Function (fix)

import Data.Aeson (toJSON)
import Data.Maybe
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime(..), diffUTCTime, getCurrentTime)
import System.Directory (getModificationTime)
import Control.Applicative
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Packages.Types
-- TODO:
-- 1. Write an HTML view for organizing uploads
-- 2. Have cabal generate a standard doc tarball, and serve that here
data DocumentationFeature = DocumentationFeature {
    documentationFeatureInterface :: HackageFeature,

    queryHasDocumentation   :: forall m. MonadIO m => PackageIdentifier -> m Bool,
    queryDocumentation      :: forall m. MonadIO m => PackageIdentifier -> m (Maybe BlobId),
    queryDocumentationIndex :: forall m. MonadIO m => m (Map.Map PackageId BlobId),

    latestPackageWithDocumentation :: forall m. MonadIO m => PreferredInfo -> [PkgInfo] -> m (Maybe PackageId),

    uploadDocumentation :: DynamicPath -> ServerPartE Response,
    deleteDocumentation :: DynamicPath -> ServerPartE Response,


    documentationResource :: DocumentationResource,

    -- | Notification of documentation changes
    documentationChangeHook :: Hook PackageId ()

}

instance IsHackageFeature DocumentationFeature where
    getFeatureInterface = documentationFeatureInterface

data DocumentationResource = DocumentationResource {
    packageDocsContent :: Resource,
    packageDocsWhole   :: Resource,
    packageDocsStats   :: Resource,

    packageDocsContentUri :: PackageId -> String,
    packageDocsWholeUri   :: String -> PackageId -> String
}

initDocumentationFeature :: String
                         -> ServerEnv
                         -> IO (CoreResource
                             -> IO [PackageIdentifier]
                             -> UploadFeature
                             -> TarIndexCacheFeature
                             -> ReportsFeature
                             -> UserFeature
                             -> VersionsFeature
                             -> IO DocumentationFeature)
initDocumentationFeature name
                         env@ServerEnv{serverStateDir} = do
    -- Canonical state
    documentationState <- documentationStateComponent name serverStateDir

    -- Hooks
    documentationChangeHook <- newHook

    return $ \core getPackages upload tarIndexCache reportsCore user version -> do
      let feature = documentationFeature name env
                                         core getPackages upload tarIndexCache reportsCore user version
                                         documentationState
                                         documentationChangeHook
      return feature

documentationStateComponent :: String -> FilePath -> IO (StateComponent AcidState Documentation)
documentationStateComponent name stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> name) initialDocumentation
  return StateComponent {
      stateDesc    = "Package documentation"
    , stateHandle  = st
    , getState     = query st GetDocumentation
    , putState     = update st . ReplaceDocumentation
    , backupState  = \_ -> dumpBackup
    , restoreState = updateDocumentation (Documentation Map.empty)
    , resetState   = documentationStateComponent name
    }
  where
    dumpBackup doc =
        let exportFunc (pkgid, blob) = BackupBlob [display pkgid, "documentation.tar"] blob
        in map exportFunc . Map.toList $ documentation doc

    updateDocumentation :: Documentation -> RestoreBackup Documentation
    updateDocumentation docs = RestoreBackup {
        restoreEntry = \entry ->
          case entry of
            BackupBlob [str, "documentation.tar"] blobId | Just pkgId <- simpleParse str -> do
              docs' <- importDocumentation pkgId blobId docs
              return (updateDocumentation docs')
            _ ->
              return (updateDocumentation docs)
      , restoreFinalize = return docs
      }

    importDocumentation :: PackageId -> BlobId -> Documentation -> Restore Documentation
    importDocumentation pkgId blobId (Documentation docs) =
      return (Documentation (Map.insert pkgId blobId docs))

documentationFeature :: String
                     -> ServerEnv
                     -> CoreResource
                     -> IO [PackageIdentifier]
                     -> UploadFeature
                     -> TarIndexCacheFeature
                     -> ReportsFeature
                     -> UserFeature
                     -> VersionsFeature
                     -> StateComponent AcidState Documentation
                     -> Hook PackageId ()
                     -> DocumentationFeature
documentationFeature name
                     env@ServerEnv{serverBlobStore = store, serverBaseURI}
                     CoreResource{
                         packageInPath
                       , guardValidPackageId
                       , corePackagePage
                       , corePackagesPage
                       , lookupPackageName
                       }
                     getPackages
                     UploadFeature{..}
                     TarIndexCacheFeature{cachedTarIndex}
                     ReportsFeature{..}
                     UserFeature{ guardAuthorised_ }
                     VersionsFeature{queryGetPreferredInfo}
                     documentationState
                     documentationChangeHook
  = DocumentationFeature{..}
  where
    documentationFeatureInterface = (emptyHackageFeature name) {
        featureDesc = "Maintain and display documentation"
      , featureResources =
          map ($ documentationResource) [
              packageDocsContent
            , packageDocsWhole
            , packageDocsStats
            ]
      , featureState = [abstractAcidStateComponent documentationState]
      }

    queryHasDocumentation :: MonadIO m => PackageIdentifier -> m Bool
    queryHasDocumentation pkgid = queryState documentationState (HasDocumentation pkgid)

    queryDocumentation :: MonadIO m => PackageIdentifier -> m (Maybe BlobId)
    queryDocumentation pkgid = queryState documentationState (LookupDocumentation pkgid)

    queryDocumentationIndex :: MonadIO m => m (Map.Map PackageId BlobId)
    queryDocumentationIndex =
      liftM documentation (queryState documentationState GetDocumentation)

    documentationResource = fix $ \r -> DocumentationResource {
        packageDocsContent = (extendResourcePath "/docs/.." corePackagePage) {
            resourceDesc   = [ (GET, "Browse documentation") ]
          , resourceGet    = [ ("", serveDocumentation) ]
          }
      , packageDocsWhole = (extendResourcePath "/docs.:format" corePackagePage) {
            resourceDesc = [ (GET, "Download documentation")
                           , (PUT, "Upload documentation")
                           , (DELETE, "Delete documentation")
                           ]
          , resourceGet    = [ ("tar", serveDocumentationTar) ]
          , resourcePut    = [ ("tar", uploadDocumentation) ]
          , resourceDelete = [ ("", deleteDocumentation) ]
          }
      , packageDocsStats = (extendResourcePath "/docs.:format" corePackagesPage) {
            resourceDesc   = [ (GET, "Get information about which packages have documentation") ]
          , resourceGet    = [ ("json", serveDocumentationStats) ]
          }
      , packageDocsContentUri = \pkgid ->
          renderResource (packageDocsContent r) [display pkgid]
      , packageDocsWholeUri = \format pkgid ->
          renderResource (packageDocsWhole r) [display pkgid, format]
      }

    serveDocumentationStats :: DynamicPath -> ServerPartE Response
    serveDocumentationStats _dpath = do
        hasDoc        <- optional (look "doc")
        failCnt'      <- optional (look "fail")
        selectedPkgs' <- optional (look "pkgs")
        ghcid         <- optional (look "ghcid")
        pkgs          <- mapParaM queryHasDocumentation =<< liftIO getPackages

        pkgs' <- mapM pkgReportDetails
                  $ filter (matchDoc hasDoc)
                  $ filter (isSelected $ parsePkgs $ fromMaybe "" selectedPkgs') pkgs

        return . toResponse . toJSON
                    $ filter (isGHCok $ parseVersion' ghcid)
                    $ filter (isfailCntOk $ fmap read failCnt') pkgs'
      where
        parseVersion' :: Maybe String -> Maybe Version
        parseVersion' Nothing = Nothing
        parseVersion' (Just k) = P.simpleParsec k

        parsePkgs :: String -> [PackageIdentifier]
        parsePkgs pkgsStr = mapMaybe (P.simpleParsec . C.unpack) (C.split ',' (C.pack pkgsStr))

        isSelectedPackage pkgid pkgid'@(PackageIdentifier _ v)
            | nullVersion == v =
            packageName pkgid == packageName pkgid'
        isSelectedPackage pkgid pkgid' =
            pkgid == pkgid'

        isSelected :: [PackageIdentifier] -> (PackageIdentifier,Bool) -> Bool
        isSelected [] _                   = True
        isSelected selectedPkgs (pkg, _)  = any (isSelectedPackage pkg) selectedPkgs

        matchDoc :: Maybe String -> (PackageIdentifier, Bool) -> Bool
        matchDoc (Just "true")  (_, False)  = False
        matchDoc (Just "false") (_, True)   = False
        matchDoc _ _                        = True

        isfailCntOk:: Maybe Int -> PkgDetails -> Bool
        isfailCntOk Nothing   _   = True
        isfailCntOk (Just i) pkg  = case failCnt pkg of
                                        Nothing -> True
                                        (Just BuildOK) -> False
                                        (Just (BuildFailCnt x)) -> i>x

        isGHCok :: Maybe Version -> PkgDetails -> Bool
        isGHCok Nothing _ = True
        isGHCok (Just ver) pkg = case ghcId pkg of
                              Nothing   -> True
                              Just ver' -> ver' < ver



    serveDocumentationTar :: DynamicPath -> ServerPartE Response
    serveDocumentationTar dpath =
      withDocumentation (packageDocsWhole documentationResource)
                        dpath $ \_ blobid _ -> do
        age <- liftIO . getFileAge $ BlobStorage.filepath store blobid
        let maxAge = documentationCacheTime age
        cacheControl [Public, maxAge]
                     (BlobStorage.blobETag blobid)
        file <- liftIO $ BlobStorage.fetch store blobid
        return $ toResponse $ Resource.DocTarball file blobid


    -- return: not-found error or tarball
    serveDocumentation :: DynamicPath -> ServerPartE Response
    serveDocumentation dpath = do
      withDocumentation (packageDocsContent documentationResource)
                        dpath $ \pkgid blob index -> do
        let tarball = BlobStorage.filepath store blob
            etag    = BlobStorage.blobETag blob
        -- if given a directory, the default page is index.html
        -- the root directory within the tarball is e.g. foo-1.0-docs/
        mtime <- liftIO $ getModificationTime tarball
        age <- liftIO $ getFileAge tarball
        let maxAge = documentationCacheTime age
        tarServe <-
          ServerTarball.serveTarball (display pkgid ++ " documentation")
                                     [{-no index-}] (display pkgid ++ "-docs")
                                     tarball index [Public, maxAge] etag (Just rewriteDocs)
        case tarServe of
          ServerTarball.TarDir response -> pure response
          ServerTarball.TarFile fileContent response -> do
            let
              digest = show $ sha256 fileContent
              -- Because JSON files cannot execute code or affect layout, we don't need to verify anything else
              isDocIndex =
                case dpath of
                  ("..","doc-index.json") : _ -> True
                  _ -> False
              hashesToCheck =
                case dpath of
                  ("..", "quick-jump.min.js") : _ -> Just quickJumpJsKnownGoodSha256
                  ("..", "quick-jump.css")    : _ -> Just quickJumpCssKnownGoodSha256
                  _                               -> Nothing
            if
              | isDocIndex ->
                  pure response
              | Just hashes <- hashesToCheck ->
                  -- Because Quick Jump also runs on the package page,
                  -- and not just on the user content domain,
                  -- we cannot accept arbitrary user-uploaded content.
                  if digest `elem` hashes
                    then pure response
                    else errForbidden "Quick Jump hash is not correct" [MText "Accepted Quick Jump hashes are listed in the hackage-server source code."]
              | mtime < UTCTime (fromGregorian 2025 2 1) 0 ->
                  pure response
              | otherwise ->
                  requireUserContent env response

    rewriteDocs :: BSL.ByteString -> BSL.ByteString
    rewriteDocs dochtml = case BSL.breakFindAfter (BS.pack "<head>") dochtml of
                ((h,t),True) -> h `BSL.append` extraCss `BSL.append` t
                _ -> dochtml
        where extraCss = BSL.pack "<style type=\"text/css\">#synopsis details:not([open]) > ul { visibility: hidden; }</style>"

    -- The cache time for documentation starts at ten minutes and
    -- increases exponentially for four days, when it cuts off at
    -- a maximum of one day.
    documentationCacheTime :: NominalDiffTime -> CacheControl
    documentationCacheTime t
      -- We check if it's been four days instead of just capping the time
      -- with max because there's no point in doing the whole calculation
      -- for old documentation if we're going to throw it away anyway.
      | t > 3600*24*4 = maxAgeDays 1
      | otherwise = maxAgeSeconds $ 60*10 + ceiling (exp (3.28697e-5 * fromInteger (ceiling t) :: Double))

    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

    uploadDocumentation :: DynamicPath -> ServerPartE Response
    uploadDocumentation dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      -- The order of operations:
      -- \* Insert new documentation into blob store
      -- \* Generate the new index
      -- \* Drop the index for the old tar-file
      -- \* Link the new documentation to the package
      fileContents <- expectCompressedTarball
      let filename = display pkgid ++ "-docs" <.> "tar.gz"
          unpacked = Gzip.decompressNamed filename fileContents
      mres <- liftIO $ BlobStorage.addWith store unpacked 
                         (\content -> return (checkDocTarball pkgid content))
      case mres of
        Left  err -> errBadRequest "Invalid documentation tarball" [MText err]
        Right ((), blobid) -> do
          updateState documentationState $ InsertDocumentation pkgid blobid
          runHook_ documentationChangeHook pkgid
          noContent (toResponse ())

   {-
     To upload documentation using curl:

     curl -u admin:admin \
          -X PUT \
          -H "Content-Type: application/x-tar" \
          --data-binary @transformers-0.3.0.0-docs.tar \
          http://localhost:8080/package/transformers-0.3.0.0/docs

     or

     curl -u admin:admin \
          -X PUT \
          -H "Content-Type: application/x-tar" \
          -H "Content-Encoding: gzip" \
          --data-binary @transformers-0.3.0.0-docs.tar.gz \
          http://localhost:8080/package/transformers-0.3.0.0/docs

     The tarfile is expected to have the structure

        transformers-0.3.0.0-docs/index.html
        ..
   -}

    deleteDocumentation :: DynamicPath -> ServerPartE Response
    deleteDocumentation dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      updateState documentationState $ RemoveDocumentation pkgid
      runHook_ documentationChangeHook pkgid
      noContent (toResponse ())

    latestPackageWithDocumentation :: MonadIO m => PreferredInfo -> [PkgInfo] -> m (Maybe PackageId)
    latestPackageWithDocumentation prefInfo ps = helper (reverse ps)
      where
        helper [] = helper2 (reverse ps)
        helper (pkg:pkgs) = do
          hasDoc <- queryHasDocumentation (pkgInfoId pkg)
          let status = getVersionStatus prefInfo (packageVersion pkg)
          if hasDoc && status == NormalVersion
              then pure (Just (packageId pkg))
              else helper pkgs

        helper2 [] = pure Nothing
        helper2 (pkg:pkgs) = do
          hasDoc <- queryHasDocumentation (pkgInfoId pkg)
          if hasDoc
              then pure (Just (packageId pkg))
              else helper2 pkgs

    withDocumentation :: Resource -> DynamicPath
                      -> (PackageId -> BlobId -> TarIndex -> ServerPartE Response)
                      -> ServerPartE Response
    withDocumentation self dpath func = do
      pkgid <- packageInPath dpath

      -- Set up the canonical URL to point to the unversioned path
      let basedpath =
            [ if var == "package"
                then (var, unPackageName $ pkgName pkgid)
                else e
            | e@(var, _) <- dpath ]
          basePkgPath   = renderResource' self basedpath
          canonicalLink = show serverBaseURI ++ basePkgPath
          canonicalHeader = "<" ++ canonicalLink ++ ">; rel=\"canonical\""

      -- Link: <http://canonical.link>; rel="canonical"
      -- See https://support.google.com/webmasters/answer/139066?hl=en#6
      setHeaderM "Link" canonicalHeader

      -- Essentially errNotFound, but overloaded to specify a header.
      -- (Needed since errNotFound throws away result of setHeaderM)
      let errNotFoundH title message = throwError
                  (ErrorResponse 404
                  [("Link", canonicalHeader)]
                  title message)

      case pkgVersion pkgid == nullVersion of
        -- if no version is given we want to redirect to the latest version with docs
        True -> do
            pkgs <- lookupPackageName (pkgName pkgid)
            prefInfo <- queryGetPreferredInfo (pkgName pkgid)
            latestPackageWithDocumentation prefInfo pkgs >>= \case
              Just latestWithDocs -> do
                let dpath' = [ if var == "package"
                                then (var, display latestWithDocs)
                                else e
                            | e@(var, _) <- dpath ]
                    latestPkgPath = (renderResource' self dpath')
                tempRedirect latestPkgPath (toResponse "")
              Nothing -> errNotFoundH "Not Found" [MText "There is no documentation for this package."]
        False -> do
          mdocs <- queryState documentationState $ LookupDocumentation pkgid
          case mdocs of
            Nothing ->
              errNotFoundH "Not Found"
                [ MText "There is no documentation for "
                , MLink (display pkgid) ("/package/" ++ display pkgid)
                , MText ". See "
                , MLink canonicalLink canonicalLink
                , MText " for the latest version."
                ]
            Just blob -> do
              index <- liftIO $ cachedTarIndex blob
              func pkgid blob index


-- Check the tar file is well formed and all files are within foo-1.0-docs/
checkDocTarball :: PackageId -> BSL.ByteString -> Either String ()
checkDocTarball pkgid =
     checkEntries
   . fmapErr (either id show) . chainChecks (Tar.checkEntryTarbomb (display pkgid ++ "-docs"))
   . fmapErr (either id show) . chainChecks Tar.checkEntrySecurity
   . fmapErr (either id show) . chainChecks Tar.checkEntryPortability
   . fmapErr (either id show) . Tar.decodeLongNames
   . fmapErr show             . Tar.read
  where
    fmapErr f    = Tar.foldEntries Tar.Next Tar.Done (Tar.Fail . f)
    chainChecks check = Tar.mapEntries (\entry -> maybe (Right entry) Left (check entry))

    checkEntries = Tar.foldEntries checkEntry (Right ()) Left

    checkEntry entry remainder
      | Tar.entryTarPath entry == docMetaPath = checkDocMeta entry remainder
      | otherwise                             = remainder

    checkDocMeta entry remainder =
      case Tar.entryContent entry of
        Tar.NormalFile content size
          | size <= maxDocMetaFileSize ->
              case DocMeta.parseDocMeta content of
                Just _ -> remainder
                Nothing -> Left "meta.json is invalid"
          | otherwise -> Left "meta.json too large"
        _ -> Left "meta.json not a file"

    maxDocMetaFileSize = 16 * 1024 -- 16KiB
    docMetaPath = DocMeta.packageDocMetaTarPath pkgid



{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

mapParaM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapParaM f = mapM (\x -> (,) x <$> f x)

getFileAge :: FilePath -> IO NominalDiffTime
getFileAge file = diffUTCTime <$> getCurrentTime <*> getModificationTime file

quickJumpJsKnownGoodSha256 :: [String]
quickJumpJsKnownGoodSha256 =
  [
    -- commit: e99aefb50ca63e2dbcc95841efbb53cea90151d8 (Sep 23 2017)
    -- object: c9f2b445b9
    "e1da96b0d7ab3d72cfe3786def923c5af91ba331858852f1f43a1acfc5ee6966"

    -- commit: 8e88615a23a9f1980a55bd1b3ef9dcc938d95237 (Oct 10 2017)
    -- object: cb24f8bdea
  , "a273a3ef19c21032afc5f65d1e09933146f183da906ca9d0b4c285095539e0e7"

    -- commit: b4982d87f41d9a4d3f6237bacfd819145723e35b (Oct 30 2017)
    -- object: f22f8f2881
  , "8aed621ac2b746751585cbe271631394cacc0e01cca4ef589e11b077b0acd291"

    -- commit: 93c1e6eb9e829a66ff213ec076d529ab008880b3 (Dec 16 2017)
    -- object: bfdf04a372
  , "4b10c18a7ad35f032e8cdc0d263716a93878bf06d998b1b66dccff06ceeee89d"

    -- commit: 59812a09eb69cbf12407206381f4c214987b1efd (Apr 3 2018)
    -- object: c03e083607
  , "ce86bba43edb0534c0faa2d6d0f504877576c5271321e3fbd9638fd4667384a2"

    -- commit: a69311708493efe8524aed0e9d19365f79f2fab3 (Oct 24 2018)
    -- object: 06c35c7454
  , "548d676b3e5a52cbfef06d7424ec065c1f34c230407f9f5dc002c27a9666bec4"

    -- commit: 7776566531e72c415f66dd3b13da9041c52076aa (Nov 13 2019)
    -- object: 0b0eeb27d1
  , "7ca43fc2058574846e032bc5493a0ad4568e4fa14fb58558fbf48d3bd6693e59"
  ]


quickJumpCssKnownGoodSha256 :: [String]
quickJumpCssKnownGoodSha256 =
  [
    -- commit: d41abb0f606bf5fdbdc0a7bd3758e0c30601b121 (Sep 23 2017)
    -- object: b69903c3
    "f95b8b12a8a13dd31add93527e1239fdff6997c7f2396e975e2e415db04b75fb"

    -- commit: 0997eb61803a37803ddb6cf7116eb9db1046b2ce (Oct 10 2017)
    -- object: ede05042
  , "59693ef3f0d793031b3af58b214af7884c0f63ce6db659ffd7432cf0aa852b51"

    -- commit: fc069bf200f930c21f96ddbbec1d7c5c69f8ba72 (Jan 15 2018)
    -- object: 468d8036
  , "1d51573b72bc8a7b9b0dda3beffb7882db78d22a37840203f761e3969d915027"

    -- commit: fa5ec121e2a700137bab8bd48cc30b1e80f58fd4 (Feb 27 2019)
    -- object: 8772809c
  , "29fe483bd37ad3feba12f646e9661731127526f246c246b0011b384e11649dff"

    -- commit: 05ccce6e07731f9788a434d6e06f4cadeff3d6ba (Dec 8 2020)
    -- object: d656f51c
  , "6997c223e09b340f5f1bb970c930b458f768a0bbbe787cb87f181820a3d122b3"

    -- commit: 9511e587701349093cbe3ac7c00f13583820774f (Feb 7 2021)
    -- object: cf10eee4
  , "6bd159f6d7b1cfef1bd190f1f5eadcd15d35c6c567330d7465c3c35d5195bc6f"
  ]
