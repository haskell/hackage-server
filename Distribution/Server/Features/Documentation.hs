{-# LANGUAGE RankNTypes, FlexibleContexts,
             NamedFieldPuns, RecordWildCards, PatternGuards #-}
module Distribution.Server.Features.Documentation (
    DocumentationFeature(..),
    DocumentationResource(..),
    initDocumentationFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Documentation.State
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache
import Distribution.Server.Features.BuildReports
import Distribution.Version ( Version )

import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Framework.BlobStorage (BlobId)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.ServeTarball as ServerTarball
import qualified Distribution.Server.Util.DocMeta as DocMeta
import Distribution.Server.Features.BuildReports.BuildReport (PkgDetails(..), BuildStatus(..))
import Data.TarIndex (TarIndex)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar

import Distribution.Text
import Distribution.Package
import Distribution.Version (nullVersion)
import qualified Distribution.Parsec as P

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Function (fix)

import Data.Aeson (toJSON)
import Data.Maybe
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Directory (getModificationTime)
import Control.Applicative
-- TODO:
-- 1. Write an HTML view for organizing uploads
-- 2. Have cabal generate a standard doc tarball, and serve that here
data DocumentationFeature = DocumentationFeature {
    documentationFeatureInterface :: HackageFeature,

    queryHasDocumentation   :: forall m. MonadIO m => PackageIdentifier -> m Bool,
    queryDocumentation      :: forall m. MonadIO m => PackageIdentifier -> m (Maybe BlobId),
    queryDocumentationIndex :: forall m. MonadIO m => m (Map.Map PackageId BlobId),

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
                             -> IO DocumentationFeature)
initDocumentationFeature name
                         env@ServerEnv{serverStateDir} = do
    -- Canonical state
    documentationState <- documentationStateComponent name serverStateDir

    -- Hooks
    documentationChangeHook <- newHook

    return $ \core getPackages upload tarIndexCache reportsCore -> do
      let feature = documentationFeature name env
                                         core getPackages upload tarIndexCache reportsCore
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
        let exportFunc (pkgid, blob) = BackupBlob ([display pkgid, "documentation.tar"]) blob
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
                     -> StateComponent AcidState Documentation
                     -> Hook PackageId ()
                     -> DocumentationFeature
documentationFeature name
                     ServerEnv{serverBlobStore = store, serverBaseURI}
                     CoreResource{
                         packageInPath
                       , guardValidPackageId
                       , corePackagePage
                       , corePackagesPage
                       , lookupPackageId
                       }
                     getPackages
                     UploadFeature{..}
                     TarIndexCacheFeature{cachedTarIndex}
                     ReportsFeature{..}
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
        parsePkgs pkgsStr = map fromJust $ filter isJust $ map (P.simpleParsec . C.unpack) $ C.split ',' (C.pack pkgsStr)

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
        age <- liftIO $ getFileAge tarball
        let maxAge = documentationCacheTime age
        ServerTarball.serveTarball (display pkgid ++ " documentation")
                                   [{-no index-}] (display pkgid ++ "-docs")
                                   tarball index [Public, maxAge] etag

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
      fileContents <- expectUncompressedTarball
      mres <- liftIO $ BlobStorage.addWith store fileContents
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

    withDocumentation :: Resource -> DynamicPath
                      -> (PackageId -> BlobId -> TarIndex -> ServerPartE Response)
                      -> ServerPartE Response
    withDocumentation self dpath func = do
      pkgid <- packageInPath dpath

      -- lookupPackageId returns the latest version if no version is specified.
      pkginfo <- lookupPackageId pkgid

      -- Set up the canonical URL to point to the unversioned path
      let basedpath =
            [ if var == "package"
                then (var, unPackageName $ pkgName pkgid)
                else e
            | e@(var, _) <- dpath ]
          basePkgPath   = (renderResource' self basedpath)
          canonicalLink = show serverBaseURI ++ basePkgPath
          canonicalHeader = "<" ++ canonicalLink ++ ">; rel=\"canonical\""

      -- Link: <http://canonical.link>; rel="canonical"
      -- See https://support.google.com/webmasters/answer/139066?hl=en#6
      setHeaderM "Link" canonicalHeader

      case pkgVersion pkgid == nullVersion of
        -- if no version is given we want to redirect to the latest version
        True -> tempRedirect latestPkgPath (toResponse "")
          where
            latest = packageId pkginfo
            dpath' = [ if var == "package"
                         then (var, display latest)
                         else e
                     | e@(var, _) <- dpath ]
            latestPkgPath = (renderResource' self dpath')

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
              where
                -- Essentially errNotFound, but overloaded to specify a header.
                -- (Needed since errNotFound throws away result of setHeaderM)
                errNotFoundH title message = throwError
                  (ErrorResponse 404
                  [("Link", canonicalHeader)]
                  title message)
            Just blob -> do
              index <- liftIO $ cachedTarIndex blob
              func pkgid blob index


-- Check the tar file is well formed and all files are within foo-1.0-docs/
checkDocTarball :: PackageId -> BSL.ByteString -> Either String ()
checkDocTarball pkgid =
     checkEntries
   . fmapErr (either id show) . Tar.checkTarbomb (display pkgid ++ "-docs")
   . fmapErr (either id show) . Tar.checkSecurity
   . fmapErr (either id show) . Tar.checkPortability
   . fmapErr show             . Tar.read
  where
    fmapErr f    = Tar.foldEntries Tar.Next Tar.Done (Tar.Fail . f)
    checkEntries = Tar.foldEntries checkEntry (Right ()) Left

    checkEntry entry remainder
      | Tar.entryPath entry == docMetaPath = checkDocMeta entry remainder
      | otherwise                          = remainder

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
mapParaM f = mapM (\x -> (,) x `liftM` f x)

getFileAge :: FilePath -> IO NominalDiffTime
getFileAge file = diffUTCTime <$> getCurrentTime <*> getModificationTime file
