{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, PatternGuards #-}
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

import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Framework.BlobStorage (BlobId)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.ServeTarball as ServerTarball
import Data.TarIndex (TarIndex)

import Distribution.Text
import Distribution.Package

import qualified Data.Map as Map
import Data.Function (fix)

import Text.JSON (showJSON)

-- TODO:
-- 1. Write an HTML view for organizing uploads
-- 2. Have cabal generate a standard doc tarball, and serve that here
data DocumentationFeature = DocumentationFeature {
    documentationFeatureInterface :: HackageFeature,

    queryHasDocumentation :: MonadIO m => PackageIdentifier -> m Bool,

    documentationResource :: DocumentationResource
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
                         -> CoreResource
                         -> IO [PackageIdentifier] 
                         -> UploadFeature
                         -> TarIndexCacheFeature
                         -> IO DocumentationFeature
initDocumentationFeature name
                         env@ServerEnv{serverStateDir, serverVerbosity = verbosity}
                         core
                         getPackages
                         upload
                         tarIndexCache = do
    loginfo verbosity "Initialising documentation feature, start"
    documentationState <- documentationStateComponent name serverStateDir
    let feature = documentationFeature name env core getPackages upload tarIndexCache documentationState
    loginfo verbosity "Initialising documentation feature, end"
    return feature

documentationStateComponent :: String -> FilePath -> IO (StateComponent AcidState Documentation)
documentationStateComponent name stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> name) initialDocumentation
  return StateComponent {
      stateDesc    = "Package documentation"
    , stateHandle  = st
    , getState     = query st GetDocumentation
    , putState     = update st . ReplaceDocumentation
    , backupState  = dumpBackup
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
                     -> StateComponent AcidState Documentation
                     -> DocumentationFeature
documentationFeature name
                     ServerEnv{serverBlobStore = store}
                     CoreResource{ 
                         packageInPath
                       , guardValidPackageId
                       , corePackagePage
                       , corePackagesPage
                       }
                     getPackages
                     UploadFeature{..}
                     TarIndexCacheFeature{cachedTarIndex}
                     documentationState
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

    documentationResource = fix $ \r -> DocumentationResource {
        packageDocsContent = (extendResourcePath "/docs/.." corePackagePage) {
            resourceDesc = [ (GET, "Browse documentation") ]
          , resourceGet  = [ ("", serveDocumentation) ]
          }
      , packageDocsWhole = (extendResourcePath "/docs.:format" corePackagePage) {
            resourceDesc = [ (GET, "Download documentation")
                           , (PUT, "Upload documentation")
                           ]
          , resourceGet  = [ ("tar", serveDocumentationTar) ]
          , resourcePut  = [ ("tar", uploadDocumentation) ]
          }
      , packageDocsStats = (extendResourcePath "/docs.:format" corePackagesPage) {
            resourceDesc = [ (GET, "Get information about which packages have documentation") ]
          , resourceGet  = [ ("json", serveDocumentationStats) ]
          }
      , packageDocsContentUri = \pkgid ->
          renderResource (packageDocsContent r) [display pkgid]
      , packageDocsWholeUri = \format pkgid ->
          renderResource (packageDocsWhole r) [display pkgid, format]
      }

    serveDocumentationStats :: DynamicPath -> ServerPartE Response
    serveDocumentationStats _dpath = do
        pkgs <- mapParaM queryHasDocumentation =<< liftIO getPackages
        return . toResponse . Resource.JSON . showJSON . map aux $ pkgs
      where
        aux :: (PackageIdentifier, Bool) -> (String, Bool) 
        aux (pkgId, hasDocs) = (display pkgId, hasDocs)

    serveDocumentationTar :: DynamicPath -> ServerPartE Response
    serveDocumentationTar dpath =
      withDocumentation dpath $ \_ blob _ -> do
        file <- liftIO $ BlobStorage.fetch store blob
        return $ toResponse $ Resource.DocTarball file blob


    -- return: not-found error or tarball
    serveDocumentation :: DynamicPath -> ServerPartE Response
    serveDocumentation dpath = do
      withDocumentation dpath $ \pkgid blob index -> do
        let tarball = BlobStorage.filepath store blob
            etag    = blobETag blob
        -- if given a directory, the default page is index.html
        -- the root directory within the tarball is e.g. foo-1.0-docs/
        ServerTarball.serveTarball ["index.html"] (display pkgid ++ "-docs")
                                   tarball index etag

    -- return: not-found error (parsing) or see other uri
    uploadDocumentation :: DynamicPath -> ServerPartE Response
    uploadDocumentation dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      -- The order of operations:
      -- * Insert new documentation into blob store
      -- * Generate the new index
      -- * Drop the index for the old tar-file
      -- * Link the new documentation to the package
      fileContents <- expectUncompressedTarball
      blob <- liftIO $ BlobStorage.add store fileContents
      --TODO: validate the tarball here.
      -- Check all files in the tarball are under the dir foo-1.0-docs/
      void $ updateState documentationState $ InsertDocumentation pkgid blob
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

    withDocumentation :: DynamicPath -> (PackageId -> BlobId -> TarIndex -> ServerPartE a) -> ServerPartE a
    withDocumentation dpath func = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      mdocs <- queryState documentationState $ LookupDocumentation pkgid
      case mdocs of
        Nothing -> errNotFound "Not Found" [MText $ "There is no documentation for " ++ display pkgid]
        Just blob -> do
          index <- liftIO $ cachedTarIndex blob
          func pkgid blob index

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

mapParaM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapParaM f = mapM (\x -> (,) x `liftM` f x) 
