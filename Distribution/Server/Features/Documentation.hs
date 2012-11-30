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

import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import Distribution.Server.Framework.BlobStorage (BlobId)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.ServeTarball as TarIndex
import Data.TarIndex (TarIndex)

import Distribution.Text
import Distribution.Package

import Data.Function
import qualified Data.Map as Map
import qualified Codec.Compression.GZip as GZip

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
    packageDocs :: Resource,
    packageDocsUpload :: Resource,
    packageDocTar :: Resource,
    packageDocUri :: PackageId -> String -> String
}

initDocumentationFeature :: ServerEnv -> CoreFeature -> UploadFeature -> IO DocumentationFeature
initDocumentationFeature env@ServerEnv{serverStateDir, serverBlobStore} core upload = do
    documentationState <- documentationStateComponent serverBlobStore serverStateDir
    return $ documentationFeature env core upload documentationState

documentationStateComponent :: BlobStorage -> FilePath -> IO (StateComponent Documentation)
documentationStateComponent store stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Documentation") initialDocumentation
  return StateComponent {
      stateDesc    = "Package documentation"
    , acidState    = st
    , getState     = query st GetDocumentation
    , backupState  = dumpBackup
    , restoreState = updateDocumentation st
    , resetState   = documentationStateComponent
    }
  where
    dumpBackup doc =
        let exportFunc (pkgid, (blob, _)) = BackupBlob ([display pkgid, "documentation.tar"]) blob
        in map exportFunc . Map.toList $ documentation doc

    updateDocumentation :: AcidState Documentation -> RestoreBackup
    updateDocumentation st =
      fromPureRestoreBackup store
        (update st . ReplaceDocumentation)
        (updateDocumentationPure (Documentation Map.empty))

    updateDocumentationPure :: Documentation -> PureRestoreBackup Documentation
    updateDocumentationPure docs = PureRestoreBackup {
        pureRestoreEntry = \entry ->
          case entry of
            BackupBlob [str, "documentation.tar"] blobId | Just pkgId <- simpleParse str -> do
              docs' <- importDocumentation pkgId blobId docs
              return (updateDocumentationPure docs')
            _ ->
              return (updateDocumentationPure docs)
      , pureRestoreFinalize = return docs
      }

    importDocumentation :: PackageId -> BlobId -> Documentation -> Restore Documentation
    importDocumentation pkgId blobId (Documentation docs) = do
      tar <- restoreGetBlob blobId
      case TarIndex.constructTarIndex tar of
        Left err ->
          fail err
        Right tarIndex ->
          return (Documentation (Map.insert pkgId (blobId, tarIndex) docs))

documentationFeature :: ServerEnv
                     -> CoreFeature
                     -> UploadFeature
                     -> StateComponent Documentation
                     -> DocumentationFeature
documentationFeature ServerEnv{serverBlobStore = store}
                     CoreFeature{..} UploadFeature{..}
                     documentationState
  = DocumentationFeature{..}
  where
    documentationFeatureInterface = (emptyHackageFeature "documentation") {
        featureResources =
          map ($ documentationResource) [
              packageDocs
            , packageDocTar
            , packageDocsUpload
            ]
        -- We don't really want to check that the tar index is the same (probably)
      , featureState = [abstractStateComponent' (compareState `on` (Map.map fst . documentation)) documentationState]
      }

    queryHasDocumentation :: MonadIO m => PackageIdentifier -> m Bool
    queryHasDocumentation pkgid = queryState documentationState (HasDocumentation pkgid)

    documentationResource = DocumentationResource {
        packageDocs = (resourceAt "/package/:package/doc/..") { resourceGet = [("", serveDocumentation)] }
      , packageDocsUpload = (resourceAt "/package/:package/doc/.:format") { resourcePut = [("txt", uploadDocumentation)] }
      , packageDocTar = (resourceAt "/package/:package/:doc.tar") { resourceGet = [("tar", serveDocumentationTar)] }
      , packageDocUri = \pkgid str -> renderResource (packageDocs documentationResource) [display pkgid, str]
      }

    serveDocumentationTar :: DynamicPath -> ServerPart Response
    serveDocumentationTar dpath = runServerPartE $ withDocumentation dpath $ \_ blob _ -> do
        file <- liftIO $ BlobStorage.fetch store blob
        return $ toResponse $ Resource.DocTarball file blob


    -- return: not-found error or tarball
    serveDocumentation :: DynamicPath -> ServerPart Response
    serveDocumentation dpath = runServerPartE $ withDocumentation dpath $ \pkgid blob index -> do
        let tarball = BlobStorage.filepath store blob
        -- if given a directory, the default page is index.html
        -- the default directory prefix is the package name itself
        TarIndex.serveTarball ["index.html"] (display $ packageName pkgid) tarball index

    -- return: not-found error (parsing) or see other uri
    uploadDocumentation :: DynamicPath -> ServerPart Response
    uploadDocumentation dpath = runServerPartE $
                                withPackageId dpath $ \pkgid ->
                                withPackageAuth pkgid $ \_ _ -> do
            -- The order of operations:
            -- * Insert new documentation into blob store
            -- * Generate the new index
            -- * Drop the index for the old tar-file
            -- * Link the new documentation to the package
            Body fileContents <- consumeRequestBody
            blob <- liftIO $ BlobStorage.add store (GZip.decompress fileContents)
            tarIndex <- liftIO $ TarIndex.readTarIndex (BlobStorage.filepath store blob)
            void $ updateState documentationState $ InsertDocumentation pkgid blob tarIndex
            seeOther ("/package/" ++ display pkgid) (toResponse ())

    -- curl -u mgruen:admin -X PUT --data-binary @gtk.tar.gz http://localhost:8080/package/gtk-0.11.0

    withDocumentation :: DynamicPath -> (PackageId -> BlobId -> TarIndex -> ServerPartE a) -> ServerPartE a
    withDocumentation dpath func =
        withPackagePath dpath $ \pkg _ -> do
        let pkgid = packageId pkg
        mdocs <- queryState documentationState $ LookupDocumentation pkgid
        case mdocs of
          Nothing -> errNotFound "Not Found" [MText $ "There is no documentation for " ++ display pkgid]
          Just (blob, index) -> func pkgid blob index

