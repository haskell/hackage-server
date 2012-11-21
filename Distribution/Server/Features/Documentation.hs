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

import Distribution.Server.Framework.BackupDump
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
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad.State (modify)

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
initDocumentationFeature env@ServerEnv{serverStateDir} core upload = do

    -- Canonical state
    documentationState <- openLocalStateFrom
                            (serverStateDir </> "db" </> "Documentation")
                            initialDocumentation

    return $
      documentationFeature env core upload
                           documentationState

documentationFeature :: ServerEnv
                     -> CoreFeature
                     -> UploadFeature
                     -> AcidState Documentation
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
      , featureCheckpoint  = createCheckpoint documentationState
      , featureShutdown    = closeAcidState documentationState
      , featureDumpRestore = Just hackageFeatureBackup {
            featureBackup     = dumpBackup
          , featureRestore    = restoreBackup
          , featureTestBackup = testRoundtrip
          }
      }

    dumpBackup = do
        doc <- query documentationState GetDocumentation
        let exportFunc (pkgid, (blob, _)) = ([display pkgid, "documentation.tar"], Right blob)
        readExportBlobs store $ map exportFunc . Map.toList $ documentation doc
    restoreBackup = updateDocumentation (Documentation Map.empty)
    -- Checking documentation roundtripped is a bit tricky:
    -- 1. We don't really want to check that the tar index is the same (probably)
    -- 2. We must that the documentation blobs all got imported. To do this we just
    --    need to check they *exist*, due to the MD5-hashing scheme we use.
    testRoundtrip = testRoundtripByQuery' (liftM (Map.map fst . documentation) $ query documentationState GetDocumentation) $ \doc ->
        testBlobsExist store (Map.elems doc)

    queryHasDocumentation :: MonadIO m => PackageIdentifier -> m Bool
    queryHasDocumentation pkgid = query' documentationState (HasDocumentation pkgid)

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
            void $ update' documentationState $ InsertDocumentation pkgid blob tarIndex
            seeOther ("/package/" ++ display pkgid) (toResponse ())

    -- curl -u mgruen:admin -X PUT --data-binary @gtk.tar.gz http://localhost:8080/package/gtk-0.11.0

    withDocumentation :: DynamicPath -> (PackageId -> BlobId -> TarIndex -> ServerPartE a) -> ServerPartE a
    withDocumentation dpath func =
        withPackagePath dpath $ \pkg _ -> do
        let pkgid = packageId pkg
        mdocs <- query' documentationState $ LookupDocumentation pkgid
        case mdocs of
          Nothing -> errNotFound "Not Found" [MText $ "There is no documentation for " ++ display pkgid]
          Just (blob, index) -> func pkgid blob index

    ---- Import
    updateDocumentation :: Documentation -> RestoreBackup
    updateDocumentation docs = fix $ \r -> RestoreBackup
      { restoreEntry = \(entryPath, bs) ->
            case entryPath of
                [str, "documentation.tar"] | Just pkgid <- simpleParse str -> do
                    res <- runImport docs (importDocumentation pkgid bs)
                    return $ fmap updateDocumentation res
                _ -> return . Right $ r
      , restoreFinalize = return . Right $ r
      , restoreComplete = update documentationState $ ReplaceDocumentation docs
      }

    importDocumentation :: PackageId
                        -> ByteString -> Import Documentation ()
    importDocumentation pkgid doc = do
        blobId <- liftIO $ BlobStorage.add store doc
        -- this may fail for a bad tarball
        tarred <- liftIO $ TarIndex.readTarIndex (BlobStorage.filepath store blobId)
        modify $ Documentation . Map.insert pkgid (blobId, tarred) . documentation

