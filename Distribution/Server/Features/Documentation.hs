{-# LANGUAGE PatternGuards #-}

module Distribution.Server.Features.Documentation (
    DocumentationFeature,
    DocumentationResource(..),
    initDocumentationFeature
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Packages.State
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import Distribution.Server.Framework.BlobStorage (BlobId, BlobStorage)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.ServeTarball as TarIndex
import Data.TarIndex (TarIndex)

import Distribution.Text
import Distribution.Package

import Data.Function
import Control.Monad.Trans
import qualified Data.Map as Map
import qualified Codec.Compression.GZip as GZip
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad.State (modify)

-- TODO:
-- 1. Write an HTML view for organizing uploads
-- 2. Have cabal generate a standard doc tarball, and serve that here
data DocumentationFeature = DocumentationFeature {
    documentationResource :: DocumentationResource
}

data DocumentationResource = DocumentationResource {
    packageDocs :: Resource,
    packageDocsUpload :: Resource,
    packageDocTar :: Resource,
    packageDocUri :: PackageId -> String -> String
}

instance HackageFeature DocumentationFeature where
    getFeature docs = HackageModule
      { featureName = "documentation"
      , resources   = map ($documentationResource docs) [packageDocs, packageDocTar, packageDocsUpload]
      , dumpBackup    = Just $ \storage -> do
            doc <- query GetDocumentation
            let exportFunc (pkgid, (blob, _)) = ([display pkgid, "documentation.tar"], Right blob)
            readExportBlobs storage . map exportFunc . Map.toList $ documentation doc
      , restoreBackup = Just $ \storage -> updateDocumentation storage (Documentation Map.empty)
      }


initDocumentationFeature :: ServerEnv -> CoreFeature -> UploadFeature -> IO DocumentationFeature
initDocumentationFeature env _ _ = do
    let store = serverBlobStore env
    return DocumentationFeature
      { documentationResource = fix $ \r -> DocumentationResource
          { packageDocs = (resourceAt "/package/:package/doc/..") { resourceGet = [("", serveDocumentation store)] }
          , packageDocsUpload = (resourceAt "/package/:package/doc/.:format") { resourcePut = [("txt", uploadDocumentation store)] }
          , packageDocTar = (resourceAt "/package/:package/:doc.tar") { resourceGet = [("tar", serveDocumentationTar store)] }
          , packageDocUri = \pkgid str -> renderResource (packageDocs r) [display pkgid, str]
          }
      }
  where

serveDocumentationTar :: BlobStorage -> DynamicPath -> ServerPart Response
serveDocumentationTar store dpath = runServerPartE $ withDocumentation dpath $ \_ blob _ -> do
    file <- liftIO $ BlobStorage.fetch store blob
    return $ toResponse $ Resource.DocTarball file blob


-- return: not-found error or tarball
serveDocumentation :: BlobStorage -> DynamicPath -> ServerPart Response
serveDocumentation store dpath = runServerPartE $ withDocumentation dpath $ \pkgid blob index -> do
    let tarball = BlobStorage.filepath store blob
    -- if given a directory, the default page is index.html
    -- the default directory prefix is the package name itself
    TarIndex.serveTarball ["index.html"] (display $ packageName pkgid) tarball index

-- return: not-found error (parsing) or see other uri
uploadDocumentation :: BlobStorage -> DynamicPath -> ServerPart Response
uploadDocumentation store dpath = runServerPartE $
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
        update $ InsertDocumentation pkgid blob tarIndex
        seeOther ("/package/" ++ display pkgid) (toResponse ())

-- curl -u mgruen:admin -X PUT --data-binary @gtk.tar.gz http://localhost:8080/package/gtk-0.11.0

withDocumentation :: DynamicPath -> (PackageId -> BlobId -> TarIndex -> ServerPartE a) -> ServerPartE a
withDocumentation dpath func =
    withPackagePath dpath $ \pkg _ -> do
    let pkgid = packageId pkg
    mdocs <- query $ LookupDocumentation pkgid
    case mdocs of
      Nothing -> errNotFound "Not Found" [MText $ "There is no documentation for " ++ display pkgid]
      Just (blob, index) -> func pkgid blob index

---- Import 
updateDocumentation :: BlobStorage -> Documentation -> RestoreBackup
updateDocumentation store docs = fix $ \r -> RestoreBackup
  { restoreEntry = \(entryPath, bs) ->
        case entryPath of
            [str, "documentation.tar"] | Just pkgid <- simpleParse str -> do
                res <- runImport docs (importDocumentation store pkgid bs)
                return $ fmap (updateDocumentation store) res
            _ -> return . Right $ r
  , restoreFinalize = return . Right $ r
  , restoreComplete = update $ ReplaceDocumentation docs
  }

importDocumentation :: BlobStorage -> PackageId
                    -> ByteString -> Import Documentation ()
importDocumentation store pkgid doc = do
    blobId <- liftIO $ BlobStorage.add store doc
    -- this may fail for a bad tarball
    tarred <- liftIO $ TarIndex.readTarIndex (BlobStorage.filepath store blobId)
    modify $ Documentation . Map.insert pkgid (blobId, tarred) . documentation

