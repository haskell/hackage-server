module Distribution.Server.Features.Documentation (
    DocumentationFeature(..),
    DocumentationResource(..),
    initDocumentationFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core
import Distribution.Server.Types

import Distribution.Server.Packages.State
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Backup.Export
import Distribution.Server.Util.BlobStorage (BlobId, BlobStorage)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.Util.Serve as TarIndex
import Data.TarIndex (TarIndex)
import qualified Data.TarIndex as TarIndex

import Distribution.Text
import Distribution.Package

import Happstack.Server
import Happstack.State (update, query)
import Data.Function
import Control.Monad.Trans
import Control.Monad (mzero)
import qualified Data.Map as Map
import qualified Codec.Compression.GZip as GZip

data DocumentationFeature = DocumentationFeature {
    documentationResource :: DocumentationResource
}

data DocumentationResource = DocumentationResource {
    packageDocs :: Resource,
    packageDocsUpload :: Resource,
    packageDocUri :: PackageId -> String -> String
}

instance HackageFeature DocumentationFeature where
    getFeature docs = HackageModule
      { featureName = "documentation"
      , resources   = map ($documentationResource docs) [packageDocs]
      , dumpBackup    = Just $ \storage -> do
            doc <- query GetDocumentation
            let exportFunc (pkgid, (blob, _)) = ([display pkgid, "documentation.tar"], Right blob)
            readExportBlobs storage . map exportFunc . Map.toList $ documentation doc
      , restoreBackup = Nothing {-Just $ \storage -> reportsBackup storage-}
      }

initDocumentationFeature :: Config -> CoreFeature -> UploadFeature -> IO DocumentationFeature
initDocumentationFeature config _ _ = do
    let store = serverStore config
    return DocumentationFeature
      { documentationResource = fix $ \r -> DocumentationResource
          { packageDocs = (resourceAt "/package/:package/doc/..") { resourceGet = [("", serveDocumentation store)] }
          , packageDocsUpload = (resourceAt "/package/:package/doc/.:format") { resourcePost = [("", uploadDocumentation store)] }
          , packageDocUri = \pkgid str -> renderResource (packageDocs r) [display pkgid, str]
          }
      }
  where

serveDocumentation :: BlobStorage -> DynamicPath -> ServerPart Response
serveDocumentation store dpath = withDocumentation dpath $ \pkgid blob index -> do
    let tarball = BlobStorage.filepath store blob
    TarIndex.serveTarball ["index.html"] (display $ packageName pkgid) tarball index

uploadDocumentation :: BlobStorage -> DynamicPath -> ServerPart Response
uploadDocumentation store dpath = withPackageId dpath $ \pkgid -> do
    requirePackageAuth pkgid
    withRequest $ \req -> do
        {-
          The order of operations:
          - Insert new documentation into blob store
          - Generate the new index
          - Drop the index for the old tar-file
          - Link the new documentation to the package
        -}
        let Body fileContents = rqBody req
        blob <- liftIO $ BlobStorage.add store (GZip.decompress fileContents)
        tarIndex <- liftIO $ TarIndex.readTarIndex (BlobStorage.filepath store blob)
        update $ InsertDocumentation pkgid blob tarIndex
        seeOther ("/package/" ++ display pkgid) (toResponse ())

withDocumentation :: DynamicPath -> (PackageId -> BlobId -> TarIndex -> ServerPart Response) -> ServerPart Response
withDocumentation dpath func = withPackageId dpath $ \pkgid -> do
    mdocs <- query $ LookupDocumentation pkgid
    case mdocs of
        Nothing -> mzero
        Just (blob, index) -> func pkgid blob index

