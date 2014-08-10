{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature(..),
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ChangeLog
import Distribution.Server.Util.ServeTarball (serveTarEntry, serveTarball)
import qualified Data.TarIndex as TarIndex

import Distribution.Text
import Distribution.Package

import Control.Monad.Error (ErrorT(..))

data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource,

    -- Functionality exported to other features
    packageTarball   :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarIndex)),
    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarEntryOffset, FilePath))
}

instance IsHackageFeature PackageContentsFeature where
    getFeatureInterface = packageFeatureInterface

data PackageContentsResource = PackageContentsResource {
    packageContents             :: Resource,
    packageContentsChangeLog    :: Resource,
    packageContentsChangeLogUri :: PackageId -> String
}

initPackageContentsFeature :: ServerEnv
                           -> CoreFeature
                           -> TarIndexCacheFeature
                           -> IO PackageContentsFeature
initPackageContentsFeature env@ServerEnv{serverVerbosity = verbosity}
                           core
                           tarIndexCache = do
    loginfo verbosity "Initialising package-contents feature, start"

    let feature = packageContentsFeature env core tarIndexCache

    loginfo verbosity "Initialising package-contents feature, end"
    return feature

packageContentsFeature :: ServerEnv
                       -> CoreFeature
                       -> TarIndexCacheFeature
                       -> PackageContentsFeature

packageContentsFeature ServerEnv{serverBlobStore = store}
                       CoreFeature{ coreResource = CoreResource{
                                      packageInPath
                                    , lookupPackageId
                                    }
                                  }
                       TarIndexCacheFeature{cachedPackageTarIndex}
  = PackageContentsFeature{..}
  where
    packageFeatureInterface = (emptyHackageFeature "package-contents") {
        featureResources =
          map ($ packageContentsResource) [
              packageContents
            , packageContentsChangeLog
            ]
      , featureState = []
      , featureDesc = "The PackageContents feature shows the contents of packages and caches their TarIndexes"
      }

    packageContentsResource = PackageContentsResource {
          packageContents = (resourceAt "/package/:package/src/..") {
              resourceGet = [("", serveContents)]
            }
        , packageContentsChangeLog = (resourceAt "/package/:package/changelog") {
              resourceGet = [("changelog", serveChangeLog)]
            }
        , packageContentsChangeLogUri = \pkgid ->
            renderResource (packageContentsChangeLog packageContentsResource) [display pkgid, display (packageName pkgid)]
        }

{-------------------------------------------------------------------------------
  TODO: everything below is duplicated in PackageCandidates.
-------------------------------------------------------------------------------}

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPartE Response
    serveChangeLog dpath = do
      pkg        <- packageInPath dpath >>= lookupPackageId
      mChangeLog <- liftIO $ packageChangeLog pkg
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (fp, etag, offset, name) -> do
          useETag etag
          liftIO $ serveTarEntry fp offset name

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPartE Response
    serveContents dpath = do
      pkg      <- packageInPath dpath >>= lookupPackageId
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
          etag   = BlobStorage.blobETag blobid
      index <- cachedPackageTarIndex pkgTarball
      return $ Right (fp, etag, index)
    packageTarball _ =
      return $ Left "No tarball found"

    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarEntryOffset, FilePath))
    packageChangeLog pkgInfo = runErrorT $ do
      (fp, etag, index) <- ErrorT $ packageTarball pkgInfo
      (offset, fname)   <- ErrorT $ return . maybe (Left "No changelog found") Right
                                  $ findChangeLog pkgInfo index
      return (fp, etag, offset, fname)
