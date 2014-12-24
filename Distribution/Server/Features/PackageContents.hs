{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature(..),
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache

import Distribution.Server.Packages.ChangeLog
import Distribution.Server.Packages.Types
import Distribution.Server.Util.ServeTarball (serveTarEntry, serveTarball)
import qualified Data.TarIndex as TarIndex
import qualified Data.ByteString.Lazy as BS

import Distribution.Text
import Distribution.Package


data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource,
    -- TODO: Exported temporarily from TarIndexCache for RecentPackages's `render`
    --       which in turn is only in RecentPackages because PackageContents doesn't
    --       have access to the Users feature. Refactor.
    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex.TarEntryOffset, FilePath, BS.ByteString))
    
  
}

instance IsHackageFeature PackageContentsFeature where
    getFeatureInterface = packageFeatureInterface

data PackageContentsResource = PackageContentsResource {
    packageContents             :: Resource,
    packageContentsChangeLog    :: Resource,
    packageContentsChangeLogUri :: PackageId -> String
}

initPackageContentsFeature :: ServerEnv
                           -> IO (CoreFeature
                               -> TarIndexCacheFeature
                               -> IO PackageContentsFeature)
initPackageContentsFeature env@ServerEnv{} = do
    return $ \core tarIndexCache -> do
      let feature = packageContentsFeature env core tarIndexCache

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
                       TarIndexCacheFeature{packageTarball, packageChangeLog}
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
        Right (fp, etag, offset, name, _contents) -> do
          cacheControl [Public, maxAgeDays 30] etag
          liftIO $ serveTarEntry fp offset name  -- TODO: We've already loaded the contents; refactor (also in Candidates)

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPartE Response
    serveContents dpath = do
      pkg      <- packageInPath dpath >>= lookupPackageId
      mTarball <- liftIO $ packageTarball pkg
      case mTarball of
        Left err ->
          errNotFound "Could not serve package contents" [MText err]
        Right (fp, etag, index) ->
          serveTarball ["index.html"] (display (packageId pkg)) fp index
                       [Public, maxAgeDays 30] etag
