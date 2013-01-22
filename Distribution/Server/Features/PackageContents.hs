{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature,
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core

import Distribution.Server.Packages.Types
import Distribution.Server.Util.ChangeLog (lookupTarballAndConstructTarIndex, lookupChangeLog)
import qualified Distribution.Server.Util.ServeTarball as TarIndex
import Data.TarIndex (TarIndex)

import Distribution.Text
import Distribution.Package


-- FIXME: cache TarIndexes?

data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource
}

instance IsHackageFeature PackageContentsFeature where
    getFeatureInterface = packageFeatureInterface

data PackageContentsResource = PackageContentsResource {
    packageContents             :: Resource,
    packageContentsChangeLog    :: Resource,
    packageContentsChangeLogUri :: PackageId -> String
}

initPackageContentsFeature :: ServerEnv -> CoreFeature -> IO PackageContentsFeature
initPackageContentsFeature env@ServerEnv{serverVerbosity = verbosity} core = do
    loginfo verbosity "Initialising package-contents feature, start"

    -- currently no state
    let feature = packageContentsFeature env core

    loginfo verbosity "Initialising package-contents feature, end"
    return feature


packageContentsFeature :: ServerEnv
                       -> CoreFeature
                       -> PackageContentsFeature

packageContentsFeature ServerEnv{serverBlobStore = store}
                       CoreFeature{..}
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

    --TODO: use something other than runServerPartE for nice html error pages

    withPackagePath' :: DynamicPath -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withPackagePath'   dpath k = withPackagePath   dpath $ \pkg _ -> k pkg

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPart Response
    serveChangeLog = serveChangeLog' withPackagePath'

    serveChangeLog' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                    -> DynamicPath -> ServerPart Response
    serveChangeLog' with_pkg_path dpath = runServerPartE $ with_pkg_path dpath $ \pkg -> do
        res <- liftIO $ lookupChangeLog store pkg
        case res of
          Left err -> errNotFound "Changelog not found" [MText err]
          Right (fp, offset, name) -> liftIO $ TarIndex.serveTarEntry fp offset name

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPart Response
    serveContents = serveContents' withPackagePath'

    serveContents' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                   -> DynamicPath -> ServerPart Response
    serveContents' with_pkg_path dpath = runServerPartE $ withContents $ \pkgid tarball index -> do
        -- if given a directory, the default page is index.html
        -- the default directory prefix is the package name itself (including the version)
        TarIndex.serveTarball ["index.html"] (display pkgid) tarball index
      where
        withContents :: (PackageId -> FilePath -> TarIndex -> ServerPartE Response) -> ServerPartE Response
        withContents func = with_pkg_path dpath $ \pkg ->
            case lookupTarballAndConstructTarIndex store pkg of
                Nothing -> fail "Could not serve package contents: no tarball exists."
                Just io -> liftIO io >>= \(fp, index) -> func (packageId pkg) fp index
