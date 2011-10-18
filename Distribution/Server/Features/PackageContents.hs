module Distribution.Server.Features.PackageContents (
    PackageContentsFeature,
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Check
import Distribution.Server.Features.Core

import Distribution.Server.Packages.Types
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Util.ChangeLog (lookupTarball, lookupChangeLog)
import qualified Distribution.Server.Util.ServeTarball as TarIndex
import Data.TarIndex (TarIndex)

import Distribution.Text
import Distribution.Package

import Control.Monad.Trans


-- FIXME: cache TarIndexes?

data PackageContentsFeature = PackageContentsFeature {
    featureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource
}

data PackageContentsResource = PackageContentsResource {
    packageContents                   :: Resource,
    packageContentsChangeLog          :: Resource,
    packageContentsCandidateChangeLog :: Resource,

    packageContentsChangeLogUri          :: PackageId -> String,
    packageContentsCandidateChangeLogUri :: PackageId -> String
}

instance IsHackageFeature PackageContentsFeature where
    getFeatureInterface = featureInterface

initPackageContentsFeature :: ServerEnv -> CoreFeature -> CheckFeature -> IO PackageContentsFeature
initPackageContentsFeature env _ _ = do
    let store = serverBlobStore env
        resources = PackageContentsResource {
              packageContents                   = (resourceAt "/package/:package/src/..") { resourceGet = [("", serveContents store)] }
            , packageContentsChangeLog          = (resourceAt "/package/:package/changelog") { resourceGet = [("changelog", runServerPartE . serveChangeLog store)] }
            , packageContentsCandidateChangeLog = (resourceAt "/package/:package/candidate/changelog") { resourceGet = [("changelog", serveCandidateChangeLog store)] }

            , packageContentsChangeLogUri          = \pkgid -> renderResource (packageContentsChangeLog          resources) [display pkgid, display (packageName pkgid)]
            , packageContentsCandidateChangeLogUri = \pkgid -> renderResource (packageContentsCandidateChangeLog resources) [display pkgid, display (packageName pkgid)]
          }
    return PackageContentsFeature {
        featureInterface = (emptyHackageFeature "package-contents") {
          featureResources = map ($ resources) [packageContents, packageContentsChangeLog, packageContentsCandidateChangeLog]
        }
      , packageContentsResource = resources
      }


-- result: changelog or not-found error
serveChangeLog :: BlobStorage -> DynamicPath -> ServerPartE Response
serveChangeLog store dpath = withPackagePath dpath $ \pkg _ -> do
    res <- liftIO $ lookupChangeLog store pkg
    case res of
      Left err -> errNotFound "Changelog not found" [MText err]
      Right (fp, offset, name) -> liftIO $ TarIndex.serveTarEntry fp offset name

serveCandidateChangeLog :: BlobStorage -> DynamicPath -> ServerPart Response
serveCandidateChangeLog store dpath =
    runServerPartE $ --TODO: use something else for nice html error pages
    withCandidatePath dpath $ \_ pkg -> do
          res <- liftIO $ lookupChangeLog store (candPkgInfo pkg)
          case res of
            Left err -> errNotFound "Changelog not found" [MText err]
            Right (fp, offset, name) -> liftIO $ TarIndex.serveTarEntry fp offset name

-- return: not-found error or tarball
serveContents :: BlobStorage -> DynamicPath -> ServerPart Response
serveContents store dpath = runServerPartE $ withContents store dpath $ \pkgid tarball index -> do
    -- if given a directory, the default page is index.html
    -- the default directory prefix is the package name itself (including the version)
    TarIndex.serveTarball ["index.html"] (display pkgid) tarball index

withContents :: BlobStorage -> DynamicPath -> (PackageId -> FilePath -> TarIndex -> ServerPartE a) -> ServerPartE a
withContents store dpath func =
    withPackagePath dpath $ \pkg _ -> do
    let pkgid = packageId pkg
    case lookupTarball store pkg of
        Nothing -> fail "Could not serve package contents: no tarball exists."
        Just io -> liftIO io >>= \(fp, index) -> func pkgid fp index
