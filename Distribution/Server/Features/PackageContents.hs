{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature,
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Check
import Distribution.Server.Features.Core

import Distribution.Server.Packages.Types
import Distribution.Server.Util.ChangeLog (lookupTarball, lookupChangeLog)
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
    packageContents                   :: Resource,
    packageContentsCandidate          :: Resource,
    packageContentsChangeLog          :: Resource,
    packageContentsCandidateChangeLog :: Resource,

    packageContentsChangeLogUri          :: PackageId -> String,
    packageContentsCandidateChangeLogUri :: PackageId -> String
}

initPackageContentsFeature :: ServerEnv -> CoreFeature -> CheckFeature -> IO PackageContentsFeature
initPackageContentsFeature env core check = do

    -- currently no state

    return $
      packageContentsFeature env core check


packageContentsFeature :: ServerEnv
                       -> CoreFeature
                       -> CheckFeature
                       -> PackageContentsFeature

packageContentsFeature ServerEnv{serverBlobStore = store}
                       CoreFeature{..} CheckFeature{..}
  = PackageContentsFeature{..}
  where
    packageFeatureInterface = (emptyHackageFeature "package-contents") {
        featureResources = map ($ packageContentsResource) [packageContents, packageContentsCandidate, packageContentsChangeLog, packageContentsCandidateChangeLog]
    }

    packageContentsResource = PackageContentsResource {
              packageContents                   = (resourceAt "/package/:package/src/..") { resourceGet = [("", serveContents)] }
            , packageContentsCandidate          = (resourceAt "/package/:package/candidate/src/..") { resourceGet = [("", serveCandidateContents)] }
            , packageContentsChangeLog          = (resourceAt "/package/:package/changelog") { resourceGet = [("changelog", serveChangeLog)] }
            , packageContentsCandidateChangeLog = (resourceAt "/package/:package/candidate/changelog") { resourceGet = [("changelog", serveCandidateChangeLog)] }

            , packageContentsChangeLogUri          = \pkgid -> renderResource (packageContentsChangeLog          packageContentsResource) [display pkgid, display (packageName pkgid)]
            , packageContentsCandidateChangeLogUri = \pkgid -> renderResource (packageContentsCandidateChangeLog packageContentsResource) [display pkgid, display (packageName pkgid)]
          }

    --TODO: use something other than runServerPartE for nice html error pages

    withPackagePath', withCandidatePath' :: DynamicPath -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withPackagePath'   dpath k = withPackagePath   dpath $ \pkg _ -> k pkg
    withCandidatePath' dpath k = withCandidatePath dpath $ \_ pkg -> k (candPkgInfo pkg)

    -- result: changelog or not-found error
    serveChangeLog, serveCandidateChangeLog :: DynamicPath -> ServerPart Response
    serveChangeLog = serveChangeLog' withPackagePath'
    serveCandidateChangeLog = serveChangeLog' withCandidatePath'

    serveChangeLog' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                    -> DynamicPath -> ServerPart Response
    serveChangeLog' with_pkg_path dpath = runServerPartE $ with_pkg_path dpath $ \pkg -> do
        res <- liftIO $ lookupChangeLog store pkg
        case res of
          Left err -> errNotFound "Changelog not found" [MText err]
          Right (fp, offset, name) -> liftIO $ TarIndex.serveTarEntry fp offset name

    -- return: not-found error or tarball
    serveContents, serveCandidateContents :: DynamicPath -> ServerPart Response
    serveContents = serveContents' withPackagePath'
    serveCandidateContents = serveContents' withCandidatePath'

    serveContents' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                   -> DynamicPath -> ServerPart Response
    serveContents' with_pkg_path dpath = runServerPartE $ withContents $ \pkgid tarball index -> do
        -- if given a directory, the default page is index.html
        -- the default directory prefix is the package name itself (including the version)
        TarIndex.serveTarball ["index.html"] (display pkgid) tarball index
      where
        withContents :: (PackageId -> FilePath -> TarIndex -> ServerPartE Response) -> ServerPartE Response
        withContents func = with_pkg_path dpath $ \pkg ->
            case lookupTarball store pkg of
                Nothing -> fail "Could not serve package contents: no tarball exists."
                Just io -> liftIO io >>= \(fp, index) -> func (packageId pkg) fp index
