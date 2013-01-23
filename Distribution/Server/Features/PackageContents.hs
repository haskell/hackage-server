{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature(..),
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Util.ServeTarball as ServeTarball
import Data.TarIndex (TarIndex)
import qualified Data.TarIndex as TarIndex
import qualified Text.PrettyPrint.HughesPJ as Pretty (render)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Text
import Distribution.Package


-- FIXME: cache TarIndexes?

data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource,

    -- Functionality exported to other features
    packageTarball   :: PkgInfo -> IO (Maybe (FilePath, TarIndex.TarIndex)),
    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, TarIndex.TarEntryOffset, String))
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
                       CoreFeature{..}
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

    --TODO: use something other than runServerPartE for nice html error pages

    withPackagePath' :: DynamicPath -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withPackagePath'   dpath k = withPackagePath   dpath $ \pkg _ -> k pkg

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPart Response
    serveChangeLog = serveChangeLog' withPackagePath'

    serveChangeLog' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                    -> DynamicPath -> ServerPart Response
    serveChangeLog' with_pkg_path dpath = runServerPartE $ with_pkg_path dpath $ \pkg -> do
        mChangeLog <- liftIO $ packageChangeLog pkg
        case mChangeLog of
          Left err ->
            errNotFound "Changelog not found" [MText err]
          Right (fp, offset, name) ->
            liftIO $ ServeTarball.serveTarEntry fp offset name

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPart Response
    serveContents = serveContents' withPackagePath'

    serveContents' :: (DynamicPath -> ((PkgInfo -> ServerPartE Response) -> ServerPartE Response))
                   -> DynamicPath -> ServerPart Response
    serveContents' with_pkg_path dpath = runServerPartE $ withContents $ \pkgid tarball index -> do
        -- if given a directory, the default page is index.html
        -- the default directory prefix is the package name itself (including the version)
        ServeTarball.serveTarball ["index.html"] (display pkgid) tarball index
      where
        withContents :: (PackageId -> FilePath -> TarIndex -> ServerPartE Response) -> ServerPartE Response
        withContents func = with_pkg_path dpath $ \pkg -> do
            mTarball <- liftIO $ packageTarball pkg
            case mTarball of
              Nothing ->
                fail "Could not serve package contents: no tarball exists."
              Just (fp, index) ->
                func (packageId pkg) fp index

    packageTarball :: PkgInfo -> IO (Maybe (FilePath, TarIndex.TarIndex))
    packageTarball PkgInfo{pkgTarball = (pkgTarball, _) : _} = do
      let fp = BlobStorage.filepath store (pkgTarballNoGz pkgTarball)
      index <- cachedPackageTarIndex pkgTarball
      return $ Just (fp, index)
    packageTarball _ =
      return Nothing

    packageChangeLog :: PkgInfo -> IO (Either String (FilePath, TarIndex.TarEntryOffset, String))
    packageChangeLog pkgInfo = do
      mTarball <- packageTarball pkgInfo
      case mTarball of
        Nothing ->
          return $ Left "Could not extract changelog: no tarball exists."
        Just (fp, index) ->
          case msum $ map (lookupFile index) candidates of
            Just (name, offset) -> return $ Right (fp, offset, name)
            Nothing ->
                do let msg = "No changelog found, files considered: " ++ show candidates
                   return $ Left msg
      where
        lookupFile index fname =
            do entry <- TarIndex.lookup index fname
               case entry of
                 TarIndex.TarFileEntry offset -> return (fname, offset)
                 _ -> fail "is a directory"
        candidates =
            let l = ["ChangeLog", "CHANGELOG", "CHANGE_LOG", "Changelog", "changelog"]
                pkgId = Pretty.render $ disp (pkgInfoId pkgInfo)
            in map (pkgId </>) $ map (++ ".html") l ++ l
