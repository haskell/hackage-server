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
import Distribution.Server.Packages.Readme
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render
import Distribution.Server.Features.Users
import Distribution.Server.Util.ServeTarball (serveTarEntry, serveTarball)


import Distribution.Text
import Distribution.Package


data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource,

    -- necessary information for the representation of a package resource
    -- This needs to be here in order to extract from the tar file
    packageRender :: PkgInfo -> IO PackageRender
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
                               -> UserFeature
                               -> IO PackageContentsFeature)
initPackageContentsFeature _ = do
    return $ \core tarIndexCache user -> do
      let feature = packageContentsFeature core tarIndexCache user

      return feature

packageContentsFeature :: CoreFeature
                       -> TarIndexCacheFeature
                       -> UserFeature
                       -> PackageContentsFeature

packageContentsFeature CoreFeature{ coreResource = CoreResource{
                                      packageInPath
                                    , lookupPackageId
                                    }
                                  }
                       TarIndexCacheFeature{packageTarball, findToplevelFile}
                       UserFeature{queryGetUserDb}
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
        
    packageRender :: PkgInfo -> IO PackageRender
    packageRender pkg = do
      users <- queryGetUserDb
      mChangeLog <- findToplevelFile pkg isChangeLogFile
      mReadme <- findToplevelFile pkg isReadmeFile
      let changeLog = case mChangeLog of Right (_,_,_,fname,contents) -> Just (fname, contents) 
                                         _                            -> Nothing
          readme    = case mReadme    of Right (_,_,_,fname,contents) -> Just (fname, contents) 
                                         _                            -> Nothing
          render = doPackageRender users pkg
      return $ render { rendChangeLog = changeLog, rendReadme = readme }

{-------------------------------------------------------------------------------
  TODO: everything below is duplicated in PackageCandidates.
-------------------------------------------------------------------------------}

    -- result: changelog or not-found error
    serveChangeLog :: DynamicPath -> ServerPartE Response
    serveChangeLog dpath = do
      pkg        <- packageInPath dpath >>= lookupPackageId
      mChangeLog <- liftIO $ findToplevelFile pkg isChangeLogFile
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (fp, etag, offset, name, _contents) -> do
          cacheControl [Public, maxAgeDays 30] etag
          liftIO $ serveTarEntry fp offset name  -- TODO: We've already loaded the contents
                                                 -- we do repeated work here by re-seeking in the tar.
                                                 -- This should be refactored; same thing in PackageCandidates

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPartE Response
    serveContents dpath = do
      pkg      <- packageInPath dpath >>= lookupPackageId
      mTarball <- liftIO $ packageTarball pkg
      case mTarball of
        Left err ->
          errNotFound "Could not serve package contents" [MText err]
        Right (fp, etag, index) ->
          serveTarball (display (packageId pkg) ++ " source tarball")
                       ["index.html"] (display (packageId pkg)) fp index
                       [Public, maxAgeDays 30] etag
