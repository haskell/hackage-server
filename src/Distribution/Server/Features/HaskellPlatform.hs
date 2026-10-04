{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature,
    PlatformResource(..),
    initPlatformFeature,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.HaskellPlatform.Acid (acidStore)
import qualified Distribution.Server.Features.HaskellPlatform.Store as Store

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function


-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--

data PlatformFeature = PlatformFeature {
    platformFeatureInterface :: HackageFeature,

    platformResource :: PlatformResource,

    platformVersions      :: forall m. MonadIO m => PackageName -> m [Version],
    platformPackageLatest :: forall m. MonadIO m => m [(PackageName, Version)],
    setPlatform           :: forall m. MonadIO m => PackageName -> [Version] -> m (),
    removePlatform        :: forall m. MonadIO m => PackageName -> m ()
}

instance IsHackageFeature PlatformFeature where
    getFeatureInterface = platformFeatureInterface

data PlatformResource = PlatformResource {
    platformPackage :: Resource,
    platformPackages :: Resource,
    platformPackageUri :: String -> PackageName -> String,
    platformPackagesUri :: String -> String
}

initPlatformFeature :: ServerEnv -> IO (IO PlatformFeature)
initPlatformFeature ServerEnv{serverStateDir} = do
    platformState <- acidStore serverStateDir

    return $ do
      let feature = platformFeature platformState
      return feature

platformFeature :: Store.Backend
                -> PlatformFeature
platformFeature Store.Backend{..}
  = PlatformFeature{..}
  where
    platformFeatureInterface = (emptyHackageFeature "platform") {
        featureDesc = "List packages which are part of the Haskell platform (this is work in progress)"
      , featureResources =
          map ($ platformResource) [
              platformPackage
            , platformPackages
            ]
      , featureState = backendState
      }

    platformResource = fix $ \r -> PlatformResource
      { platformPackage = (resourceAt "/platform/package/:package.:format") {
            resourceGet    = []
          , resourceDelete = []
          , resourcePut    = []
          }
      , platformPackages = (resourceAt "/platform/.:format") {
            resourceGet  = []
          , resourcePost = []
          }
      , platformPackageUri = \format pkgid ->
          renderResource (platformPackage r) [display pkgid, format]
      , platformPackagesUri = \format ->
          renderResource (platformPackages r) [format]
       -- and maybe "/platform/haskell-platform.cabal"
      }

    ------------------------------------------
    -- functionality: showing status for a single package, and for all packages, adding a package, deleting a package
    platformVersions :: MonadIO m => PackageName -> m [Version]
    platformVersions = Store.platformVersions backendStore

    platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
    platformPackageLatest = Store.platformPackageLatest backendStore

    setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
    setPlatform = Store.setPlatform backendStore

    removePlatform :: MonadIO m => PackageName -> m ()
    removePlatform = Store.removePlatform backendStore
