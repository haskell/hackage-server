{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature,
    PlatformResource(..),
    initPlatformFeature,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.HaskellPlatform.State

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--

data PlatformFeature = PlatformFeature {
    platformFeatureInterface :: HackageFeature,

    platformResource :: PlatformResource,

    platformVersions      :: MonadIO m => PackageName -> m [Version],
    platformPackageLatest :: MonadIO m => m [(PackageName, Version)],
    setPlatform           :: MonadIO m => PackageName -> [Version] -> m (),
    removePlatform        :: MonadIO m => PackageName -> m ()
}

instance IsHackageFeature PlatformFeature where
    getFeatureInterface = platformFeatureInterface

data PlatformResource = PlatformResource {
    platformPackage :: Resource,
    platformPackages :: Resource,
    platformPackageUri :: String -> PackageName -> String,
    platformPackagesUri :: String -> String
}

initPlatformFeature :: ServerEnv -> IO PlatformFeature
initPlatformFeature ServerEnv{serverStateDir} = do
    platformState <- platformStateComponent serverStateDir
    return $ platformFeature platformState

platformStateComponent :: FilePath -> IO (StateComponent PlatformPackages)
platformStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PlatformPackages") initialPlatformPackages
  return StateComponent {
      stateDesc    = "Platform packages"
    , acidState    = st
    , getState     = query st GetPlatformPackages
    , resetState   = const platformStateComponent
    -- TODO: backup
    , backupState  = \_ -> []
    , restoreState = mempty
    }

platformFeature :: StateComponent PlatformPackages
                -> PlatformFeature
platformFeature platformState
  = PlatformFeature{..}
  where
    platformFeatureInterface = (emptyHackageFeature "platform") {
        featureResources =
          map ($platformResource) [
              platformPackage
            , platformPackages
            ]
      , featureState = [abstractStateComponent platformState]
      }

    platformResource = fix $ \r -> PlatformResource
          { platformPackage  = (resourceAt "/platform/package/:package.:format") { resourceGet = [], resourceDelete = [], resourcePut = [] }
          , platformPackages = (resourceAt "/platform/.:format") { resourceGet = [], resourcePost = [] }
          , platformPackageUri = \format pkgid -> renderResource (platformPackage r) [display pkgid, format]
          , platformPackagesUri = \format -> renderResource (platformPackages r) [format]
        -- and maybe "/platform/haskell-platform.cabal"
          }

    ------------------------------------------
    -- functionality: showing status for a single package, and for all packages, adding a package, deleting a package
    platformVersions :: MonadIO m => PackageName -> m [Version]
    platformVersions pkgname = liftM Set.toList $ queryState platformState $ GetPlatformPackage pkgname

    platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
    platformPackageLatest = liftM (Map.toList . Map.map Set.findMax . blessedPackages) $ queryState platformState $ GetPlatformPackages

    setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
    setPlatform pkgname versions = updateState platformState $ SetPlatformPackage pkgname (Set.fromList versions)

    removePlatform :: MonadIO m => PackageName -> m ()
    removePlatform pkgname = updateState platformState $ SetPlatformPackage pkgname Set.empty

