{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature,
    PlatformResource(..),
    initPlatformFeature,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

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
    platformState <- platformStateComponent serverStateDir

    return $ do
      let feature = platformFeature platformState
      return feature

platformStateComponent :: FilePath -> IO (StateComponent AcidState PlatformPackages)
platformStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PlatformPackages") initialPlatformPackages
  return StateComponent {
      stateDesc    = "Platform packages"
    , stateHandle  = st
    , getState     = query st GetPlatformPackages
    , putState     = update st . ReplacePlatformPackages
    , resetState   = platformStateComponent
    -- TODO: backup
    -- For now backup is just empty, as this package is basically featureless
    -- It defines state, but there is no way at all to modify this state
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry for platform"
                       , restoreFinalize = return initialPlatformPackages
                       }
    }

platformFeature :: StateComponent AcidState PlatformPackages
                -> PlatformFeature
platformFeature platformState
  = PlatformFeature{..}
  where
    platformFeatureInterface = (emptyHackageFeature "platform") {
        featureDesc = "List packages which are part of the Haskell platform (this is work in progress)"
      , featureResources =
          map ($ platformResource) [
              platformPackage
            , platformPackages
            ]
      , featureState = [abstractAcidStateComponent platformState]
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
    platformVersions pkgname = liftM Set.toList $ queryState platformState $ GetPlatformPackage pkgname

    platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
    platformPackageLatest = liftM (Map.toList . Map.map Set.findMax . blessedPackages) $ queryState platformState $ GetPlatformPackages

    setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
    setPlatform pkgname versions = updateState platformState $ SetPlatformPackage pkgname (Set.fromList versions)

    removePlatform :: MonadIO m => PackageName -> m ()
    removePlatform pkgname = updateState platformState $ SetPlatformPackage pkgname Set.empty

