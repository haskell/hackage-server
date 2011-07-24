module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature(..),
    PlatformResource(..),
    initPlatformFeature,
    platformVersions,
    platformPackageLatest,
    setPlatform,
    removePlatform
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Platform
import Data.Function

import Distribution.Package
import Distribution.Version
import Distribution.Text

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO)

-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--

data PlatformFeature = PlatformFeature {
    platformResource :: PlatformResource
}

data PlatformResource = PlatformResource {
    platformPackage :: Resource,
    platformPackages :: Resource,
    platformPackageUri :: String -> PackageName -> String,
    platformPackagesUri :: String -> String
}

instance HackageFeature PlatformFeature where
    getFeature platform = HackageModule
      { featureName = "platform"
      , resources   = map ($platformResource platform) [platformPackage, platformPackages]
      , dumpBackup    = Nothing -- TODO
      , restoreBackup = Nothing
      }

initPlatformFeature :: Config -> CoreFeature -> IO PlatformFeature
initPlatformFeature _ _ = do
    return PlatformFeature
      { platformResource = fix $ \r -> PlatformResource
          { platformPackage  = (resourceAt "/platform/package/:package.:format") { resourceGet = [], resourceDelete = [], resourcePut = [] }
          , platformPackages = (resourceAt "/platform/.:format") { resourceGet = [], resourcePost = [] }
          , platformPackageUri = \format pkgid -> renderResource (platformPackage r) [display pkgid, format]
          , platformPackagesUri = \format -> renderResource (platformPackages r) [format]
        -- and maybe "/platform/haskell-platform.cabal"
          }
      }

------------------------------------------
-- functionality: showing status for a single package, and for all packages, adding a package, deleting a package
platformVersions :: MonadIO m => PackageName -> m [Version]
platformVersions pkgname = liftM Set.toList $ query $ GetPlatformPackage pkgname

platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
platformPackageLatest = liftM (Map.toList . Map.map Set.findMax . blessedPackages) $ query $ GetPlatformPackages

setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
setPlatform pkgname versions = update $ SetPlatformPackage pkgname (Set.fromList versions)

removePlatform :: MonadIO m => PackageName -> m ()
removePlatform pkgname = update $ SetPlatformPackage pkgname Set.empty

