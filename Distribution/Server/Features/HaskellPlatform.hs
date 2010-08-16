module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature(..),
    initPlatformFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Data.List (nub, sort)

-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--
-- TODO: finish
data PlatformFeature = PlatformFeature {
    platformResource :: PlatformResource
}

data PlatformResource = PlatformResource {
    platformListing :: Resource,
    platformPackage :: Resource,
    platformPackages :: Resource
}

instance HackageFeature PlatformFeature where
    getFeature platform = HackageModule
      { featureName = "platform"
      , resources   = map ($platformResource platform) []
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initPlatformFeature :: Config -> CoreFeature -> IO PlatformFeature
initPlatformFeature _ _ = do
    return PlatformFeature
      { PlatformResource = fix $ \r -> PlatformResource
          { platformListing = (resourceAt "/platform/.:format") { resourceGet = [] }
          , platformPackage = (resourceAt "/platform/package/:package.:format") { resourceGet = [], resourceDelete = [] }
          , platformPackages = (resourceAt "/platform/packages/.:format") { resourceGet = [], resourcePost = [] }
        -- and maybe "/platform/haskell-platform.cabal"
          }
      }

------------------------------------------
-- TODO: split these off into a Distribution.Server.Packages.* module
newtype PlatformPackages = PlatformPackages {
    blessedPackages :: Map PackageName (Set Version)
} deriving (Show, Typeable)

getPlatformPackages :: Query PlatformPackages PlatformPackages
getPlatformPackages = ask

setPlatformPackage :: PackageName -> Set Version -> Update PlatformPackages ()
setPlatformPackage pkgname versions = modify $ \p -> case versions of
    [] -> p { blessedPackages = Map.delete pkgname $ blessedPackages p }
    _  -> p { blessedPackages = Map.insert versions pkgname $ blessedPackages p }

instance Version PlatformPackages
$(deriveSerialize ''PlatformPackages)


