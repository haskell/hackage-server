module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature(..),
    initPlatformFeature
  ) where

import Distribution.Server.Feature

-- some boilerplate-y code follows
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
newtype PlatformPackages = PlatformPackages {
    blessedPackages :: Map PackageName [Version]
} deriving (Show, Typeable)

getPlatformPackages :: Query PlatformPackages PlatformPackages
getPlatformPackages = ask

setPlatformPackage :: PackageName -> [Version] -> Update PlatformPackages ()
setPlatformPackage pkgname versions = modify $ \p -> case versions of
    [] -> p { blessedPackages = Map.delete pkgname $ blessedPackages p }
    _  -> p { blessedPackages = Map.insert version pkgname $ blessedPackages p }


