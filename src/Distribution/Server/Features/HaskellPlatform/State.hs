{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.HaskellPlatform.State where

import Data.Acid (Query, Update, makeAcidic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import Distribution.Version

import Control.Monad.Reader (ask, asks)
import Control.Monad.State (put, modify)

newtype PlatformPackages = PlatformPackages {
    blessedPackages :: Map PackageName (Set Version)
} deriving (Show, Typeable, Eq, MemSize)

emptyPlatformPackages :: PlatformPackages
emptyPlatformPackages = PlatformPackages Map.empty

getPlatformPackages :: Query PlatformPackages PlatformPackages
getPlatformPackages = ask

getPlatformPackage :: PackageName -> Query PlatformPackages (Set Version)
getPlatformPackage pkgname = asks (Map.findWithDefault Set.empty pkgname . blessedPackages)

setPlatformPackage :: PackageName -> Set Version -> Update PlatformPackages ()
setPlatformPackage pkgname versions = modify $ \p -> case Set.null versions of
    True  -> p { blessedPackages = Map.delete pkgname $ blessedPackages p }
    False -> p { blessedPackages = Map.insert pkgname versions $ blessedPackages p }

replacePlatformPackages :: PlatformPackages -> Update PlatformPackages ()
replacePlatformPackages = put

$(deriveSafeCopy 0 'base ''PlatformPackages)

initialPlatformPackages :: PlatformPackages
initialPlatformPackages = emptyPlatformPackages

makeAcidic ''PlatformPackages ['getPlatformPackages
                              ,'getPlatformPackage
                              ,'setPlatformPackage
                              ,'replacePlatformPackages
                              ]

