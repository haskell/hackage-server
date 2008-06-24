{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.PackagesState where

import Distribution.Package (PackageIdentifier)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Hackage.Types (PkgInfo)

import HAppS.State
import HAppS.Data.Serialize
import Data.Binary

import Data.Typeable
import Control.Monad.Reader
import Data.Monoid

newtype PackagesState = PackagesState (PackageIndex.PackageIndex PkgInfo)
  deriving (Typeable, Read, Show)

instance Version PackagesState where
  mode = Versioned 0 Nothing --change to Just [...] for previous versions

instance Serialize PackagesState where
  putCopy = contain . put . show
  getCopy = contain $ fmap read get

instance Version PackageIdentifier where
  mode = Versioned 0 Nothing

instance Serialize PackageIdentifier where
  putCopy = contain . put . show
  getCopy = contain $ fmap read get

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy = contain . put . show
  getCopy = contain $ fmap read get

--insert

lookupPackageId :: PackageIdentifier -> Query PackagesState (Maybe PkgInfo)
lookupPackageId pkgid = do
  PackagesState index <- ask
  return (PackageIndex.lookupPackageId index pkgid)

$(mkMethods ''PackagesState ['lookupPackageId])

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState mempty

