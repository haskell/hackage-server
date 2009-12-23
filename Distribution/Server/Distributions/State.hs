{-# LANGUAGE
    DeriveDataTypeable
  , TemplateHaskell
  , RecordWildCards
  , TypeFamilies
  , FlexibleInstances
  , MultiParamTypeClasses
  , FlexibleContexts
  #-}

module Distribution.Server.Distributions.State where

import Distribution.Package (PackageName)

import qualified Distribution.Server.Distributions.Distributions as Dist
import Distribution.Server.Distributions.Distributions
    (DistroName, DistroId, Distributions, DistroVersions, DistroPackageInfo)

import Data.Maybe (mapMaybe)
import Data.Typeable

import Happstack.State
import Happstack.State.ComponentSystem
import Happstack.Data.Serialize

import qualified Data.Binary as Binary

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (asks)

data Distros
    = Distros
      { dist_distros  :: !Distributions
      , dist_versions :: !DistroVersions
      }
 deriving Typeable

instance Version Distros
$(deriveSerialize ''Distros)

instance Component Distros where
    type Dependencies Distros = End
    initialValue = Distros Dist.emptyDistributions Dist.emptyDistroVersions

addDistro :: DistroName -> Update Distros (Maybe DistroId)
addDistro name
    = do
  state <- get
  let distros = dist_distros state
  case Dist.addDistro name distros of
    Nothing -> return Nothing
    Just (did, distros')
        -> put state{dist_distros = distros'} >> return (Just did)

-- DELETES a distribution. The name may then be re-used.
-- You should also clean up the permissions DB as well.
removeDistro :: DistroId -> Update Distros ()
removeDistro distro
    = modify $ \state@Distros{..} ->
      state{ dist_distros  = Dist.removeDistro distro dist_distros
           , dist_versions = Dist.removeDistroVersions distro dist_versions
           }

lookupDistroName :: DistroId -> Query Distros (Maybe DistroName)
lookupDistroName distro = asks $ Dist.lookupDistroName distro . dist_distros

lookupDistroId :: DistroName -> Query Distros (Maybe DistroId)
lookupDistroId name = asks $ Dist.lookupDistroId name . dist_distros

enumerate :: Query Distros [(DistroId, DistroName)]
enumerate = asks $ Dist.enumerate . dist_distros

addPackage :: DistroId -> PackageName -> DistroPackageInfo -> Update Distros ()
addPackage distro package info
    = modify $ \state@Distros{..} ->
      state{ dist_versions = Dist.addPackage distro package info dist_versions }

dropPackage :: DistroId -> PackageName -> Update Distros ()
dropPackage distro package
    = modify $ \state@Distros{..} ->
      state{ dist_versions = Dist.dropPackage distro package dist_versions }

distroStatus :: DistroId -> Query Distros [(PackageName, DistroPackageInfo)]
distroStatus distro
    = asks $ Dist.distroStatus distro . dist_versions

packageStatusById :: PackageName -> Query Distros [(DistroId, DistroPackageInfo)]
packageStatusById package
    = asks $ Dist.packageStatus package . dist_versions

packageStatus :: PackageName -> Query Distros [(DistroName, DistroPackageInfo)]
packageStatus package
    = do
  distros <- asks dist_distros
  stats <- packageStatusById package
  return $ mapMaybe (onFstM $ flip Dist.lookupDistroName distros) stats
 where onFstM f (x,y)
           = f x >>= \x' -> return (x', y)
  

$(mkMethods
  ''Distros
  [ -- update collection of distributions
    'addDistro
  , 'removeDistro

  -- query collection of distributions
  , 'lookupDistroName
  , 'lookupDistroId
  , 'enumerate

  -- update package versions in distros
  , 'addPackage
  , 'dropPackage

  -- query status of package versions
  , 'distroStatus
  , 'packageStatus
  ]
 )
