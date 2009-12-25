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
    (DistroName, Distributions, DistroVersions, DistroPackageInfo)

import Data.Maybe (mapMaybe)
import Data.Typeable

import Happstack.State
import Happstack.State.ComponentSystem
import Happstack.Data.Serialize

import qualified Data.Binary as Binary

import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask, asks)

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

addDistro :: DistroName -> Update Distros Bool
addDistro name
    = do
  state <- get
  let distros = dist_distros state
  case Dist.addDistro name distros of
    Nothing -> return False
    Just distros'
        -> put state{dist_distros = distros'} >> return True

-- DELETES a distribution. The name may then be re-used.
-- You should also clean up the permissions DB as well.
removeDistro :: DistroName -> Update Distros ()
removeDistro distro
    = modify $ \state@Distros{..} ->
      state{ dist_distros  = Dist.removeDistro distro dist_distros
           , dist_versions = Dist.removeDistroVersions distro dist_versions
           }

enumerate :: Query Distros [DistroName]
enumerate = asks $ Dist.enumerate . dist_distros

isDistribution :: DistroName -> Query Distros Bool
isDistribution distro
    = asks $ Dist.isDistribution distro . dist_distros

getDistributions :: Query Distros Distros
getDistributions = ask

replaceDistributions :: Distributions -> DistroVersions -> Update Distros ()
replaceDistributions distributions distroVersions
    = put $ Distros distributions distroVersions

addPackage :: DistroName -> PackageName -> DistroPackageInfo -> Update Distros ()
addPackage distro package info
    = modify $ \state@Distros{..} ->
      state{ dist_versions = Dist.addPackage distro package info dist_versions }

dropPackage :: DistroName -> PackageName -> Update Distros ()
dropPackage distro package
    = modify $ \state@Distros{..} ->
      state{ dist_versions = Dist.dropPackage distro package dist_versions }

distroStatus :: DistroName -> Query Distros [(PackageName, DistroPackageInfo)]
distroStatus distro
    = asks $ Dist.distroStatus distro . dist_versions

packageStatus :: PackageName -> Query Distros [(DistroName, DistroPackageInfo)]
packageStatus package
    = asks $ Dist.packageStatus package . dist_versions


$(mkMethods
  ''Distros
  [ -- update collection of distributions
    'addDistro
  , 'removeDistro

  -- query collection of distributions
  , 'enumerate
  , 'isDistribution

  -- update package versions in distros
  , 'addPackage
  , 'dropPackage

  -- query status of package versions
  , 'distroStatus
  , 'packageStatus

  -- import/export
  , 'getDistributions
  , 'replaceDistributions
  ]
 )
