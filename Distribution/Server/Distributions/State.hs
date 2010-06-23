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

import Distribution.Server.Users.Group (UserList)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.State ()

import Data.Typeable

import Happstack.State

import Data.Maybe (fromMaybe)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask, asks)

data Distros = Distros {
    distDistros  :: !Distributions,
    distVersions :: !DistroVersions
}
 deriving (Typeable, Show)

instance Version Distros
$(deriveSerialize ''Distros)

instance Component Distros where
    type Dependencies Distros = End
    initialValue = Distros Dist.emptyDistributions Dist.emptyDistroVersions

addDistro :: DistroName -> Update Distros Bool
addDistro name = do
    state <- get
    let distros = distDistros state
    case Dist.addDistro name distros of
        Nothing -> return False
        Just distros' -> put state{distDistros = distros'} >> return True

-- DELETES a distribution. The name may then be re-used.
-- You should also clean up the permissions DB as well.
removeDistro :: DistroName -> Update Distros ()
removeDistro distro
    = modify $ \state@Distros{..} ->
      state { distDistros  = Dist.removeDistro distro distDistros
            , distVersions = Dist.removeDistroVersions distro distVersions
            }

enumerate :: Query Distros [DistroName]
enumerate = asks $ Dist.enumerate . distDistros

isDistribution :: DistroName -> Query Distros Bool
isDistribution distro = asks $ Dist.isDistribution distro . distDistros

getDistributions :: Query Distros Distros
getDistributions = ask

replaceDistributions :: Distributions -> DistroVersions -> Update Distros ()
replaceDistributions distributions distroVersions = put $ Distros distributions distroVersions

addPackage :: DistroName -> PackageName -> DistroPackageInfo -> Update Distros ()
addPackage distro package info
    = modify $ \state ->
      state{ distVersions = Dist.addPackage distro package info $ distVersions state }

dropPackage :: DistroName -> PackageName -> Update Distros ()
dropPackage distro package
    = modify $ \state ->
      state{ distVersions = Dist.dropPackage distro package $ distVersions state }

distroStatus :: DistroName -> Query Distros [(PackageName, DistroPackageInfo)]
distroStatus distro
    = asks $ Dist.distroStatus distro . distVersions

packageStatus :: PackageName -> Query Distros [(DistroName, DistroPackageInfo)]
packageStatus package
    = asks $ Dist.packageStatus package . distVersions

distroPackageStatus :: DistroName -> PackageName -> Query Distros (Maybe DistroPackageInfo)
distroPackageStatus distro package = asks $ Dist.distroPackageStatus distro package . distVersions

getDistroMaintainers :: DistroName -> Query Distros UserList
getDistroMaintainers name = fmap (fromMaybe Group.empty . Dist.getDistroMaintainers name) (asks distDistros)

modifyDistroMaintainers :: DistroName -> (UserList -> UserList) -> Update Distros ()
modifyDistroMaintainers name func = modify (\distros -> distros {distDistros = Dist.modifyDistroMaintainers name func (distDistros distros) })

addDistroMaintainer :: DistroName -> UserId -> Update Distros ()
addDistroMaintainer name uid = modifyDistroMaintainers name (Group.add uid)

removeDistroMaintainer :: DistroName -> UserId -> Update Distros ()
removeDistroMaintainer name uid = modifyDistroMaintainers name (Group.remove uid)

replaceDistroMaintainers :: DistroName -> UserList -> Update Distros ()
replaceDistroMaintainers name ulist = modifyDistroMaintainers name (const ulist)

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
  , 'distroPackageStatus

  -- import/export
  , 'getDistributions
  , 'replaceDistributions

  -- distro maintainers
  , 'getDistroMaintainers
  , 'replaceDistroMaintainers
  , 'addDistroMaintainer
  , 'removeDistroMaintainer
  ]
 )
