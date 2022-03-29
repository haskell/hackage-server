{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RecordWildCards #-}

module Distribution.Server.Features.Distro.State where

import Distribution.Package (PackageName)

import qualified Distribution.Server.Features.Distro.Distributions as Dist
import Distribution.Server.Features.Distro.Distributions
    (DistroName, Distributions, DistroVersions, DistroPackageInfo)

import Distribution.Server.Users.Group (UserIdSet)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.State ()
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable

import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask, asks)

data Distros = Distros {
    distDistros  :: !Distributions,
    distVersions :: !DistroVersions
}
 deriving (Eq, Typeable, Show)

deriveSafeCopy 0 'base ''Distros

instance MemSize Distros where
    memSize (Distros a b) = memSize2 a b

initialDistros :: Distros
initialDistros = Distros Dist.emptyDistributions Dist.emptyDistroVersions

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

enumerateDistros :: Query Distros [DistroName]
enumerateDistros = asks $ Dist.enumerate . distDistros

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

putDistroPackageList :: DistroName -> [(PackageName, DistroPackageInfo)] -> Update Distros ()
putDistroPackageList distro list
    = modify $ \state->
      state{ distVersions = Dist.updatePackageList distro list $ distVersions state }

packageStatus :: PackageName -> Query Distros [(DistroName, DistroPackageInfo)]
packageStatus package
    = asks $ Dist.packageStatus package . distVersions

distroPackageStatus :: DistroName -> PackageName -> Query Distros (Maybe DistroPackageInfo)
distroPackageStatus distro package = asks $ Dist.distroPackageStatus distro package . distVersions

getDistroMaintainers :: DistroName -> Query Distros UserIdSet
getDistroMaintainers name = liftM (fromMaybe Group.empty . Dist.getDistroMaintainers name) (asks distDistros)

modifyDistroMaintainers :: DistroName -> (UserIdSet -> UserIdSet) -> Update Distros ()
modifyDistroMaintainers name func = modify (\distros -> distros {distDistros = Dist.modifyDistroMaintainers name func (distDistros distros) })

addDistroMaintainer :: DistroName -> UserId -> Update Distros ()
addDistroMaintainer name uid = modifyDistroMaintainers name (Group.insert uid)

removeDistroMaintainer :: DistroName -> UserId -> Update Distros ()
removeDistroMaintainer name uid = modifyDistroMaintainers name (Group.delete uid)

replaceDistroMaintainers :: DistroName -> UserIdSet -> Update Distros ()
replaceDistroMaintainers name ulist = modifyDistroMaintainers name (const ulist)

makeAcidic
  ''Distros
  [ -- update collection of distributions
    'addDistro
  , 'removeDistro

  -- query collection of distributions
  , 'enumerateDistros
  , 'isDistribution

  -- update package versions in distros
  , 'addPackage
  , 'dropPackage

  -- query status of package versions
  , 'distroStatus
  , 'packageStatus
  , 'distroPackageStatus

  -- bulk update
  , 'putDistroPackageList

  -- import/export
  , 'getDistributions
  , 'replaceDistributions

  -- distro maintainers
  , 'getDistroMaintainers
  , 'replaceDistroMaintainers
  , 'addDistroMaintainer
  , 'removeDistroMaintainer
  ]

