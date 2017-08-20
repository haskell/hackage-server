{-# LANGUAGE
    RecordWildCards
  #-}

module Distribution.Server.Features.Distro.Distributions
    ( DistroName(..)
    , Distributions(..)
    , emptyDistributions
    , addDistro
    , removeDistro
    , updatePackageList
    , enumerate
    , isDistribution
    , DistroVersions(..)
    , emptyDistroVersions
    , DistroPackageInfo(..)
    , addPackage
    , dropPackage
    , removeDistroVersions
    , distroStatus
    , packageStatus
    , distroPackageStatus
    , getDistroMaintainers
    , modifyDistroMaintainers
    ) where

import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import qualified Data.Set as Set

import Distribution.Server.Features.Distro.Types
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserIdSet)

import Distribution.Package

import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)

emptyDistributions :: Distributions
emptyDistributions = Distributions Map.empty

emptyDistroVersions :: DistroVersions
emptyDistroVersions = DistroVersions Map.empty Map.empty

--- Distribution updating
isDistribution :: DistroName -> Distributions -> Bool
isDistribution distro distros
    = Map.member distro (nameMap distros)

-- | Add a distribution. Returns 'Nothing' if the
-- name is already in use.
addDistro :: DistroName -> Distributions -> Maybe Distributions
addDistro name distros
    | isDistribution name distros = Nothing
    | otherwise = Just . Distributions $ Map.insert name Group.empty (nameMap distros)


-- | List all known distributions
enumerate :: Distributions -> [DistroName]
enumerate distros = Map.keys (nameMap distros)

--- Queries

-- | For a particular distribution, which packages do they have, and
-- at which version. This function isn't very total.
distroStatus :: DistroName -> DistroVersions -> [(PackageName, DistroPackageInfo)]
distroStatus distro distros
    = let packageNames = maybe [] Set.toList (Map.lookup distro $ distroMap distros)
          f package = let infoMap = fromJust $ Map.lookup package (packageDistroMap distros)
                          info = fromJust $ Map.lookup distro infoMap
                      in (package, info)
      in map f packageNames

-- | For a particular package, which distributions contain it and at which
-- version.
packageStatus :: PackageName -> DistroVersions -> [(DistroName, DistroPackageInfo)]
packageStatus package dv = maybe [] Map.toList (Map.lookup package $ packageDistroMap dv)

distroPackageStatus :: DistroName -> PackageName -> DistroVersions -> Maybe DistroPackageInfo
distroPackageStatus distro package dv = Map.lookup distro =<< Map.lookup package (packageDistroMap dv)

--- Removing

-- | Remove a distirbution from the list of known distirbutions
removeDistro :: DistroName -> Distributions -> Distributions
removeDistro distro distros = distros { nameMap = Map.delete distro (nameMap distros) }

-- | Drop all packages for a distribution.
removeDistroVersions :: DistroName -> DistroVersions -> DistroVersions
removeDistroVersions distro dv
    = let packageNames = maybe [] Set.toList (Map.lookup distro $ distroMap dv)
      in foldl' (flip $ dropPackage distro) dv packageNames

--- Updating

-- | Bulk update of all information for one specific distribution
updatePackageList :: DistroName -> [(PackageName, DistroPackageInfo)] -> DistroVersions -> DistroVersions
updatePackageList distro list dv = foldr (\(pn,dpi) -> addPackage distro pn dpi) (removeDistroVersions distro dv) list

-- | Flag a package as no longer being distributed
dropPackage :: DistroName -> PackageName -> DistroVersions -> DistroVersions
dropPackage distro package dv@DistroVersions{..}
    = dv
      { packageDistroMap = Map.update pUpdate package packageDistroMap
      , distroMap  = Map.update dUpdate distro distroMap
      }
 where pUpdate infoMap =
           case Map.delete distro infoMap of
             infoMap'
                 -> if Map.null infoMap'
                    then Nothing
                    else Just infoMap'

       dUpdate packageNames =
           case Set.delete package packageNames of
             packageNames'
                 -> if Set.null packageNames'
                    then Nothing
                    else Just packageNames'

-- | Add a package for a distribution. If the distribution already
-- had information for the specified package, that information is replaced.
addPackage :: DistroName -> PackageName -> DistroPackageInfo
           -> DistroVersions -> DistroVersions
addPackage distro package info dv@DistroVersions{..}
    = dv
      { packageDistroMap = Map.Strict.insertWith
                      (const $ Map.insert distro info)
                      package
                      (Map.singleton distro info)
                      packageDistroMap

      , distroMap  = Map.insertWith  -- should be insertWith'?
                      (const $ Set.insert package)
                      distro
                      (Set.singleton package)
                      distroMap
      }

getDistroMaintainers :: DistroName -> Distributions -> Maybe UserIdSet
getDistroMaintainers name = Map.lookup name . nameMap

modifyDistroMaintainers :: DistroName -> (UserIdSet -> UserIdSet) -> Distributions -> Distributions
modifyDistroMaintainers name func dists = dists {nameMap = Map.alter (Just . func . fromMaybe Group.empty) name (nameMap dists) }

