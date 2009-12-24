{-# LANGUAGE
    RecordWildCards
  #-}

module Distribution.Server.Distributions.Distributions
    ( DistroName
    , Distributions
    , emptyDistributions
    , addDistro
    , removeDistro
    , enumerate
    , isDistribution
    , DistroVersions
    , emptyDistroVersions
    , DistroPackageInfo(..)
    , addPackage
    , dropPackage
    , removeDistroVersions
    , distroStatus
    , packageStatus
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import Distribution.Server.Distributions.Types

import Distribution.Package

import Data.List (foldl')
import Data.Maybe (fromJust)

emptyDistributions :: Distributions
emptyDistributions = Distributions Set.empty

emptyDistroVersions :: DistroVersions
emptyDistroVersions = DistroVersions Map.empty Map.empty


--- Distribution updating

isDistribution :: DistroName -> Distributions -> Bool
isDistribution distro d@Distributions{..}
    = Set.member distro name_map

-- | Add a distribution. Returns 'Nothing' if the
-- name is already in use.
addDistro :: DistroName -> Distributions -> Maybe Distributions
addDistro name d@Distributions{..}
    | isDistribution name d = Nothing
    | otherwise = Just . Distributions $ Set.insert name name_map


-- | List all known distributions
enumerate :: Distributions -> [DistroName]
enumerate Distributions{..}
    = Set.toList name_map

--- Queries

-- | For a particular distribution, which packages do they have, and
-- at which version.
distroStatus :: DistroName -> DistroVersions -> [(PackageName, DistroPackageInfo)]
distroStatus  distro DistroVersions{..}
    = let packageNames = maybe [] Set.toList (Map.lookup distro distro_map)
          f packageName = let infoMap = fromJust $ Map.lookup packageName package_map
                              info = fromJust $ Map.lookup distro infoMap
                          in (packageName, info)
      in map f packageNames

-- | For a particular package, which distributions contain it and at which
-- version.
packageStatus :: PackageName -> DistroVersions -> [(DistroName, DistroPackageInfo)]
packageStatus packageName DistroVersions{..}
    = maybe [] Map.toList (Map.lookup packageName package_map)

--- Removing

-- | Remove a distirbution from the list of known distirbutions
removeDistro :: DistroName -> Distributions -> Distributions
removeDistro distro distros@Distributions{..}
    = distros
      { name_map = Set.delete distro name_map
      }

-- | Drop all packages for a distribution.
removeDistroVersions :: DistroName -> DistroVersions -> DistroVersions
removeDistroVersions distro dv@DistroVersions{..}
    = let packageNames = maybe [] Set.toList (Map.lookup distro distro_map)
      in foldl' (flip $ dropPackage distro) dv packageNames

--- Updating

-- | Flag a package as no longer being distributed
dropPackage :: DistroName -> PackageName -> DistroVersions -> DistroVersions
dropPackage distro packageName dv@DistroVersions{..}
    = dv
      { package_map = Map.update pUpdate packageName package_map
      , distro_map  = Map.update dUpdate distro distro_map
      }
 where pUpdate infoMap = 
           case Map.delete distro infoMap of
             infoMap'
                 -> if Map.null infoMap'
                    then Nothing
                    else Just infoMap'

       dUpdate packageNames =
           case Set.delete packageName packageNames of
             packageNames'
                 -> if Set.null packageNames'
                    then Nothing
                    else Just packageNames'

-- | Add a package for a distribution. If the distribution already
-- had information for the specified package, that information is replaced.
addPackage :: DistroName -> PackageName -> DistroPackageInfo
           -> DistroVersions -> DistroVersions
addPackage distro packageName info dv@DistroVersions{..}
    = dv
      { package_map = Map.insertWith'
                      (const $ Map.insert distro info)
                      packageName
                      (Map.singleton distro info)
                      package_map

      , distro_map  = Map.insertWith  -- should be insertWith'?
                      (const $ Set.insert packageName)
                      distro
                      (Set.singleton packageName)
                      distro_map
      }
