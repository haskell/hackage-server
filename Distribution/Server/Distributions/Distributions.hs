{-# LANGUAGE
    RecordWildCards
  #-}

module Distribution.Server.Distributions.Distributions
    ( DistroName
    , DistroId
    , Distributions
    , emptyDistributions
    , addDistro
    , insertDistro
    , removeDistro
    , lookupDistroName
    , lookupDistroId
    , enumerate
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
emptyDistributions = Distributions IntMap.empty Map.empty 0


emptyDistroVersions :: DistroVersions
emptyDistroVersions = DistroVersions Map.empty IntMap.empty


--- Distribution updating

-- | Add a distribution, generate a fresh id. Returns 'Nothing' if the
-- name is already in use.
addDistro :: DistroName -> Distributions -> Maybe (DistroId, Distributions)
addDistro name d@Distributions{..}
    = case lookupDistroId name d of
        Just{} -> Nothing
        Nothing ->
            let newDistro = DistroId next_id
            in Just
                   (newDistro
                   , d
                    { name_map = IntMap.insert next_id name name_map
                    , id_map   = Map.insert name newDistro id_map
                    , next_id = next_id + 1
                    }
                   )

-- | Add a dsitribution with the specified id and name. Returns 'Nothing'
-- if either the id or name are in use.
insertDistro :: DistroId -> DistroName -> Distributions -> Maybe Distributions
insertDistro dId@(DistroId distro) name d@Distributions{..}
    = case (lookupDistroId name d, lookupDistroName dId d) of
        (Nothing, Nothing) ->
            Just d{ name_map = IntMap.insert distro name name_map
                  , id_map   = Map.insert name dId id_map
                  }
        _ -> Nothing

-- | Lookup the id of a distribution from its name.
lookupDistroId :: DistroName -> Distributions -> Maybe DistroId
lookupDistroId name Distributions{..}
    = Map.lookup name id_map

-- | Lookup the name of a distribution from its id.
lookupDistroName :: DistroId -> Distributions -> Maybe DistroName
lookupDistroName (DistroId distro) Distributions{..}
    = IntMap.lookup distro name_map

-- | List all known distributions
enumerate :: Distributions -> [(DistroId, DistroName)]
enumerate Distributions{..}
    = map (onFst DistroId) $ IntMap.toList name_map
 where onFst f (x,y) = (f x, y)

--- Queries

-- | For a particular distribution, which packages do they have, and
-- at which version.
distroStatus :: DistroId -> DistroVersions -> [(PackageName, DistroPackageInfo)]
distroStatus  (DistroId distro) DistroVersions{..}
    = let packageNames = maybe [] Set.toList (IntMap.lookup distro distro_map)
          f packageName = let infoMap = fromJust $ Map.lookup packageName package_map
                              info = fromJust $ IntMap.lookup distro infoMap
                          in (packageName, info)
      in map f packageNames

-- | For a particular package, which distributions contain it and at which
-- version.
packageStatus :: PackageName -> DistroVersions -> [(DistroId, DistroPackageInfo)]
packageStatus packageName DistroVersions{..}
    = map (onFst DistroId) $
      maybe [] IntMap.toList (Map.lookup packageName package_map)
 where onFst f (x,y) = (f x, y)


--- Removing

-- | Remove a distirbution from the list of known distirbutions
removeDistro :: DistroId -> Distributions -> Distributions
removeDistro dId@(DistroId distro) distros@Distributions{..}
    = case lookupDistroName dId distros of
        Nothing -> distros
        Just distroName ->
            distros
            { name_map = IntMap.delete distro name_map
            , id_map   = Map.delete distroName id_map
            }

-- | Drop all packages for a distribution.
removeDistroVersions :: DistroId -> DistroVersions -> DistroVersions
removeDistroVersions dId@(DistroId distro) dv@DistroVersions{..}
    = let packageNames = maybe [] Set.toList (IntMap.lookup distro distro_map)
      in foldl' (flip $ dropPackage dId) dv packageNames

--- Updating

-- | Flag a package as no longer being distributed
dropPackage :: DistroId -> PackageName -> DistroVersions -> DistroVersions
dropPackage (DistroId distro) packageName dv@DistroVersions{..}
    = dv
      { package_map = Map.update pUpdate packageName package_map
      , distro_map  = IntMap.update dUpdate distro distro_map
      }
 where pUpdate infoMap = 
           case IntMap.delete distro infoMap of
             infoMap'
                 -> if IntMap.null infoMap'
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
addPackage :: DistroId -> PackageName -> DistroPackageInfo
           -> DistroVersions -> DistroVersions
addPackage did@(DistroId distro) packageName info dv@DistroVersions{..}
    = dv
      { package_map = Map.insertWith'
                      (const $ IntMap.insert distro info)
                      packageName
                      (IntMap.singleton distro info)
                      package_map

      , distro_map  = IntMap.insertWith  -- should be insertWith'?
                      (const $ Set.insert packageName)
                      distro
                      (Set.singleton packageName)
                      distro_map
      }
