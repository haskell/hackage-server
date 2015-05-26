{-# LANGUAGE DeriveDataTypeable #-}

-- | Defines the primary data types and functions that hold/alter
-- | state relating to package ranking.
module Distribution.Server.Features.Ranking.Types
  ( Stars(..)
  , StarMap
  , initialStars
  , addStar
  , removeStar
  , getUsersWhoStarred
  , getNumberOfStarsFor
  , askUserStarred
  , enumerate
  ) where

import Distribution.Package (PackageName(..))
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Framework.MemSize

import Data.Typeable
import Data.Map as Map
import Data.Set as Set
import Data.List as L

type StarMap = Map PackageName (Set UserId)

data Stars = Stars {
  extractMap:: !StarMap
  } deriving (Show, Eq, Typeable)

instance MemSize Stars where
    memSize (Stars a) = memSize1 a


initialStars :: Stars
initialStars = Stars Map.empty

addStar :: PackageName -> UserId -> Stars -> Stars
addStar pkgname uid stars = Stars $
  adjust (Set.insert uid) pkgname somemap
    where
      smap  = extractMap stars
      somemap = if pkgname `Map.member` smap
        then smap
        else Map.insert pkgname Set.empty smap

removeStar :: PackageName -> UserId -> Stars -> Stars
removeStar pkgname uid vmap = Stars $
  adjust (Set.delete uid) pkgname (extractMap vmap)

getUsersWhoStarred :: PackageName -> Stars -> Set UserId
getUsersWhoStarred pkgname stars =
  case pkgname `Map.member` smap of
    True -> smap Map.! pkgname
    False -> Set.empty
  where
    smap    = extractMap stars

-- Find out if a particular user starred a package
askUserStarred :: PackageName -> UserId -> Stars -> Bool
askUserStarred  pkgname uid stars =
  case pkgname `Map.member` smap of
    True -> uid `Set.member` (smap Map.! pkgname)
    False -> False
  where
    smap = extractMap stars

getNumberOfStarsFor :: PackageName -> Stars -> Int
getNumberOfStarsFor pkgname vmap =
  Set.size (getUsersWhoStarred pkgname vmap)

enumerate :: Stars -> [(String, Set UserId)]
enumerate vmap = L.map (\(name, uids) -> (unPackageName name, uids)) $ Map.toList (extractMap vmap)
