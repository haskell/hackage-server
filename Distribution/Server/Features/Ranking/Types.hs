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
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Framework.MemSize

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map


type StarMap = Map PackageName UserIdSet

data Stars = Stars {
  extractMap:: !StarMap
  } deriving (Show, Eq, Typeable)

instance MemSize Stars where
    memSize (Stars a) = memSize1 a


initialStars :: Stars
initialStars = Stars Map.empty

addStar :: PackageName -> UserId -> Stars -> Stars
addStar pkgname uid stars = Stars $
  Map.alter f pkgname (extractMap stars)
    where
      f k = Just $ UserIdSet.insert uid $ case k of
        Just key -> key
        Nothing  -> UserIdSet.empty

removeStar :: PackageName -> UserId -> Stars -> Stars
removeStar pkgname uid vmap = Stars $
  Map.adjust (UserIdSet.delete uid) pkgname (extractMap vmap)

getUsersWhoStarred :: PackageName -> Stars -> UserIdSet
getUsersWhoStarred pkgname stars =
  Map.findWithDefault UserIdSet.empty pkgname $ extractMap stars

-- Find out if a particular user starred a package
askUserStarred :: PackageName -> UserId -> Stars -> Bool
askUserStarred  pkgname uid stars =
  UserIdSet.member uid $ getUsersWhoStarred pkgname stars

getNumberOfStarsFor :: PackageName -> Stars -> Int
getNumberOfStarsFor pkgname vmap =
  UserIdSet.size $ getUsersWhoStarred pkgname vmap

enumerate :: Stars -> [(String, UserIdSet)]
enumerate vmap = map
  (\(name, uids) -> (unPackageName name, uids)) $
    Map.toList (extractMap vmap)
