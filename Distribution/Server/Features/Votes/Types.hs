{-# LANGUAGE DeriveDataTypeable #-}

-- | Defines the primary data types and functions that hold\/alter
-- | state relating to package votes.
module Distribution.Server.Features.Votes.Types
  ( Votes(..)
  , VotesMap
  , initialVotes
  , addVote
  , removeVote
  , getUsersWhoVoted
  , getNumberOfVotesFor
  , askUserVoted
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


type VotesMap = Map PackageName UserIdSet

newtype Votes = Votes { votesMap :: VotesMap }
  deriving (Show, Eq, Typeable)

instance MemSize Votes where
    memSize (Votes a) = memSize1 a


initialVotes :: Votes
initialVotes = Votes Map.empty

addVote :: PackageName -> UserId -> Votes -> Votes
addVote pkgname uid (Votes votes) =
    Votes $ Map.alter f pkgname votes
  where
    f k = Just $ UserIdSet.insert uid $ case k of
      Just key -> key
      Nothing  -> UserIdSet.empty

removeVote :: PackageName -> UserId -> Votes -> Votes
removeVote pkgname uid (Votes votes) =
    Votes $ Map.adjust (UserIdSet.delete uid) pkgname votes

getUsersWhoVoted :: PackageName -> Votes -> UserIdSet
getUsersWhoVoted pkgname (Votes votes) =
    Map.findWithDefault UserIdSet.empty pkgname votes

-- Find out if a particular user voted for a package
askUserVoted :: PackageName -> UserId -> Votes -> Bool
askUserVoted  pkgname uid votes =
    UserIdSet.member uid $ getUsersWhoVoted pkgname votes

getNumberOfVotesFor :: PackageName -> Votes -> Int
getNumberOfVotesFor pkgname votes =
    UserIdSet.size $ getUsersWhoVoted pkgname votes

enumerate :: Votes -> [(PackageName, UserIdSet)]
enumerate (Votes votes) = Map.toList votes

