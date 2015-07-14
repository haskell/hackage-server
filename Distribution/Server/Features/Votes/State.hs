{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Votes.State where

import Distribution.Server.Framework.MemSize

import Distribution.Package (PackageName)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet

import Distribution.Server.Users.State ()

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask)


newtype VotesState = VotesState { votesMap :: Map PackageName UserIdSet }
  deriving (Show, Eq, Typeable, MemSize)

$(deriveSafeCopy 0 'base ''VotesState)

initialVotesState :: VotesState
initialVotesState = VotesState Map.empty

-- helper function
userVotedForPackage :: PackageName -> UserId -> Map PackageName UserIdSet -> Bool
userVotedForPackage pkgname uid votes =
    case Map.lookup pkgname votes of
      Nothing     -> False
      Just uidset -> UserIdSet.member uid uidset

-- All the acid state transactions

addVote :: PackageName -> UserId -> Update VotesState Bool
addVote pkgname uid = do
    VotesState votes <- State.get
    if userVotedForPackage pkgname uid votes
      then return False
      else do let votes' = Map.alter insert pkgname votes
                  insert = Just . UserIdSet.insert uid . fromMaybe UserIdSet.empty
              State.put $! VotesState votes'
              return True

removeVote :: PackageName -> UserId -> Update VotesState Bool
removeVote pkgname uid = do
    VotesState votes <- State.get
    if userVotedForPackage pkgname uid votes
       then do let votes' = Map.adjust (UserIdSet.delete uid) pkgname votes
               State.put $! VotesState votes'
               return True
       else return False

getPackageVoteCount :: PackageName -> Query VotesState Int
getPackageVoteCount pkgname = do
    VotesState votes <- ask
    case Map.lookup pkgname votes of
      Nothing     -> return 0
      Just uidset -> return $! UserIdSet.size uidset

getPackageUserVoted :: PackageName -> UserId -> Query VotesState Bool
getPackageUserVoted pkgname uid = do
    VotesState votes <- ask
    return $! userVotedForPackage pkgname uid votes

getAllPackageVoteSets :: Query VotesState (Map PackageName UserIdSet)
getAllPackageVoteSets = do
    VotesState votes <- ask
    return votes


-- get and replace the entire state, for backups

getVotesState :: Query VotesState VotesState
getVotesState = ask

replaceVotesState :: VotesState -> Update VotesState ()
replaceVotesState = State.put

makeAcidic
  ''VotesState
  [ 'addVote
  , 'removeVote
  , 'getPackageVoteCount
  , 'getPackageUserVoted
  , 'getAllPackageVoteSets
  , 'getVotesState
  , 'replaceVotesState
  ]
