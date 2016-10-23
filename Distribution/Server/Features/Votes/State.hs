{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Votes.State where

import Distribution.Server.Framework.MemSize

import Distribution.Package (PackageName)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.State ()

import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromMaybe)
import Control.Arrow ((&&&))
import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask)

type Score = Int
newtype VotesState = VotesState (Map PackageName  (Map UserId Score))
  deriving (Show, Eq, Typeable, MemSize)

$(deriveSafeCopy 0 'base ''VotesState)

initialVotesState :: VotesState
initialVotesState = VotesState Map.empty

-- helper function
userVotedForPackage :: PackageName -> UserId -> Map PackageName (Map UserId Score) -> Bool
userVotedForPackage pkgname uid votes =
    case Map.lookup pkgname votes of
      Nothing     -> False
      Just m -> case Map.lookup uid m of
                  Nothing -> False
                  Just _ -> True

-- Using La Placian rule of succession to calculate scoring
votesScore :: Map UserId Score -> Float
votesScore m =
    let grouping = map (head &&& length) . group . sort . Map.elems $ m
        score =  fromIntegral (sum $ map (uncurry (*)) grouping) / fromIntegral (4 + foldr (\(_,b) -> (+b) ) 0 grouping)
    in score*10

-- All the acid state transactions

addVote :: PackageName -> UserId -> Score -> Update VotesState Float
addVote pkgname uid score = do
    VotesState votes <- State.get
    let votes' = Map.insertWith Map.union pkgname (Map.singleton uid score) votes
    State.put $! VotesState votes'
    return $ votesScore $ fromMaybe Map.empty $ Map.lookup pkgname votes'

removeVote :: PackageName -> UserId -> Update VotesState Bool
removeVote pkgname uid = do
    VotesState votes <- State.get
    if userVotedForPackage pkgname uid votes
       then do let votes' = Map.adjust (Map.delete uid) pkgname votes
               State.put $! VotesState votes'
               return True
       else return False

getPackageVoteCount :: PackageName -> Query VotesState Int
getPackageVoteCount pkgname = do
    VotesState votes <- ask
    case Map.lookup pkgname votes of
      Nothing     -> return 0
      Just m -> return $! Map.size m

getPackageVoteScore :: PackageName -> Query VotesState Float
getPackageVoteScore pkgname = do
    VotesState votes <- ask
    case Map.lookup pkgname votes of
      Nothing     -> return 0
      Just m -> return $! votesScore m

getPackageUserVoted :: PackageName -> UserId -> Query VotesState Bool
getPackageUserVoted pkgname uid = do
    VotesState votes <- ask
    return $! userVotedForPackage pkgname uid votes

getPackageUserVote :: PackageName -> UserId -> Query VotesState (Maybe Score)
getPackageUserVote pkgname uid = do
    VotesState votes <- ask
    return $! Map.lookup uid =<< Map.lookup pkgname votes

getAllPackageVoteSets :: Query VotesState (Map PackageName (Map UserId Score))
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
  , 'getPackageVoteScore
  , 'getPackageUserVoted
  , 'getPackageUserVote
  , 'getAllPackageVoteSets
  , 'getVotesState
  , 'replaceVotesState
  ]
