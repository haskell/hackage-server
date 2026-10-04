{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Votes.Store
  ( Backend(..)
  , Store(..)
  , votesScore
  ) where

import Distribution.Server.Features.Votes.Types
import Distribution.Server.Framework.Feature (AbstractStateComponent)
import Distribution.Server.Users.Types (UserId)

import Distribution.Package (PackageName)

import Control.Arrow ((&&&))
import Control.Monad.Trans (MonadIO)
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getAllPackageVoteSets :: forall m. MonadIO m => m (Map.Map PackageName (Map.Map UserId Score))
  , addVote               :: forall m. MonadIO m => PackageName -> UserId -> Score -> m Float
  , removeVote            :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , getPackageVoteCount   :: forall m. MonadIO m => PackageName -> m Int
  , getPackageVoteScore   :: forall m. MonadIO m => PackageName -> m Float
  , getPackageUserVoted   :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , getPackageUserVote    :: forall m. MonadIO m => PackageName -> UserId -> m (Maybe Score)
  }

-- Using a Bayesian average (m=1.5, C=2) to calculate scoring
votesScore :: Map UserId Score -> Float
votesScore m =
     let grouping = map (head &&& length) . group . sort . Map.elems $ m
         score :: Float
         score = fromIntegral ((sum $ map (uncurry (*)) grouping) + 3)/
                 fromIntegral (2 + sum (map snd grouping))
         roundedScore = fromIntegral (round (score * 4) :: Int) / 4
     in roundedScore
