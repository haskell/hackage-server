{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.PackageCandidates.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageId, PackageName)
import Distribution.Server.Features.PackageCandidates.Types (CandPkgInfo)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.Types (PkgInfo)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getCandidateIndex :: forall m. MonadIO m => m (PackageIndex CandPkgInfo)
  , candidateTarballIsMigrated :: forall m. MonadIO m => m Bool
  , setCandidateTarballMigrated :: forall m. MonadIO m => m ()
  , addCandidate :: forall m. MonadIO m => CandPkgInfo -> m ()
  , deleteCandidate :: forall m. MonadIO m => PackageId -> m ()
  , deleteCandidates :: forall m. MonadIO m => PackageName -> m ()
  , updateCandidatePkgInfo :: forall m. MonadIO m => PackageId -> PkgInfo -> m Bool
  }
