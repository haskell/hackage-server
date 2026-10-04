{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Documentation.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Framework.BlobStorage (BlobId)

import Distribution.Package (PackageId, PackageIdentifier)

import Control.Monad.Trans (MonadIO)
import Data.Map (Map)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    hasDocumentation       :: forall m. MonadIO m => PackageIdentifier -> m Bool
  , lookupDocumentation   :: forall m. MonadIO m => PackageIdentifier -> m (Maybe BlobId)
  , getDocumentationIndex :: forall m. MonadIO m => m (Map PackageId BlobId)
  , insertDocumentation   :: forall m. MonadIO m => PackageId -> BlobId -> m ()
  , removeDocumentation   :: forall m. MonadIO m => PackageId -> m ()
  }
