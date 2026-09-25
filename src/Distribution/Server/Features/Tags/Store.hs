{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Tags.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageName)
import Distribution.Server.Features.Tags.State (PackageTags)
import Distribution.Server.Features.Tags.Types (Tag)
import Distribution.Server.Framework (AbstractStateComponent)

import Control.Monad.Trans (MonadIO)
import Data.Set (Set)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getTagList :: forall m. MonadIO m => m [(Tag, Set PackageName)]
  , getTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag)
  , getReviewTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag, Set Tag)
  , getTagAlias :: forall m. MonadIO m => Tag -> m Tag
  , getPackagesForTag :: forall m. MonadIO m => Tag -> m (Set PackageName)
  , getPackageTags :: forall m. MonadIO m => m PackageTags
  , setPackageTags :: forall m. MonadIO m => PackageName -> Set Tag -> m ()
  , setTagPackages :: forall m. MonadIO m => Tag -> Set PackageName -> m ()
  , addTagAlias :: forall m. MonadIO m => Tag -> Tag -> m ()
  , insertReviewTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m ()
  , replaceReviewTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m ()
  }
