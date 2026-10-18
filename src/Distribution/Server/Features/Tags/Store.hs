{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Tags.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageName)
import Distribution.Server.Features.Tags.Types (Tag)
import Distribution.Server.Framework (AbstractStateComponent)

import Control.Monad.Trans (MonadIO)
import Data.Map (Map)
import Data.Set (Set)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    -- | Every tag with its number of packages, in tag order.
    getTagCounts :: forall m. MonadIO m => m [(Tag, Int)]
  , getTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag)
  , getReviewTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag, Set Tag)
  , getTagAlias :: forall m. MonadIO m => Tag -> m Tag
  , getPackagesForTag :: forall m. MonadIO m => Tag -> m (Set PackageName)
    -- | The tags of each given package, including packages with none.
  , getTagsForPackages :: forall m. MonadIO m => [PackageName] -> m (Map PackageName (Set Tag))
  , setPackageTags :: forall m. MonadIO m => PackageName -> Set Tag -> m ()
  , setTagPackages :: forall m. MonadIO m => Tag -> Set PackageName -> m ()
  , addTagAlias :: forall m. MonadIO m => Tag -> Tag -> m ()
  , insertReviewTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m ()
  , replaceReviewTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m ()
  }
