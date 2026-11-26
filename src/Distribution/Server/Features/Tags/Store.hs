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
    -- | Add and remove tags of a package, applied to its current tags.
    -- Returns the resulting tags.
  , addRemovePackageTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m (Set Tag)
    -- | Add and remove tags as 'addRemovePackageTags' does, and drop the
    -- given proposed additions and removals from the package's review tags.
    -- Returns the resulting tags and review tags.
  , editPackageTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> Set Tag -> Set Tag -> m (Set Tag, (Set Tag, Set Tag))
    -- | Propose adding the given tags the package does not have and removing
    -- those it has, judged against its current tags. Returns what was
    -- proposed.
  , proposePackageTags :: forall m. MonadIO m => PackageName -> Set Tag -> Set Tag -> m (Set Tag, Set Tag)
    -- | Replace the first tag with the second on every package that has it.
    -- Returns the changed packages with their resulting tags.
  , mergePackageTag :: forall m. MonadIO m => Tag -> Tag -> m [(PackageName, Set Tag)]
  , setTagPackages :: forall m. MonadIO m => Tag -> Set PackageName -> m ()
  , addTagAlias :: forall m. MonadIO m => Tag -> Tag -> m ()
  }
