{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.TarIndexCache.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Framework.BlobStorage (BlobId)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    -- | Every tarball blob with a cached tar index, and that index's blob.
    allTarIndices      :: IO [(BlobId, BlobId)]
    -- | Forget every cached tar index. The blobs are not removed.
  , clearTarIndices    :: IO ()
  , findTarIndex       :: BlobId -> IO (Maybe BlobId)
  , setTarIndex        :: BlobId -> BlobId -> IO ()
  }
