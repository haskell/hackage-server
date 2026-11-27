{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Security.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.Security.FileInfo (FileInfo)
import Distribution.Server.Features.Security.ResponseContentTypes (Mirrors, Root)
import Distribution.Server.Features.Security.Types (SecurityStateFiles)
import Distribution.Server.Framework (AbstractStateComponent)

import Control.Monad.Trans (MonadIO)
import Data.Time (UTCTime)
import Hackage.Security.Util.Some (Some)
import qualified Hackage.Security.Server as Sec

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getSecurityFiles :: forall m. MonadIO m => m (Maybe SecurityStateFiles)
    -- | Store the root, mirrors and keys and re-sign, unless they equal the
    -- stored ones. Returns whether they changed.
  , setRootMirrorsAndKeysIfChanged :: forall m. MonadIO m => Root -> Mirrors -> Some Sec.Key -> Some Sec.Key -> UTCTime -> m Bool
  , setTarGzFileInfo :: forall m. MonadIO m => FileInfo -> FileInfo -> UTCTime -> m ()
  , resignSnapshotAndTimestamp :: forall m. MonadIO m => Int -> UTCTime -> m ()
  }
