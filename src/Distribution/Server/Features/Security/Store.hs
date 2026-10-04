{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Security.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.Security.FileInfo (FileInfo)
import Distribution.Server.Features.Security.ResponseContentTypes (Mirrors, Root)
import Distribution.Server.Features.Security.State (SecurityState, SecurityStateFiles)
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
    getSecurityState :: forall m. MonadIO m => m SecurityState
  , getSecurityFiles :: forall m. MonadIO m => m (Maybe SecurityStateFiles)
  , setRootMirrorsAndKeys :: forall m. MonadIO m => Root -> Mirrors -> Some Sec.Key -> Some Sec.Key -> UTCTime -> m ()
  , setTarGzFileInfo :: forall m. MonadIO m => FileInfo -> FileInfo -> UTCTime -> m ()
  , resignSnapshotAndTimestamp :: forall m. MonadIO m => Int -> UTCTime -> m ()
  }
