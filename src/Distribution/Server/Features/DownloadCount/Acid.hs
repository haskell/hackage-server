module Distribution.Server.Features.DownloadCount.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.DownloadCount.Backup
import qualified Distribution.Server.Features.DownloadCount.State as State
import qualified Distribution.Server.Features.DownloadCount.Store as Store
import Distribution.Server.Framework

import Data.Time.Clock (getCurrentTime, utctDay)

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  inMemState <- inMemStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.recordedToday = queryState inMemState State.RecordedToday
        , Store.getInMemStats = queryState inMemState State.GetInMemStats
        , Store.replaceInMemStats = \stats -> updateState inMemState (State.ReplaceInMemStats stats)
        , Store.registerDownload = \pkgid -> updateState inMemState (State.RegisterDownload pkgid)
        , Store.checkpointInMemStats = liftIO (createCheckpoint (stateHandle inMemState))
        }
    , Store.backendState = [abstractAcidStateComponent inMemState]
    }

inMemStateComponent :: FilePath -> IO (StateComponent AcidState State.InMemStats)
inMemStateComponent stateDir = do
  initSt <- State.initInMemStats . utctDay <$> getCurrentTime
  st <- openLocalStateFrom (stateDir </> "db" </> "DownloadCount" </> "inmem") initSt
  return StateComponent {
      stateDesc    = "Today's download counts"
    , stateHandle  = st
    , getState     = query st State.GetInMemStats
    , putState     = update st . State.ReplaceInMemStats
    , backupState  = \_ -> inMemBackup
    , restoreState = inMemRestore
    , resetState   = inMemStateComponent
    }
