module Distribution.Server.Features.DownloadCount.Acid
  ( inMemStateComponent
  ) where

import Distribution.Server.Features.DownloadCount.Backup
import qualified Distribution.Server.Features.DownloadCount.State as State
import Distribution.Server.Framework

import Data.Time.Clock (getCurrentTime, utctDay)

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
