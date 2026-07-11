module Distribution.Server.Features.Core.Acid
  ( packagesStateComponent
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Features.Core.Backup
import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Framework

packagesStateComponent :: Verbosity -> Bool -> FilePath -> IO (StateComponent AcidState Acid.PackagesState)
packagesStateComponent verbosity freshDB stateDir = do
  let stateFile = stateDir </> "db" </> "PackagesState"
  st <- logTiming verbosity "Loaded PackagesState" $
          openLocalStateFrom stateFile (Acid.initialPackagesState freshDB)
  return StateComponent {
       stateDesc    = "Main package database"
     , stateHandle  = st
     , getState     = query st Acid.GetPackagesState
     , putState     = update st . Acid.ReplacePackagesState
     , backupState  = \_ -> indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity True
     }
