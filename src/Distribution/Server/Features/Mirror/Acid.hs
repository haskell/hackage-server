module Distribution.Server.Features.Mirror.Acid
  ( acidStore
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.Mirror.Store
import Distribution.Server.Users.State
  ( MirrorClients(..)
  , GetMirrorClients(..)
  , GetMirrorClientsList(..)
  , ReplaceMirrorClients(..)
  , AddMirrorClient(..)
  , RemoveMirrorClient(..)
  , initialMirrorClients
  , mirrorClients
  )
import Distribution.Server.Users.Backup (groupBackup, groupToCSV)

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  mirrorersState <- mirrorersStateComponent stateDir
  pure Backend {
      backendStore = Store {
          getMirrorClientsList = queryState mirrorersState GetMirrorClientsList
        , addMirrorClient      = \uid -> updateState mirrorersState (AddMirrorClient uid)
        , removeMirrorClient   = \uid -> updateState mirrorersState (RemoveMirrorClient uid)
        }
    , backendState = [abstractAcidStateComponent mirrorersState]
    }

mirrorersStateComponent :: FilePath -> IO (StateComponent AcidState MirrorClients)
mirrorersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "MirrorClients") initialMirrorClients
  return StateComponent {
      stateDesc    = "Mirror clients"
    , stateHandle  = st
    , getState     = query st GetMirrorClients
    , putState     = update st . ReplaceMirrorClients . mirrorClients
    , backupState  = \_ (MirrorClients clients) -> [csvToBackup ["clients.csv"] $ groupToCSV clients]
    , restoreState = MirrorClients <$> groupBackup ["clients.csv"]
    , resetState   = mirrorersStateComponent
    }
