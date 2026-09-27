module Distribution.Server.Features.Users.Acid
  ( usersStateComponent
  , adminsStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Users.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Users.Backup
import qualified Distribution.Server.Users.State as Acid
import qualified Distribution.Server.Users.Users as Acid

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  usersState <- usersStateComponent stateDir
  adminsState <- adminsStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getUsers = queryState usersState Acid.GetUserDb
        , Store.addUser = \uname auth -> updateState usersState (Acid.AddUserEnabled uname auth)
        , Store.addDisabledUser = \uname -> updateState usersState (Acid.AddUserDisabled uname)
        , Store.setUserEnabledStatus = \uid enabled -> updateState usersState (Acid.SetUserEnabledStatus uid enabled)
        , Store.deleteUser = \uid -> updateState usersState (Acid.DeleteUser uid)
        , Store.setUserAuth = \uid auth -> updateState usersState (Acid.SetUserAuth uid auth)
        , Store.addAuthToken = \uid token description -> updateState usersState (Acid.AddAuthToken uid token description)
        , Store.revokeAuthToken = \uid token -> updateState usersState (Acid.RevokeAuthToken uid token)
        , Store.getAdminList = queryState adminsState Acid.GetAdminList
        , Store.addAdmin = updateState adminsState . Acid.AddHackageAdmin
        , Store.removeAdmin = updateState adminsState . Acid.RemoveHackageAdmin
        }
    , Store.backendState = [ abstractAcidStateComponent usersState
                           , abstractAcidStateComponent adminsState
                           ]
    }

usersStateComponent :: FilePath -> IO (StateComponent AcidState Acid.Users)
usersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Users") Acid.initialUsers
  return StateComponent {
      stateDesc    = "List of users"
    , stateHandle  = st
    , getState     = query st Acid.GetUserDb
    , putState     = update st . Acid.ReplaceUserDb
    , backupState  = usersBackup
    , restoreState = usersRestore
    , resetState   = usersStateComponent
    }

adminsStateComponent :: FilePath -> IO (StateComponent AcidState Acid.HackageAdmins)
adminsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageAdmins") Acid.initialHackageAdmins
  return StateComponent {
      stateDesc    = "Admins"
    , stateHandle  = st
    , getState     = query st Acid.GetHackageAdmins
    , putState     = update st . Acid.ReplaceHackageAdmins . Acid.adminList
    , backupState  = \_ (Acid.HackageAdmins admins) -> [csvToBackup ["admins.csv"] (groupToCSV admins)]
    , restoreState = Acid.HackageAdmins <$> groupBackup ["admins.csv"]
    , resetState   = adminsStateComponent
    }
