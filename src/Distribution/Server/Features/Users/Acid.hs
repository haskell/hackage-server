module Distribution.Server.Features.Users.Acid
  ( usersStateComponent
  , adminsStateComponent
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Users.Backup
import qualified Distribution.Server.Users.State as Acid
import qualified Distribution.Server.Users.Users as Acid

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
