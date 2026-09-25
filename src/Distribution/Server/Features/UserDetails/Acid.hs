{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Distribution.Server.Features.UserDetails.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.UserDetails.State
import qualified Distribution.Server.Features.UserDetails.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.UserDetails.Backup

------------------------------
-- Acid event types
--

-- This splice generates an orphan IsAcidic UserDetailsTable instance:
-- IsAcidic comes from acid-state, and UserDetailsTable is defined in
-- State.
--
-- makeAcidic generates the instance together with the event
-- types. The serialized event tags of the event types depend on the
-- name of the module where the splice is run, so for simplicity we
-- continue to run the splice here, despite UserDetailsTable no longer
-- being defined here.
makeAcidic ''UserDetailsTable [
    --queries
    'getUserDetailsTable,
    'lookupUserDetails,
    --updates
    'replaceUserDetailsTable,
    'setUserDetails,
    'setUserNameContact,
    'setUserAdminInfo,
    'deleteUserDetails
  ]


acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  userDetailsState <- userDetailsStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupUserDetails = \uid -> queryState userDetailsState (LookupUserDetails uid)
        , Store.setUserDetails = \uid details -> updateState userDetailsState (SetUserDetails uid details)
        , Store.setUserNameContact = \uid name email -> updateState userDetailsState (SetUserNameContact uid name email)
        , Store.setUserAdminInfo = \uid kind notes -> updateState userDetailsState (SetUserAdminInfo uid kind notes)
        }
    , Store.backendState = [abstractAcidStateComponent userDetailsState]
    }

---------------------
-- State components
--

userDetailsStateComponent :: FilePath -> IO (StateComponent AcidState UserDetailsTable)
userDetailsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserDetails") emptyUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , stateHandle  = st
    , getState     = query st GetUserDetailsTable
    , putState     = update st . ReplaceUserDetailsTable
    , backupState  = \backuptype users ->
        [csvToBackup ["users.csv"] (userDetailsToCSV backuptype users)]
    , restoreState = userDetailsBackup
    , resetState   = userDetailsStateComponent
    }
