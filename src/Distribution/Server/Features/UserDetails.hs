{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RankNTypes,
    NamedFieldPuns, RecordWildCards, RecursiveDo, BangPatterns #-}
module Distribution.Server.Features.UserDetails (
    initUserDetailsFeature,
    UserDetailsFeature(..),

    AccountDetails(..),
    AccountKind(..)
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Users
import Distribution.Server.Features.Core

import Distribution.Server.Users.Types

import Data.SafeCopy (base, deriveSafeCopy)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Aeson.TH

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)


-- | A feature to store extra information about users like email addresses.
--
data UserDetailsFeature = UserDetailsFeature {
    userDetailsFeatureInterface :: HackageFeature,

    queryUserDetails  :: forall m. MonadIO m => UserId -> m (Maybe AccountDetails),
    updateUserDetails :: forall m. MonadIO m => UserId -> AccountDetails -> m ()
}

instance IsHackageFeature UserDetailsFeature where
  getFeatureInterface = userDetailsFeatureInterface


-------------------------
-- Types of stored data
--

data AccountDetails = AccountDetails {
                        accountName         :: !Text,
                        accountContactEmail :: !Text,
                        accountKind         :: Maybe AccountKind,
                        accountAdminNotes   :: !Text
                      }
  deriving (Eq, Show, Typeable)


data AccountKind = AccountKindRealUser | AccountKindSpecial
  deriving (Eq, Show, Typeable, Enum, Bounded)

newtype UserDetailsTable = UserDetailsTable (IntMap AccountDetails)
  deriving (Eq, Show, Typeable)

emptyAccountDetails :: AccountDetails
emptyAccountDetails   = AccountDetails T.empty T.empty Nothing T.empty
emptyUserDetailsTable :: UserDetailsTable
emptyUserDetailsTable = UserDetailsTable IntMap.empty

$(deriveSafeCopy 0 'base ''AccountDetails)
$(deriveSafeCopy 0 'base ''AccountKind)
$(deriveSafeCopy 0 'base ''UserDetailsTable)

instance MemSize AccountDetails where
    memSize (AccountDetails a b c d) = memSize4 a b c d

instance MemSize AccountKind where
    memSize _ = memSize0

instance MemSize UserDetailsTable where
    memSize (UserDetailsTable a) = memSize1 a


------------------------------
-- State queries and updates
--

getUserDetailsTable :: Query UserDetailsTable UserDetailsTable
getUserDetailsTable = ask

replaceUserDetailsTable :: UserDetailsTable -> Update UserDetailsTable ()
replaceUserDetailsTable = put

lookupUserDetails :: UserId -> Query UserDetailsTable (Maybe AccountDetails)
lookupUserDetails (UserId uid) = do
    UserDetailsTable tbl <- ask
    return $! IntMap.lookup uid tbl

setUserDetails :: UserId -> AccountDetails -> Update UserDetailsTable ()
setUserDetails (UserId uid) udetails = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.insert uid udetails tbl)

deleteUserDetails :: UserId -> Update UserDetailsTable Bool
deleteUserDetails (UserId uid) = do
    UserDetailsTable tbl <- get
    if IntMap.member uid tbl
      then do put $! UserDetailsTable (IntMap.delete uid tbl)
              return True
      else return False

setUserNameContact :: UserId -> Text -> Text -> Update UserDetailsTable ()
setUserNameContact (UserId uid) name email = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.alter upd uid tbl)
  where
    upd Nothing         = Just emptyAccountDetails { accountName = name, accountContactEmail = email }
    upd (Just udetails) = Just udetails            { accountName = name, accountContactEmail = email }

setUserAdminInfo :: UserId -> Maybe AccountKind -> Text -> Update UserDetailsTable ()
setUserAdminInfo (UserId uid) akind notes = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.alter upd uid tbl)
  where
    upd Nothing         = Just emptyAccountDetails { accountKind = akind, accountAdminNotes = notes }
    upd (Just udetails) = Just udetails            { accountKind = akind, accountAdminNotes = notes }

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

----------------------------
-- Data backup and restore
--

userDetailsBackup :: RestoreBackup UserDetailsTable
userDetailsBackup = updateUserBackup emptyUserDetailsTable

updateUserBackup :: UserDetailsTable -> RestoreBackup UserDetailsTable
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importUserDetails csv users
        return (updateUserBackup users')
      _ ->
        return (updateUserBackup users)
  , restoreFinalize =
     return users
  }

importUserDetails :: CSV -> UserDetailsTable -> Restore UserDetailsTable
importUserDetails = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> UserDetailsTable -> Restore UserDetailsTable
    fromRecord [idStr, nameStr, emailStr, kindStr, notesStr] (UserDetailsTable tbl) = do
        UserId uid <- parseText "user id" idStr
        akind      <- parseKind kindStr
        let udetails = AccountDetails {
                        accountName         = T.pack nameStr,
                        accountContactEmail = T.pack emailStr,
                        accountKind         = akind,
                        accountAdminNotes   = T.pack notesStr
                      }
        return $! UserDetailsTable (IntMap.insert uid udetails tbl)

    fromRecord x _ = fail $ "Error processing user details record: " ++ show x

    parseKind ""        = return Nothing
    parseKind "real"    = return (Just AccountKindRealUser)
    parseKind "special" = return (Just AccountKindSpecial)
    parseKind sts       = fail $ "unable to parse account kind: " ++ sts

userDetailsToCSV :: BackupType -> UserDetailsTable -> CSV
userDetailsToCSV backuptype (UserDetailsTable tbl)
    = ([showVersion userCSVVer]:) $
      (userdetailsCSVKey:) $

      flip map (IntMap.toList tbl) $ \(uid, udetails) ->
      [ display (UserId uid)
      , T.unpack (accountName udetails)  --FIXME: apparently the csv lib doesn't do unicode properly
      , if backuptype == FullBackup
        then T.unpack (accountContactEmail udetails)
        else "hidden-email@nowhere.org"
      , infoToAccountKind udetails
      , T.unpack (accountAdminNotes udetails)
      ]

 where
    userdetailsCSVKey =
       [ "uid"
       , "realname"
       , "email"
       , "kind"
       , "notes"
       ]
    userCSVVer = Version [0,2] []

    -- one of "enabled" "disabled" or "deleted"
    infoToAccountKind :: AccountDetails -> String
    infoToAccountKind udetails = case accountKind udetails of
      Nothing                  -> ""
      Just AccountKindRealUser -> "real"
      Just AccountKindSpecial  -> "special"

----------------------------------------
-- Feature definition & initialisation
--

initUserDetailsFeature :: ServerEnv
                       -> IO (UserFeature
                           -> CoreFeature
                           -> IO UserDetailsFeature)
initUserDetailsFeature ServerEnv{serverStateDir} = do
    -- Canonical state
    usersDetailsState <- userDetailsStateComponent serverStateDir

    --TODO: link up to user feature to delete

    return $ \users core -> do
      let feature = userDetailsFeature usersDetailsState users core
      return feature


userDetailsFeature :: StateComponent AcidState UserDetailsTable
                   -> UserFeature
                   -> CoreFeature
                   -> UserDetailsFeature
userDetailsFeature userDetailsState UserFeature{..} CoreFeature{..}
  = UserDetailsFeature {..}

  where
    userDetailsFeatureInterface = (emptyHackageFeature "user-details") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [userNameContactResource, userAdminInfoResource]
      , featureState     = [abstractAcidStateComponent userDetailsState]
      , featureCaches    = []
      }

    -- Resources
    --

    userNameContactResource =
      (resourceAt "/user/:username/name-contact.:format") {
        resourceDesc   = [ (GET,    "get the name and contact details of a user account")
                         , (PUT,    "set the name and contact details of a user account")
                         , (DELETE, "delete the name and contact details of a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetUserNameContact) ]
      , resourcePut    = [ ("json", handlerPutUserNameContact) ]
      , resourceDelete = [ ("",     handlerDeleteUserNameContact) ]
      }

    userAdminInfoResource =
      (resourceAt "/user/:username/admin-info.:format") {
        resourceDesc   = [ (GET,    "get the administrators' notes for a user account")
                         , (PUT,    "set the administrators' notes for a user account")
                         , (DELETE, "delete the administrators' notes for a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetAdminInfo) ]
      , resourcePut    = [ ("json", handlerPutAdminInfo) ]
      , resourceDelete = [ ("", handlerDeleteAdminInfo) ]
      }

    -- Queries and updates
    --

    queryUserDetails :: UserId -> MonadIO m => m (Maybe AccountDetails)
    queryUserDetails uid = queryState userDetailsState (LookupUserDetails uid)

    updateUserDetails :: UserId -> AccountDetails -> MonadIO m => m ()
    updateUserDetails uid udetails = do
      updateState userDetailsState (SetUserDetails uid udetails)

    -- Request handlers
    --

    handlerGetUserNameContact :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        udetails <- queryUserDetails uid
        return $ toResponse (Aeson.toJSON (render udetails))
      where
        render Nothing = NameAndContact T.empty T.empty
        render (Just (AccountDetails { accountName, accountContactEmail })) =
            NameAndContact {
              ui_name                = accountName,
              ui_contactEmailAddress = accountContactEmail
            }

    handlerPutUserNameContact :: DynamicPath -> ServerPartE Response
    handlerPutUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        NameAndContact name email <- expectAesonContent
        updateState userDetailsState (SetUserNameContact uid name email)
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPartE Response
    handlerDeleteUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        updateState userDetailsState (SetUserNameContact uid T.empty T.empty)
        noContent $ toResponse ()

    handlerGetAdminInfo :: DynamicPath -> ServerPartE Response
    handlerGetAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        udetails <- queryUserDetails uid
        return $ toResponse (Aeson.toJSON (render udetails))
      where
        render Nothing = AdminInfo Nothing T.empty
        render (Just (AccountDetails { accountKind, accountAdminNotes })) =
            AdminInfo {
              ui_accountKind = accountKind,
              ui_notes       = accountAdminNotes
            }

    handlerPutAdminInfo :: DynamicPath -> ServerPartE Response
    handlerPutAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        AdminInfo akind notes <- expectAesonContent
        updateState userDetailsState (SetUserAdminInfo uid akind notes)
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPartE Response
    handlerDeleteAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        updateState userDetailsState (SetUserAdminInfo uid Nothing T.empty)
        noContent $ toResponse ()


data NameAndContact = NameAndContact { ui_name  :: Text, ui_contactEmailAddress :: Text }
data AdminInfo      = AdminInfo      { ui_accountKind :: Maybe AccountKind, ui_notes :: Text }


deriveJSON (compatAesonOptionsDropPrefix "ui_") ''NameAndContact
deriveJSON (compatAesonOptionsDropPrefix "ui_") ''AdminInfo
deriveJSON  compatAesonOptions                  ''AccountKind
