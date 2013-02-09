{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RankNTypes, NamedFieldPuns, RecordWildCards, DoRec, BangPatterns #-}
module Distribution.Server.Features.UserDetails (
    initUserDetailsFeature,
    UserDetailsFeature(..),

    AccountDetails(..),
    AccountKind(..)
  ) where

import Distribution.Server.Framework
--import Distribution.Server.Framework.BackupDump
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



-- | A feature to store extra information about users like email addresses.
--
data UserDetailsFeature = UserDetailsFeature {
    userDetailsFeatureInterface :: HackageFeature,

    queryUserDetails  :: UserId -> MonadIO m => m (Maybe AccountDetails),
    updateUserDetails :: UserId -> AccountDetails -> MonadIO m => m ()
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
  deriving (Eq, Show, Typeable)

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

----------------------------------------
-- Feature definition & initialisation
--

initUserDetailsFeature :: ServerEnv -> UserFeature -> CoreFeature -> IO UserDetailsFeature
initUserDetailsFeature ServerEnv{serverStateDir} users core = do

  -- Canonical state
  usersDetailsState <- userDetailsStateComponent serverStateDir

  let feature = userDetailsFeature usersDetailsState users core

  return feature

userDetailsStateComponent :: FilePath -> IO (StateComponent UserDetailsTable)
userDetailsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserDetails") emptyUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , acidState    = st
    , getState     = query st GetUserDetailsTable
    , putState     = update st . ReplaceUserDetailsTable
    , backupState  = \_ -> [] -- \users -> [csvToBackup ["users.csv"] (usersToCSV users)]
    , restoreState = restoreBackupUnimplemented
    , resetState   = userDetailsStateComponent
    }

userDetailsFeature :: StateComponent UserDetailsTable
                   -> UserFeature
                   -> CoreFeature
                   -> UserDetailsFeature
userDetailsFeature userDetailsState UserFeature{..} CoreFeature{..}
  = UserDetailsFeature {..}

  where
    userDetailsFeatureInterface = (emptyHackageFeature "user-details") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [userNameContactResource, userAdminInfoResource]
      , featureState     = [abstractStateComponent userDetailsState]
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

    handlerGetUserNameContact :: DynamicPath -> ServerPart Response
    handlerGetUserNameContact dpath = runServerPartE $ do
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

    handlerPutUserNameContact :: DynamicPath -> ServerPart Response
    handlerPutUserNameContact dpath = runServerPartE $ do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        NameAndContact name email <- expectAesonContent
        updateState userDetailsState (SetUserNameContact uid name email)
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPart Response
    handlerDeleteUserNameContact dpath = runServerPartE $ do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        updateState userDetailsState (SetUserNameContact uid T.empty T.empty)
        noContent $ toResponse ()

    handlerGetAdminInfo :: DynamicPath -> ServerPart Response
    handlerGetAdminInfo dpath = runServerPartE $ do
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

    handlerPutAdminInfo :: DynamicPath -> ServerPart Response
    handlerPutAdminInfo dpath = runServerPartE $ do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        AdminInfo akind notes <- expectAesonContent
        updateState userDetailsState (SetUserAdminInfo uid akind notes)
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPart Response
    handlerDeleteAdminInfo dpath = runServerPartE $ do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        updateState userDetailsState (SetUserAdminInfo uid Nothing T.empty)
        noContent $ toResponse ()


data NameAndContact = NameAndContact { ui_name  :: Text, ui_contactEmailAddress :: Text }
data AdminInfo      = AdminInfo      { ui_accountKind :: Maybe AccountKind, ui_notes :: Text }
$(deriveJSON (drop 3) ''NameAndContact)
$(deriveJSON (drop 3) ''AdminInfo)
$(deriveJSON id       ''AccountKind)

    --TODO: link up to user feature to delete 

