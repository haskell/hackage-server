{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RankNTypes,
    NamedFieldPuns, RecordWildCards, RecursiveDo, BangPatterns, OverloadedStrings #-}
module Distribution.Server.Features.UserDetails (
    initUserDetailsFeature,
    UserDetailsFeature(..),

    AccountDetails(..),
    AccountKind(..),

    -- Exposed to setup features in test context that use the database
    dbQueryUserDetails
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails.State
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core
import Distribution.Server.Features.Database (DatabaseFeature (..), HackageDb (..))
import qualified Distribution.Server.Features.Database as Database

import Distribution.Server.Users.Types
import Distribution.Server.Util.Validators (guardValidLookingEmail, guardValidLookingName)

import Data.SafeCopy (base, deriveSafeCopy)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Aeson.TH

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)
import Database.Beam hiding (update)
import Database.Beam.Backend.SQL.BeamExtensions


-- | A feature to store extra information about users like email addresses.
--
data UserDetailsFeature = UserDetailsFeature {
    userDetailsFeatureInterface :: HackageFeature,

    queryUserDetails  :: UserId -> Database.Transaction (Maybe AccountDetails),
    updateUserDetails :: UserId -> AccountDetails -> Database.Transaction ()
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
  deriving (Eq, Show)


data AccountKind = AccountKindRealUser | AccountKindSpecial
  deriving (Eq, Show, Enum, Bounded)

newtype UserDetailsTable = UserDetailsTable (IntMap AccountDetails)
  deriving (Eq, Show)

data NameAndContact = NameAndContact { ui_name  :: Text, ui_contactEmailAddress :: Text }
data AdminInfo      = AdminInfo      { ui_accountKind :: Maybe AccountKind, ui_notes :: Text }

deriveJSON (compatAesonOptionsDropPrefix "ui_") ''NameAndContact
deriveJSON  compatAesonOptions                  ''AccountKind
deriveJSON (compatAesonOptionsDropPrefix "ui_") ''AdminInfo

emptyAccountDetails :: AccountDetails
emptyAccountDetails   = AccountDetails T.empty T.empty Nothing T.empty

emptyUserDetailsTable :: UserDetailsTable
emptyUserDetailsTable = UserDetailsTable IntMap.empty

$(deriveSafeCopy 0 'base ''AccountKind)
$(deriveSafeCopy 0 'base ''AccountDetails)
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
                       -> IO (DatabaseFeature
                           -> UserFeature
                           -> CoreFeature
                           -> UploadFeature
                           -> IO UserDetailsFeature)
initUserDetailsFeature ServerEnv{serverStateDir, serverTemplatesDir, serverTemplatesMode} = do
    -- Canonical state
    usersDetailsState <- userDetailsStateComponent serverStateDir

    --TODO: link up to user feature to delete

    templates <-
      loadTemplates serverTemplatesMode
      [serverTemplatesDir, serverTemplatesDir </> "UserDetails"]
      [ "user-details-form.html" ]

    return $ \database users core upload -> do
      migrateStateToDatabase usersDetailsState database
      
      let feature = userDetailsFeature templates usersDetailsState database users core upload
      return feature

migrateStateToDatabase :: StateComponent AcidState UserDetailsTable 
                       -> DatabaseFeature 
                       -> IO ()
migrateStateToDatabase userDetailsState DatabaseFeature{..} = do
  (UserDetailsTable tbl) <- queryState userDetailsState GetUserDetailsTable
  withTransaction $ do
    forM_ (IntMap.toList tbl) $ \(uid, details) -> do
      -- NOTE: This is actually performing a merge
      --       by inserting records of user ids we know nothing about.
      r <- accountDetailsFindByUserId (UserId uid)
      when (isNothing r) $
        accountDetailsUpsert AccountDetailsRow {
            _adUserId = fromIntegral uid,
            _adName = accountName details,
            _adContactEmail = accountContactEmail details,
            _adKind = fromAccountKind (accountKind details),
            _adAdminNotes = accountAdminNotes details
          }

userDetailsFeature :: Templates
                   -> StateComponent AcidState UserDetailsTable
                   -> DatabaseFeature
                   -> UserFeature
                   -> CoreFeature
                   -> UploadFeature
                   -> UserDetailsFeature
userDetailsFeature templates userDetailsState DatabaseFeature{..} UserFeature{..} CoreFeature{..} UploadFeature{uploadersGroup}
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
      , resourceGet    = [ ("json", handlerGetUserNameContact)
                         , ("html", handlerGetUserNameContactHtml)
                         ]
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

    -- handlerWithConnection :: (Database.Connection -> DynamicPath -> ServerPartE Response) -> DynamicPath -> ServerPartE Response
    -- handlerWithConnection handler dpath =
    --   Database.withConnection $ \conn -> _ handler conn dpath

    -- Queries and updates
    --

    queryUserDetails :: UserId -> Database.Transaction (Maybe AccountDetails)
    queryUserDetails = dbQueryUserDetails

    updateUserDetails :: UserId -> AccountDetails -> Database.Transaction ()
    updateUserDetails = dbUpdateUserDetails

    -- Request handlers
    --
    handlerGetUserNameContactHtml :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContactHtml dpath = do
      (uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      template <- getTemplate templates "user-details-form.html"
      udetails <- withTransaction $ queryUserDetails uid
      showConfirmationOfSave <- not . null <$> queryString (lookBSs "showConfirmationOfSave")
      let
        emailTxt = maybe "" accountContactEmail udetails
        nameTxt  = maybe "" accountName         udetails
      cacheControl
        [Private]
        (etagFromHash
          ( emailTxt
          , nameTxt
          , showConfirmationOfSave
          )
        )
      ok . toResponse $
        template
          [ "username" $= display (userName uinfo)
          , "contactEmailAddress" $= emailTxt
          , "name" $= nameTxt
          , "showConfirmationOfSave" $= showConfirmationOfSave
          ]

    handlerGetUserNameContact :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        udetails <- withTransaction $ queryUserDetails uid
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
        void $ guardAuthorisedWhenInAnyGroup [uploadersGroup, adminGroup]
        NameAndContact name email <- expectAesonContent
        guardValidLookingName name
        guardValidLookingEmail email
        withTransaction $ dbModifyAccountDetails uid (\adetails -> adetails { accountName = name, accountContactEmail = email })
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPartE Response
    handlerDeleteUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        withTransaction $ dbModifyAccountDetails uid (\adetails -> adetails { accountName = "", accountContactEmail = "" })
        noContent $ toResponse ()

    handlerGetAdminInfo :: DynamicPath -> ServerPartE Response
    handlerGetAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        udetails <- withTransaction $ queryUserDetails uid
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
        withTransaction $ dbModifyAccountDetails uid (\adetails -> adetails { accountKind = akind, accountAdminNotes = notes })
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPartE Response
    handlerDeleteAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        withTransaction $ dbModifyAccountDetails uid (\adetails -> adetails { accountKind = Nothing, accountAdminNotes = "" })
        noContent $ toResponse ()


-- Database

dbQueryUserDetails :: UserId -> Database.Transaction (Maybe AccountDetails)
dbQueryUserDetails uid = fmap toUserDetails <$> accountDetailsFindByUserId uid

dbUpdateUserDetails :: UserId -> AccountDetails -> Database.Transaction ()
dbUpdateUserDetails uid@(UserId _uid) udetails = dbModifyAccountDetails uid (const udetails)

-- convenient helper to update only part of the record.
-- We use the same record for information that is editable by the user and information that is only editable by admins.
dbModifyAccountDetails :: UserId -> (AccountDetails -> AccountDetails) -> Database.Transaction ()
dbModifyAccountDetails uid@(UserId _uid) change = do
  -- NOTE: we need to query the current value because we are updating only some of the fields.
  madetails <- dbQueryUserDetails uid
  -- NOTE: We could assume that the record exist since updateUserDetails is called from UserSignup
  let adetails = fromMaybe AccountDetails {
                    accountName = "",
                    accountContactEmail = "",
                    accountKind = Nothing,
                    accountAdminNotes = ""
                  } madetails
  let cdetails = change adetails

  accountDetailsUpsert AccountDetailsRow {
      _adUserId = fromIntegral _uid,
      _adName = accountName cdetails,
      _adContactEmail = accountContactEmail cdetails,
      _adKind = fromAccountKind (accountKind cdetails),
      _adAdminNotes = accountAdminNotes cdetails
  }

accountDetailsFindByUserId :: UserId -> Database.Transaction (Maybe AccountDetailsRow)
accountDetailsFindByUserId (UserId userId) =
  Database.runSelectReturningOne $
    select $
      filter_ (\ad -> _adUserId ad ==. val_ (fromIntegral userId)) $
        all_ (_accountDetails Database.hackageDb)

-- Use the values from the INSERT that caused the conflict
accountDetailsUpsert :: AccountDetailsRow -> Database.Transaction ()
accountDetailsUpsert details =
  Database.runInsert $
    insertOnConflict
      (_accountDetails Database.hackageDb)
      (insertValues [details])
      (conflictingFields primaryKey)
      ( onConflictUpdateSet $ \fields _oldRow ->
          mconcat
            [ _adName fields <-. val_ (_adName details),
              _adContactEmail fields <-. val_ (_adContactEmail details),
              _adKind fields <-. val_ (_adKind details),
              _adAdminNotes fields <-. val_ (_adAdminNotes details)
            ]
      )

toUserDetails :: AccountDetailsRow -> AccountDetails
toUserDetails AccountDetailsRow {..} =
  AccountDetails
    { accountName = _adName,
      accountContactEmail = _adContactEmail,
      accountKind = 
        -- NOTE: Should we fail to convert instead?
        toAccountKind _adKind,
      accountAdminNotes = _adAdminNotes
    }

toAccountKind :: Maybe Text -> Maybe AccountKind
toAccountKind adKind =
  case adKind of
    Just "real_user" -> Just AccountKindRealUser
    Just "special" -> Just AccountKindSpecial
    _ -> Nothing

fromAccountKind :: Maybe AccountKind -> Maybe Text
fromAccountKind adKind =
  case adKind of
    Just AccountKindRealUser -> Just "real_user"
    Just AccountKindSpecial -> Just "special"
    _ -> Nothing
