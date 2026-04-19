{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Distribution.Server.Features.UserDetails (
    initUserDetailsFeature,
    UserDetailsFeature(..),
  ) where

import qualified Distribution.Server.Features.UserDetails.Acid as Acid
import Distribution.Server.Features.UserDetails.Backup
import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Users.Types
import Distribution.Server.Util.Validators (guardValidLookingEmail, guardValidLookingName)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson

import Distribution.Text (display)


-- | A feature to store extra information about users like email addresses.
--
data UserDetailsFeature = UserDetailsFeature {
    userDetailsFeatureInterface :: HackageFeature,

    queryUserDetails  :: forall m. MonadIO m => UserId -> m (Maybe AccountDetails),
    updateUserDetails :: forall m. MonadIO m => UserId -> AccountDetails -> m ()
}

instance IsHackageFeature UserDetailsFeature where
  getFeatureInterface = userDetailsFeatureInterface


---------------------
-- State components
--

userDetailsStateComponent :: FilePath -> IO (StateComponent AcidState Acid.UserDetailsTable)
userDetailsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserDetails") Acid.emptyUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , stateHandle  = st
    , getState     = query st Acid.GetUserDetailsTable
    , putState     = update st . Acid.ReplaceUserDetailsTable
    , backupState  = \backuptype users ->
        [csvToBackup ["users.csv"] (userDetailsToCSV backuptype users)]
    , restoreState = userDetailsBackup
    , resetState   = userDetailsStateComponent
    }

----------------------------------------
-- Feature definition & initialisation
--

initUserDetailsFeature :: ServerEnv
                       -> IO (UserFeature
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

    return $ \users core upload -> do
      let feature = userDetailsFeature templates usersDetailsState users core upload
      return feature


userDetailsFeature :: Templates
                   -> StateComponent AcidState Acid.UserDetailsTable
                   -> UserFeature
                   -> CoreFeature
                   -> UploadFeature
                   -> UserDetailsFeature
userDetailsFeature templates userDetailsState UserFeature{..} CoreFeature{..} UploadFeature{uploadersGroup}
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

    -- Queries and updates
    --

    queryUserDetails :: MonadIO m => UserId -> m (Maybe AccountDetails)
    queryUserDetails uid = queryState userDetailsState (Acid.LookupUserDetails uid)

    updateUserDetails :: MonadIO m => UserId -> AccountDetails -> m ()
    updateUserDetails uid udetails = do
      updateState userDetailsState (Acid.SetUserDetails uid udetails)

    -- Request handlers
    --
    handlerGetUserNameContactHtml :: DynamicPath -> ServerPartE Response
    handlerGetUserNameContactHtml dpath = do
      (uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      template <- getTemplate templates "user-details-form.html"
      udetails <- queryUserDetails uid
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
        void $ guardAuthorisedWhenInAnyGroup [uploadersGroup, adminGroup]
        NameAndContact name email <- expectAesonContent
        guardValidLookingName name
        guardValidLookingEmail email
        updateState userDetailsState (Acid.SetUserNameContact uid name email)
        noContent $ toResponse ()

    handlerDeleteUserNameContact :: DynamicPath -> ServerPartE Response
    handlerDeleteUserNameContact dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        guardAuthorised_ [IsUserId uid, InGroup adminGroup]
        updateState userDetailsState (Acid.SetUserNameContact uid T.empty T.empty)
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
        updateState userDetailsState (Acid.SetUserAdminInfo uid akind notes)
        noContent $ toResponse ()

    handlerDeleteAdminInfo :: DynamicPath -> ServerPartE Response
    handlerDeleteAdminInfo dpath = do
        guardAuthorised_ [InGroup adminGroup]
        uid <- lookupUserName =<< userNameInPath dpath
        updateState userDetailsState (Acid.SetUserAdminInfo uid Nothing T.empty)
        noContent $ toResponse ()
