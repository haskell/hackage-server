{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, RecursiveDo,
             BangPatterns, OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.Users (
    initUserFeature,
    UserFeature(..),
    UserResource(..),
    GroupResource(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.Templating
import qualified Distribution.Server.Framework.Auth as Auth

import Distribution.Server.Users.Types
import Distribution.Server.Users.State
import Distribution.Server.Users.Backup
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group
         (UserGroup(..), GroupDescription(..), UserIdSet, nullDescription)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Control.Applicative (optional)
import Data.Aeson (toJSON)
import Data.Aeson.TH
import qualified Data.Text as T

import Distribution.Text (display, simpleParse)


-- | A feature to allow manipulation of the database of users.
--
-- TODO: clean up mismatched and duplicate functionality (some noted below).
data UserFeature = UserFeature {
    -- | The users `HackageFeature`.
    userFeatureInterface :: HackageFeature,

    -- | User resources.
    userResource :: UserResource,

    -- | Notification that a user has been added. Currently unused.
    userAdded :: Hook () (), --TODO: delete, other status changes?
    -- | The admin user group, including its description, members, and
    -- modification thereof.
    adminGroup :: UserGroup,

    groupChangedHook :: Hook (GroupDescription, Bool, UserId, UserId, String) (),

    -- Authorisation
    -- | Require any of a set of privileges.
    guardAuthorised_   :: [PrivilegeCondition] -> ServerPartE (),
    -- | Require any of a set of privileges, giving the id of the current user.
    guardAuthorised    :: [PrivilegeCondition] -> ServerPartE UserId,
    guardAuthorised'    :: [PrivilegeCondition] -> ServerPartE Bool,
    -- | Require being logged in, giving the id of the current user.
    guardAuthenticated :: ServerPartE UserId,
    -- | Gets the authentication if it exists.
    checkAuthenticated :: ServerPartE (Maybe (UserId, UserInfo)),
    -- | A hook to override the default authentication error in particular
    -- circumstances.
    authFailHook       :: Hook Auth.AuthError (Maybe ErrorResponse),
    -- | Retrieves the entire user base.
    queryGetUserDb    :: forall m. MonadIO m => m Users.Users,

    -- | Creates a Hackage 2 user credential.
    newUserAuth       :: UserName -> PasswdPlain -> UserAuth,
    -- | Adds a user with a fresh name.
    updateAddUser     :: forall m. MonadIO m => UserName -> UserAuth -> m (Either Users.ErrUserNameClash UserId),
    -- | Sets the account-enabled status of an existing user to True or False.
    updateSetUserEnabledStatus :: forall m. MonadIO m => UserId -> Bool
                               -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser)),
    -- | Sets the credentials of an existing user.
    updateSetUserAuth :: forall m. MonadIO m => UserId -> UserAuth
                      -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser)),

    -- | Adds a user to a group based on a "user" path component.
    --
    -- Use the UserGroup or GroupResource directly instead, as this is a hack.
    groupAddUser        :: UserGroup -> DynamicPath -> ServerPartE (),
    -- | Likewise, deletes a user, will go away soon.
    groupDeleteUser     :: UserGroup -> DynamicPath -> ServerPartE (),

    -- | Get a username from a path.
    userNameInPath      :: forall m. MonadPlus m => DynamicPath -> m UserName,
    -- | Lookup a `UserId` from a name, if the name exists.
    lookupUserName      :: UserName -> ServerPartE UserId,
    -- | Lookup full `UserInfo` from a name, if the name exists.
    lookupUserNameFull  :: UserName -> ServerPartE (UserId, UserInfo),
    -- | Lookup full `UserInfo` from an id, if the id exists.
    lookupUserInfo      :: UserId -> ServerPartE UserInfo,

    -- | An action to change a password directly, using "password" and
    -- "repeat-password" form fields. Only admins and the user themselves
    -- can do this. This is messy, as it was one of the first things writen
    -- for the users feature.
    --
    -- TODO: update and make more usable.
    changePassword      :: UserName -> ServerPartE (),
    -- | Determine if the first user can change the second user's password,
    -- replicating auth functionality. Avoid using.
    canChangePassword   :: forall m. MonadIO m => UserId -> UserId -> m Bool,
    -- | Action to create a new user with the given credentials. This takes the
    -- desired name, a password, and a repeated password, validating all.
    newUserWithAuth     :: String -> PasswdPlain -> PasswdPlain -> ServerPartE UserName,
    -- | Action for an admin to create a user with "username", "password", and
    -- "repeat-password" username fields.
    adminAddUser        :: ServerPartE Response,

    -- Create a group resource for the given resource path.
    groupResourceAt     :: String -> UserGroup -> IO (UserGroup, GroupResource),
    -- | Create a parameretrized group resource for the given resource path.
    -- The parameter `a` can here be called a group key, and there is
    -- potentially a set of initial values.
    --
    -- This takes functions to create a user group on the fly for the given
    -- key, go from a key to a DynamicPath (for URI generation), as well as
    -- go from a DynamicPath to a key with some possibility of failure. This
    -- should check key membership, as well.
    --
    -- When these parameretrized `UserGroup`s need to be modified, the returned
    -- `a -> UserGroup` function should be used, as it wraps the given
    -- `a -> UserGroup` function to keep user-to-group mappings up-to-date.
    groupResourcesAt    :: forall a. String -> (a -> UserGroup)
                                            -> (a -> DynamicPath)
                                            -> (DynamicPath -> ServerPartE a)
                                            -> [a]
                                            -> IO (a -> UserGroup, GroupResource),
    -- | Look up whether the current user has (add, remove) capabilities for
    -- the given group, erroring out if neither are present.
    lookupGroupEditAuth :: UserGroup -> ServerPartE (Bool, Bool),
    -- | For a given user, return all of the URIs for groups they are in.
    getGroupIndex       :: forall m. (Functor m, MonadIO m) => UserId -> m [String],
    -- | For a given URI, get a GroupDescription for it, if one can be found.
    getIndexDesc        :: forall m. MonadIO m => String -> m GroupDescription
}

instance IsHackageFeature UserFeature where
  getFeatureInterface = userFeatureInterface

data UserResource = UserResource {
    -- | The list of all users.
    userList :: Resource,
    -- | The main page for a given user.
    userPage :: Resource,
    -- | A user's password.
    passwordResource :: Resource,
    -- | A user's enabled status.
    enabledResource  :: Resource,
    -- | The admin group.
    adminResource :: GroupResource,
    -- | Manage a user
    manageUserResource :: Resource,

    -- | URI for `userList` given a format.
    userListUri :: String -> String,
    -- | URI for `userPage` given a format and name.
    userPageUri :: String -> UserName -> String,
    -- | URI for `passwordResource` given a format and name.
    userPasswordUri :: String -> UserName -> String,
    -- | URI for `enabledResource` given a format and name.
    userEnabledUri  :: String -> UserName -> String,
    -- | URI for `adminResource` given a format.
    adminPageUri :: String -> String,
    -- | URI for `manageUserResource` give a format and name
    manageUserUri :: String -> UserName -> String
}

instance FromReqURI UserName where
  fromReqURI = simpleParse

data GroupResource = GroupResource {
    -- | A group, potentially parametetrized over some collection.
    groupResource :: Resource,
    -- | A user's presence in a group.
    groupUserResource :: Resource,
    -- | A `UserGroup` for a group, with a `DynamicPath` for any parameterization.
    getGroup :: DynamicPath -> ServerPartE UserGroup
}

-- This is a mapping of UserId -> group URI and group URI -> description.
-- Like many reverse mappings, it is probably rather volatile. Still, it is
-- a secondary concern, as user groups should be defined by each feature
-- and not globally, to be perfectly modular.
data GroupIndex = GroupIndex {
    usersToGroupUri :: !(IntMap (Set String)),
    groupUrisToDesc :: !(Map String GroupDescription)
}
emptyGroupIndex :: GroupIndex
emptyGroupIndex = GroupIndex IntMap.empty Map.empty

instance MemSize GroupIndex where
    memSize (GroupIndex a b) = memSize2 a b

-- TODO: add renaming
initUserFeature :: ServerEnv -> IO (IO UserFeature)
initUserFeature ServerEnv{serverStateDir, serverTemplatesDir, serverTemplatesMode} = do
  -- Canonical state
  usersState  <- usersStateComponent  serverStateDir
  adminsState <- adminsStateComponent serverStateDir

  -- Ephemeral state
  groupIndex   <- newMemStateWHNF emptyGroupIndex

  -- Extension hooks
  userAdded     <- newHook
  authFailHook  <- newHook
  groupChangedHook <- newHook

  -- Load templates
  templates <-
      loadTemplates serverTemplatesMode
      [serverTemplatesDir, serverTemplatesDir </> "Users"]
      [ "manage.html", "token-created.html", "token-revoked.html"
      ]

  return $ do
    -- Slightly tricky: we have an almost recursive knot between the group
    -- resource management functions, and creating the admin group
    -- resource that is part of the user feature.
    --
    -- Instead of trying to pull it apart, we just use a 'do rec'
    --
    rec let (feature@UserFeature{groupResourceAt}, adminGroupDesc)
              = userFeature templates
                            usersState
                            adminsState
                            groupIndex
                            userAdded authFailHook groupChangedHook
                            adminG adminR

        (adminG, adminR) <- groupResourceAt "/users/admins/" adminGroupDesc

    return feature

usersStateComponent :: FilePath -> IO (StateComponent AcidState Users.Users)
usersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Users") initialUsers
  return StateComponent {
      stateDesc    = "List of users"
    , stateHandle  = st
    , getState     = query st GetUserDb
    , putState     = update st . ReplaceUserDb
    , backupState  = usersBackup
    , restoreState = usersRestore
    , resetState   = usersStateComponent
    }

adminsStateComponent :: FilePath -> IO (StateComponent AcidState HackageAdmins)
adminsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageAdmins") initialHackageAdmins
  return StateComponent {
      stateDesc    = "Admins"
    , stateHandle  = st
    , getState     = query st GetHackageAdmins
    , putState     = update st . ReplaceHackageAdmins . adminList
    , backupState  = \_ (HackageAdmins admins) -> [csvToBackup ["admins.csv"] (groupToCSV admins)]
    , restoreState = HackageAdmins <$> groupBackup ["admins.csv"]
    , resetState   = adminsStateComponent
    }

userFeature :: Templates
            -> StateComponent AcidState Users.Users
            -> StateComponent AcidState HackageAdmins
            -> MemState GroupIndex
            -> Hook () ()
            -> Hook Auth.AuthError (Maybe ErrorResponse)
            -> Hook (GroupDescription, Bool, UserId, UserId, String) ()
            -> UserGroup
            -> GroupResource
            -> (UserFeature, UserGroup)
userFeature templates usersState adminsState
             groupIndex userAdded authFailHook groupChangedHook
             adminGroup adminResource
  = (UserFeature {..}, adminGroupDesc)
  where
    userFeatureInterface = (emptyHackageFeature "users") {
        featureDesc = "Manipulate the user database."
      , featureResources =
          map ($ userResource)
            [ userList
            , userPage
            , passwordResource
            , enabledResource
            , manageUserResource
            ]
          ++ [
              groupResource adminResource
            , groupUserResource adminResource
            ]
      , featureState = [
            abstractAcidStateComponent usersState
          , abstractAcidStateComponent adminsState
          ]
      , featureCaches = [
            CacheComponent {
              cacheDesc       = "user group index",
              getCacheMemSize = memSize <$> readMemState groupIndex
            }
          ]
      }

    userResource = fix $ \r -> UserResource {
        userList = (resourceAt "/users/.:format") {
            resourceDesc   = [ (GET, "list of users") ]
          , resourceGet    = [ ("json", serveUsersGet) ]
          }
      , userPage = (resourceAt "/user/:username.:format") {
            resourceDesc   = [ (GET,    "user id info")
                             , (PUT,    "create user")
                             , (DELETE, "delete user")
                             ]
          , resourceGet    = [ ("json", serveUserGet) ]
          , resourcePut    = [ ("", serveUserPut) ]
          , resourceDelete = [ ("", serveUserDelete) ]
          }
      , manageUserResource =
              (resourceAt "/user/:username/manage.:format")
              { resourceDesc =
                      [ (GET, "user management page")
                      ]
              , resourceGet  = [ ("", serveUserManagementGet) ]
              , resourcePost = [ ("", serveUserManagementPost) ]
              }
      , passwordResource = resourceAt "/user/:username/password.:format"
                           --TODO: PUT
      , enabledResource  = (resourceAt "/user/:username/enabled.:format") {
            resourceDesc = [ (GET, "return if the user is enabled")
                           , (PUT, "set if the user is enabled")
                           ]
          , resourceGet  = [("json", serveUserEnabledGet)]
          , resourcePut  = [("json", serveUserEnabledPut)]
          }

      , adminResource = adminResource

      , userListUri = \format ->
          renderResource (userList r) [format]
      , userPageUri = \format uname ->
          renderResource (userPage r) [display uname, format]
      , userPasswordUri = \format uname ->
          renderResource (passwordResource r) [display uname, format]
      , userEnabledUri  = \format uname ->
          renderResource (enabledResource  r) [display uname, format]
      , adminPageUri = \format ->
          renderResource (groupResource adminResource) [format]
      , manageUserUri = \format uname ->
          renderResource (manageUserResource r) [display uname, format]
      }

    -- Queries and updates
    --

    queryGetUserDb :: MonadIO m => m Users.Users
    queryGetUserDb = queryState usersState GetUserDb

    updateAddUser :: MonadIO m => UserName -> UserAuth -> m (Either Users.ErrUserNameClash UserId)
    updateAddUser uname auth = updateState usersState (AddUserEnabled uname auth)

    updateSetUserEnabledStatus :: MonadIO m => UserId -> Bool
                               -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser))
    updateSetUserEnabledStatus uid isenabled = updateState usersState (SetUserEnabledStatus uid isenabled)

    updateSetUserAuth :: MonadIO m => UserId -> UserAuth
                      -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser))
    updateSetUserAuth uid auth = updateState usersState (SetUserAuth uid auth)

    --
    -- Authorisation: authentication checks and privilege checks
    --

    -- High level, all in one check that the client is authenticated as a
    -- particular user and has an appropriate privilege, but then ignore the
    -- identity of the user.
    guardAuthorised_ :: [PrivilegeCondition] -> ServerPartE ()
    guardAuthorised_ = void . guardAuthorised

    -- As above but also return the identity of the client
    guardAuthorised :: [PrivilegeCondition] -> ServerPartE UserId
    guardAuthorised privconds = do
        users <- queryGetUserDb
        uid   <- guardAuthenticatedWithErrHook users
        Auth.guardPriviledged users uid privconds
        return uid

    guardAuthorised' :: [PrivilegeCondition] -> ServerPartE Bool
    guardAuthorised' privconds = do
        users <- queryGetUserDb
        uid   <- guardAuthenticatedWithErrHook users
        valid <- Auth.checkPriviledged users uid privconds
        return valid

    -- Simply check if the user is authenticated as some user, without any
    -- check that they have any particular priveledges. Only useful as a
    -- building block.
    guardAuthenticated :: ServerPartE UserId
    guardAuthenticated = do
        users   <- queryGetUserDb
        guardAuthenticatedWithErrHook users

    -- As above but using the given userdb snapshot
    guardAuthenticatedWithErrHook :: Users.Users -> ServerPartE UserId
    guardAuthenticatedWithErrHook users = do
        (uid,_) <- Auth.checkAuthenticated realm users
                   >>= either handleAuthError return
        return uid
      where
        realm = Auth.hackageRealm --TODO: should be configurable

        handleAuthError :: Auth.AuthError -> ServerPartE a
        handleAuthError err = do
          defaultResponse  <- Auth.authErrorResponse realm err
          overrideResponse <- msum <$> runHook authFailHook err
          throwError (fromMaybe defaultResponse overrideResponse)

    -- Check if there is an authenticated userid, and return info, if so.
    checkAuthenticated :: ServerPartE (Maybe (UserId, UserInfo))
    checkAuthenticated = do
        users <- queryGetUserDb
        either (const Nothing) Just `fmap` Auth.checkAuthenticated Auth.hackageRealm users

    -- | Resources representing the collection of known users.
    --
    -- Features:
    --
    -- * listing the collection of users
    -- * adding and deleting users
    -- * enabling and disabling accounts
    -- * changing user's name and password
    --

    serveUsersGet :: DynamicPath -> ServerPartE Response
    serveUsersGet _ = do
      userlist <- Users.enumerateActiveUsers <$> queryGetUserDb
      let users = [ UserNameIdResource {
                      ui_username = userName uinfo,
                      ui_userid   = uid
                    }
                  | (uid, uinfo) <- userlist ]
      return . toResponse $ toJSON users

    serveUserGet :: DynamicPath -> ServerPartE Response
    serveUserGet dpath = do
      (uid, uinfo)  <- lookupUserNameFull =<< userNameInPath dpath
      groups        <- getGroupIndex uid
      return . toResponse $
        toJSON UserInfoResource {
                 ui1_username = userName uinfo,
                 ui1_userid   = uid,
                 ui1_groups   = map T.pack groups
               }

    serveUserPut :: DynamicPath -> ServerPartE Response
    serveUserPut dpath = do
      guardAuthorised_ [InGroup adminGroup]
      username <- userNameInPath dpath
      muid     <- updateState usersState $ AddUserDisabled username
      case muid of
        Left  Users.ErrUserNameClash ->
          errBadRequest "Username already exists"
            [MText "Cannot create a new user account with that username because already exists"]
        Right uid -> return . toResponse $
          toJSON UserNameIdResource {
                   ui_username = username,
                   ui_userid   = uid
                 }

    serveUserDelete :: DynamicPath -> ServerPartE Response
    serveUserDelete dpath = do
      guardAuthorised_ [InGroup adminGroup]
      uid  <- lookupUserName =<< userNameInPath dpath
      merr <- updateState usersState $ DeleteUser uid
      case merr of
        Nothing -> noContent $ toResponse ()
        --TODO: need to be able to delete user by name to fix this race condition
        Just Users.ErrNoSuchUserId -> errInternalError [MText "uid does not exist"]

    serveUserEnabledGet :: DynamicPath -> ServerPartE Response
    serveUserEnabledGet dpath = do
      guardAuthorised_ [InGroup adminGroup]
      (_uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      let enabled = case userStatus uinfo of
                      AccountEnabled _ -> True
                      _                -> False
      return . toResponse $ toJSON EnabledResource { ui_enabled = enabled }

    serveUserEnabledPut :: DynamicPath -> ServerPartE Response
    serveUserEnabledPut dpath = do
      guardAuthorised_ [InGroup adminGroup]
      uid  <- lookupUserName =<< userNameInPath dpath
      EnabledResource enabled <- expectAesonContent
      merr <- updateState usersState (SetUserEnabledStatus uid enabled)
      case merr of
        Nothing -> noContent $ toResponse ()
        Just (Left Users.ErrNoSuchUserId) ->
          errInternalError [MText "uid does not exist"]
        Just (Right Users.ErrDeletedUser) ->
          errBadRequest "User deleted"
            [MText "Cannot disable account, it has already been deleted"]

    serveUserManagementGet :: DynamicPath -> ServerPartE Response
    serveUserManagementGet dpath = do
      (uid, uinfo)  <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      template <- getTemplate templates "manage.html"
      cacheControlWithoutETag [Private]
      ok $ toResponse $
        template
          [ "username" $= display (userName uinfo)
          , "tokens"   $=
              [ templateDict
                  [ templateVal "hash" (display authtok)
                  , templateVal "description" desc
                  ]
              | (authtok, desc) <- Map.toList (userTokens uinfo) ]
          ]

    serveUserManagementPost :: DynamicPath -> ServerPartE Response
    serveUserManagementPost dpath = do
      (uid, uinfo)  <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      cacheControlWithoutETag [Private]
      action <- look "action"
      case action of
        "new-auth-token" -> do
          desc <- T.pack <$> look "description"
          template <- getTemplate templates "token-created.html"
          origTok  <- liftIO generateOriginalToken
          let storeTok = convertToken origTok
          res <- updateState usersState (AddAuthToken uid storeTok desc)
          case res of
            Nothing ->
              ok $ toResponse $
                template
                  [ "username" $= display (userName uinfo)
                  , "token"    $= viewOriginalToken origTok
                  ]
            Just Users.ErrNoSuchUserId ->
              errInternalError [MText "uid does not exist"]

        "revoke-auth-token" -> do
          mauthToken <- parseAuthToken . T.pack <$> look "auth-token"
          template <- getTemplate templates "token-revoked.html"
          case mauthToken of
            Left err -> errBadRequest "Bad auth token"
                          [MText "The auth token provided is malformed: "
                          ,MText err]
            Right authToken -> do
              res <- updateState usersState (RevokeAuthToken uid authToken)
              case res of
                Nothing ->
                  ok $ toResponse $
                    template [ "username" $= display (userName uinfo) ]
                Just (Left Users.ErrNoSuchUserId) ->
                  errInternalError [MText "uid does not exist"]
                Just (Right Users.ErrTokenNotOwned) ->
                  errBadRequest "Invalid auth token"
                    [MText "Cannot revoke this token, no such token."]

        _ -> errBadRequest "Invalid form action" []

    --
    --  Exported utils for looking up user names in URLs\/paths
    --

    userNameInPath :: forall m. MonadPlus m => DynamicPath -> m UserName
    userNameInPath dpath = maybe mzero return (simpleParse =<< lookup "username" dpath)

    lookupUserName :: UserName -> ServerPartE UserId
    lookupUserName = fmap fst . lookupUserNameFull

    lookupUserNameFull :: UserName -> ServerPartE (UserId, UserInfo)
    lookupUserNameFull uname = do
        users <- queryState usersState GetUserDb
        case Users.lookupUserName uname users of
          Just u  -> return u
          Nothing -> userLost "Could not find user: not presently registered"
      where userLost = errNotFound "User not found" . return . MText
            --FIXME: 404 is only the right error for operating on User resources
            -- not when users are being looked up for other reasons, like setting
            -- ownership of packages. In that case needs errBadRequest

    lookupUserInfo :: UserId -> ServerPartE UserInfo
    lookupUserInfo uid = do
        users <- queryState usersState GetUserDb
        case Users.lookupUserId uid users of
          Just uinfo -> return uinfo
          Nothing    -> errInternalError [MText "user id does not exist"]

    adminAddUser :: ServerPartE Response
    adminAddUser = do
        -- with this line commented out, self-registration is allowed
        guardAuthorised_ [InGroup adminGroup]
        reqData <- getDataFn lookUserNamePasswords
        case reqData of
            (Left errs) -> errBadRequest "Error registering user"
                       ((MText "Username, password, or repeated password invalid.") : map MText errs)
            (Right (ustr, pwd1, pwd2)) -> do
                uname <- newUserWithAuth ustr (PasswdPlain pwd1) (PasswdPlain pwd2)
                seeOther ("/user/" ++ display uname) (toResponse ())
       where lookUserNamePasswords = do
                 (,,) <$> look "username"
                      <*> look "password"
                      <*> look "repeat-password"

    newUserWithAuth :: String -> PasswdPlain -> PasswdPlain -> ServerPartE UserName
    newUserWithAuth _ pwd1 pwd2 | pwd1 /= pwd2 = errBadRequest "Error registering user" [MText "Entered passwords do not match"]
    newUserWithAuth userNameStr password _ =
      case simpleParse userNameStr of
        Nothing -> errBadRequest "Error registering user" [MText "Not a valid user name!"]
        Just uname -> do
          let auth = newUserAuth uname password
          muid <- updateState usersState $ AddUserEnabled uname auth
          case muid of
            Left Users.ErrUserNameClash -> errForbidden "Error registering user" [MText "A user account with that user name already exists."]
            Right _                     -> return uname

    -- Arguments: the auth'd user id, the user path id (derived from the :username)
    canChangePassword :: MonadIO m => UserId -> UserId -> m Bool
    canChangePassword uid userPathId = do
        admins <- queryState adminsState GetAdminList
        return $ uid == userPathId || (uid `Group.member` admins)

    --FIXME: this thing is a total mess!
    -- Do admins need to change user's passwords? Why not just reset passwords & (de)activate accounts.
    changePassword :: UserName -> ServerPartE ()
    changePassword username = do
        uid <- lookupUserName username
        guardAuthorised [IsUserId uid, InGroup adminGroup]
        passwd1 <- look "password"        --TODO: fail rather than mzero if missing
        passwd2 <- look "repeat-password"
        when (passwd1 /= passwd2) $
          forbidChange "Copies of new password do not match or is an invalid password (ex: blank)"
        let passwd = PasswdPlain passwd1
            auth   = newUserAuth username passwd
        res <- updateState usersState (SetUserAuth uid auth)
        case res of
          Nothing -> return ()
          Just (Left  Users.ErrNoSuchUserId) -> errInternalError [MText "user id lookup failure"]
          Just (Right Users.ErrDeletedUser)  -> forbidChange "Cannot set passwords for deleted users"
      where
        forbidChange = errForbidden "Error changing password" . return . MText

    newUserAuth :: UserName -> PasswdPlain -> UserAuth
    newUserAuth name pwd = UserAuth (Auth.newPasswdHash Auth.hackageRealm name pwd)

    ------ User group management
    adminGroupDesc :: UserGroup
    adminGroupDesc = UserGroup {
          groupDesc             = nullDescription { groupTitle = "Hackage admins" },
          queryUserGroup        = queryState  adminsState   GetAdminList,
          addUserToGroup        = updateState adminsState . AddHackageAdmin,
          removeUserFromGroup   = updateState adminsState . RemoveHackageAdmin,
          groupsAllowedToAdd    = [adminGroupDesc],
          groupsAllowedToDelete = [adminGroupDesc]
        }

    groupAddUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupAddUser group _ = do
        actorUid <- guardAuthorised (map InGroup (groupsAllowedToAdd group))
        users <- queryState usersState GetUserDb
        muser <- optional $ look "user"
        reason <- optional $ look "reason"
        case muser of
            Nothing -> addError "Bad request (could not find 'user' argument)"
            Just ustr -> case simpleParse ustr >>= \uname -> Users.lookupUserName uname users of
                Nothing      -> addError $ "No user with name " ++ show ustr ++ " found"
                Just (uid,_) -> do
                             liftIO $ addUserToGroup group uid
                             runHook_ groupChangedHook (groupDesc group, True,actorUid,uid,fromMaybe "" reason)
       where addError = errBadRequest "Failed to add user" . return . MText

    groupDeleteUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupDeleteUser group dpath = do
      actorUid <- guardAuthorised (map InGroup (groupsAllowedToDelete group))
      uid <- lookupUserName =<< userNameInPath dpath
      reason <- localRq (\req -> req {rqMethod = POST}) . optional $ look "reason"
      liftIO $ removeUserFromGroup group uid
      runHook_ groupChangedHook (groupDesc group, False,actorUid,uid,fromMaybe "" reason)

    lookupGroupEditAuth :: UserGroup -> ServerPartE (Bool, Bool)
    lookupGroupEditAuth group = do
      addList    <- liftIO . Group.queryUserGroups $ groupsAllowedToAdd group
      removeList <- liftIO . Group.queryUserGroups $ groupsAllowedToDelete group
      uid <- guardAuthenticated
      let (canAdd, canDelete) = (uid `Group.member` addList, uid `Group.member` removeList)
      if not (canAdd || canDelete)
          then errForbidden "Forbidden" [MText "Can't edit permissions for user group"]
          else return (canAdd, canDelete)

    ------------ Encapsulation of resources related to editing a user group.

    -- | Registers a user group for external display. It takes the index group
    -- mapping (groupIndex from UserFeature), the base uri of the group, and a
    -- UserGroup object with all the necessary hooks. The base uri shouldn't
    -- contain any dynamic or varying components. It returns the GroupResource
    -- object, and also an adapted UserGroup that updates the cache. You should
    -- use this in order to keep the index updated.
    groupResourceAt :: String -> UserGroup -> IO (UserGroup, GroupResource)
    groupResourceAt uri group = do
        let mainr = resourceAt uri
            descr = groupDesc group
            groupUri = renderResource mainr []
            group' = group
              { addUserToGroup = \uid -> do
                    addGroupIndex uid groupUri descr
                    addUserToGroup group uid
              , removeUserFromGroup = \uid -> do
                    removeGroupIndex uid groupUri
                    removeUserFromGroup group uid
              }
        ulist <- queryUserGroup group
        initGroupIndex ulist groupUri descr
        let groupr = GroupResource {
                groupResource = (extendResourcePath "/.:format" mainr) {
                    resourceDesc = [ (GET, "Description of the group and a list of its members (defined in 'users' feature)") ]
                  , resourceGet  = [ ("json", serveUserGroupGet groupr) ]
                  }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                    resourceDesc   = [ (PUT, "Add a user to the group (defined in 'users' feature)")
                                     , (DELETE, "Remove a user from the group (defined in 'users' feature)")
                                     ]
                  , resourcePut    = [ ("", serveUserGroupUserPut groupr) ]
                  , resourceDelete = [ ("", serveUserGroupUserDelete groupr) ]
                  }
              , getGroup = \_ -> return group'
              }
        return (group', groupr)

    -- | Registers a collection of user groups for external display. These groups
    -- are usually backing a separate collection. Like groupResourceAt, it takes the
    -- index group mapping and a base uri The base uri can contain varying path
    -- components, so there should be a group-generating function that, given a
    -- DynamicPath, yields the proper UserGroup. The final argument is the initial
    -- list of DynamicPaths to build the initial group index. Like groupResourceAt,
    -- this function returns an adaptor function that keeps the index updated.
    groupResourcesAt :: String
                     -> (a -> UserGroup)
                     -> (a -> DynamicPath)
                     -> (DynamicPath -> ServerPartE a)
                     -> [a]
                     -> IO (a -> UserGroup, GroupResource)
    groupResourcesAt uri mkGroup mkPath getGroupData initialGroupData = do
        let mainr = resourceAt uri
        sequence_
          [ do let group = mkGroup x
                   dpath = mkPath x
               ulist <- queryUserGroup group
               initGroupIndex ulist (renderResource' mainr dpath) (groupDesc group)
          | x <- initialGroupData ]

        let mkGroup' x =
              let group = mkGroup x
                  dpath = mkPath x
               in group {
                    addUserToGroup = \uid -> do
                        addGroupIndex uid (renderResource' mainr dpath) (groupDesc group)
                        addUserToGroup group uid
                  , removeUserFromGroup = \uid -> do
                        removeGroupIndex uid (renderResource' mainr dpath)
                        removeUserFromGroup group uid
                  }

            groupr = GroupResource {
                groupResource = (extendResourcePath "/.:format" mainr) {
                    resourceDesc = [ (GET, "Description of the group and a list of the members (defined in 'users' feature)") ]
                  , resourceGet  = [ ("json", serveUserGroupGet groupr) ]
                  }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                    resourceDesc   = [ (PUT,    "Add a user to the group (defined in 'users' feature)")
                                     , (DELETE, "Delete a user from the group (defined in 'users' feature)")
                                     ]
                  , resourcePut    = [ ("", serveUserGroupUserPut groupr) ]
                  , resourceDelete = [ ("", serveUserGroupUserDelete groupr) ]
                  }
              , getGroup = \dpath -> mkGroup' <$> getGroupData dpath
              }
        return (mkGroup', groupr)

    serveUserGroupGet groupr dpath = do
      group    <- getGroup groupr dpath
      userDb   <- queryGetUserDb
      userlist <- liftIO $ queryUserGroup group
      return . toResponse $ toJSON
          UserGroupResource {
            ui_title       = T.pack $ groupTitle (groupDesc group),
            ui_description = T.pack $ groupPrologue (groupDesc group),
            ui_members     = [ UserNameIdResource {
                                 ui_username = Users.userIdToName userDb uid,
                                 ui_userid   = uid
                               }
                             | uid <- Group.toList userlist ]
          }

    --TODO: add serveUserGroupUserPost for the sake of the html frontend
    --      and then remove groupAddUser & groupDeleteUser
    serveUserGroupUserPut groupr dpath = do
      group <- getGroup groupr dpath
      actorUid <- guardAuthorised (map InGroup (groupsAllowedToAdd group))
      uid <- lookupUserName =<< userNameInPath dpath
      reason <- optional $ look "reason"
      liftIO $ addUserToGroup group uid
      runHook_ groupChangedHook (groupDesc group, True,actorUid,uid,fromMaybe "" reason)
      goToList groupr dpath

    serveUserGroupUserDelete groupr dpath = do
      group <- getGroup groupr dpath
      actorUid <- guardAuthorised (map InGroup (groupsAllowedToDelete group))
      uid <- lookupUserName =<< userNameInPath dpath
      reason <- optional $ look "reason"
      liftIO $ removeUserFromGroup group uid
      runHook_ groupChangedHook (groupDesc group, False,actorUid,uid,fromMaybe "" reason)
      goToList groupr dpath

    goToList group dpath = seeOther (renderResource' (groupResource group) dpath)
                                    (toResponse ())

    ---------------------------------------------------------------
    addGroupIndex :: MonadIO m => UserId -> String -> GroupDescription -> m ()
    addGroupIndex (UserId uid) uri desc =
        modifyMemState groupIndex $
          adjustGroupIndex
            (IntMap.insertWith Set.union uid (Set.singleton uri))
            (Map.insert uri desc)

    removeGroupIndex :: MonadIO m => UserId -> String -> m ()
    removeGroupIndex (UserId uid) uri =
        modifyMemState groupIndex $
          adjustGroupIndex
            (IntMap.update (keepSet . Set.delete uri) uid)
            id
      where
        keepSet m = if Set.null m then Nothing else Just m

    initGroupIndex :: MonadIO m => UserIdSet -> String -> GroupDescription -> m ()
    initGroupIndex ulist uri desc =
        modifyMemState groupIndex $
          adjustGroupIndex
            (IntMap.unionWith Set.union (IntMap.fromList . map mkEntry $ Group.toList ulist))
            (Map.insert uri desc)
      where
        mkEntry (UserId uid) = (uid, Set.singleton uri)

    getGroupIndex :: (Functor m, MonadIO m) => UserId -> m [String]
    getGroupIndex (UserId uid) =
      liftM (maybe [] Set.toList . IntMap.lookup uid . usersToGroupUri) $ readMemState groupIndex

    getIndexDesc :: MonadIO m => String -> m GroupDescription
    getIndexDesc uri =
      liftM (Map.findWithDefault nullDescription uri . groupUrisToDesc) $ readMemState groupIndex

    -- partitioning index modifications, a cheap combinator
    adjustGroupIndex :: (IntMap (Set String) -> IntMap (Set String))
                     -> (Map String GroupDescription -> Map String GroupDescription)
                     -> GroupIndex -> GroupIndex
    adjustGroupIndex f g (GroupIndex a b) = GroupIndex (f a) (g b)


{------------------------------------------------------------------------------
  Some types for JSON resources
------------------------------------------------------------------------------}

data UserNameIdResource = UserNameIdResource { ui_username    :: UserName,
                                               ui_userid      :: UserId }
data UserInfoResource   = UserInfoResource   { ui1_username    :: UserName,
                                               ui1_userid      :: UserId,
                                               ui1_groups      :: [T.Text] }
data EnabledResource    = EnabledResource    { ui_enabled     :: Bool }
data UserGroupResource  = UserGroupResource  { ui_title       :: T.Text,
                                               ui_description :: T.Text,
                                               ui_members     :: [UserNameIdResource] }

deriveJSON (compatAesonOptionsDropPrefix "ui_")  ''UserNameIdResource
deriveJSON (compatAesonOptionsDropPrefix "ui1_") ''UserInfoResource
deriveJSON (compatAesonOptionsDropPrefix "ui_")  ''EnabledResource
deriveJSON (compatAesonOptionsDropPrefix "ui_")  ''UserGroupResource
