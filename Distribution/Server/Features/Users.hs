{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.Users (
    initUserFeature,
    UserFeature(..),
    UserResource(..),

    GroupResource(..),
    GroupGen,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import qualified Distribution.Server.Framework.Auth as Auth
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Users.Types
import Distribution.Server.Users.State
import Distribution.Server.Users.Backup
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), UserList, nullDescription)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Function (fix)
import Control.Applicative (optional)
import Text.JSON
         ( JSValue(..), toJSObject, toJSString )

import Distribution.Text (display, simpleParse)


-- | A feature to allow manipulation of the database of users.
--
data UserFeature = UserFeature {
    userFeatureInterface :: HackageFeature,

    userResource :: UserResource,

    userAdded :: Hook (IO ()),
    adminGroup :: UserGroup,
    -- Filters for all features modifying the package index
    packageMutate :: Filter (UserId -> IO Bool),

    -- Authorisation
    guardAuthorised_   :: [PrivilegeCondition] -> ServerPartE (),
    guardAuthorised    :: [PrivilegeCondition] -> ServerPartE UserId,
    guardAuthenticated :: ServerPartE UserId,
    authFailHook       :: Hook (Auth.AuthError -> IO (Maybe Response)),

    queryGetUserDb    :: forall m. MonadIO m => m Users.Users,

    newUserAuth       :: UserName -> PasswdPlain -> UserAuth,
    updateAddUser     :: forall m. MonadIO m => UserName -> UserAuth -> m (Either Users.ErrUserNameClash UserId),
    updateSetUserEnabledStatus :: MonadIO m => UserId -> Bool
                               -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser)),
    updateSetUserAuth :: MonadIO m => UserId -> UserAuth
                      -> m (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser)),

    groupAddUser        :: UserGroup -> DynamicPath -> ServerPartE (),
    groupDeleteUser     :: UserGroup -> DynamicPath -> ServerPartE (),

    userNameInPath      :: forall m. MonadPlus m => DynamicPath -> m UserName,
    lookupUserName      :: UserName -> ServerPartE UserId,
    lookupUserNameFull  :: UserName -> ServerPartE (UserId, UserInfo),
    lookupUserInfo      :: UserId -> ServerPartE UserInfo,

    changePassword      :: UserName -> ServerPartE (),
    canChangePassword   :: forall m. MonadIO m => UserId -> UserId -> m Bool,
    newUserWithAuth     :: String -> PasswdPlain -> PasswdPlain -> ServerPartE UserName,
    adminAddUser        :: ServerPartE Response,
    enabledAccount      :: UserName -> ServerPartE (),
    deleteAccount       :: UserName -> ServerPartE (),
    runUserFilter       :: UserId -> IO (Maybe ErrorResponse),
    groupResourceAt     :: String -> UserGroup -> IO (UserGroup, GroupResource),
    groupResourcesAt    :: String -> GroupGen -> [DynamicPath] -> IO (GroupGen, GroupResource),
    lookupGroupEditAuth :: UserGroup -> ServerPartE (Bool, Bool),
    getGroupIndex       :: forall m. (Functor m, MonadIO m) => UserId -> m [String],
    getIndexDesc        :: forall m. MonadIO m => String -> m GroupDescription
}

instance IsHackageFeature UserFeature where
  getFeatureInterface = userFeatureInterface

data UserResource = UserResource {
    userList :: Resource,
    userPage :: Resource,
    passwordResource :: Resource,
    enabledResource  :: Resource,
    adminResource :: GroupResource,

    userListUri :: String -> String,
    userPageUri :: String -> UserName -> String,
    userPasswordUri :: String -> UserName -> String,
    userEnabledUri  :: String -> UserName -> String,
    adminPageUri :: String -> String
}

instance FromReqURI UserName where
  fromReqURI = simpleParse

data GroupResource = GroupResource {
    groupResource :: Resource,
    groupUserResource :: Resource,
    getGroup :: GroupGen
}

type GroupGen = DynamicPath -> UserGroup

-- This is a mapping of UserId -> group URI and group URI -> description.
-- Like many reverse mappings, it is probably rather volatile. Still, it is
-- a secondary concern, as user groups should be defined by each feature
-- and not global, to be perfectly modular.
data GroupIndex = GroupIndex {
    usersToGroupUri :: !(IntMap (Set String)),
    groupUrisToDesc :: !(Map String GroupDescription)
}
emptyGroupIndex :: GroupIndex
emptyGroupIndex = GroupIndex IntMap.empty Map.empty

instance MemSize GroupIndex where
    memSize (GroupIndex a b) = memSize2 a b

-- TODO: add renaming
initUserFeature :: ServerEnv -> IO UserFeature
initUserFeature ServerEnv{serverStateDir} = do

  -- Canonical state
  usersState  <- usersStateComponent  serverStateDir
  adminsState <- adminsStateComponent serverStateDir

  -- Ephemeral state
  groupIndex   <- newMemStateWHNF emptyGroupIndex

  -- Extension hooks
  userAdded     <- newHook
  packageMutate <- newHook
  authFailHook  <- newHook

  -- Slightly tricky: we have an almost recursive knot between the group
  -- resource management functions, and creating the admin group
  -- resource that is part of the user feature.
  --
  -- Instead of trying to pull it apart, we just use a 'do rec'
  --
  rec let (feature@UserFeature{groupResourceAt}, adminGroupDesc)
            = userFeature usersState
                          adminsState
                          groupIndex
                          userAdded packageMutate authFailHook
                          adminG adminR

      (adminG, adminR) <- groupResourceAt "/users/admins/" adminGroupDesc

  return feature

usersStateComponent :: FilePath -> IO (StateComponent Users.Users)
usersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Users") initialUsers
  return StateComponent {
      stateDesc    = "List of users"
    , acidState    = st
    , getState     = query st GetUserDb
    , putState     = update st . ReplaceUserDb
    , backupState  = \users -> [csvToBackup ["users.csv"] (usersToCSV users)]
    , restoreState = userBackup
    , resetState   = usersStateComponent
    }

adminsStateComponent :: FilePath -> IO (StateComponent HackageAdmins)
adminsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageAdmins") initialHackageAdmins
  return StateComponent {
      stateDesc    = "Admins"
    , acidState    = st
    , getState     = query st GetHackageAdmins
    , putState     = update st . ReplaceHackageAdmins . adminList
    , backupState  = \(HackageAdmins admins) -> [csvToBackup ["admins.csv"] (groupToCSV admins)]
    , restoreState = HackageAdmins <$> groupBackup ["admins.csv"]
    , resetState   = adminsStateComponent
    }

userFeature :: StateComponent Users.Users
            -> StateComponent HackageAdmins
            -> MemState GroupIndex
            -> Hook (IO ())
            -> Filter (UserId -> IO Bool)
            -> Filter (Auth.AuthError -> IO (Maybe Response))
            -> UserGroup
            -> GroupResource
            -> (UserFeature, UserGroup)
userFeature  usersState adminsState
             groupIndex userAdded packageMutate authFailHook
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
            ]
          ++ [
              groupResource adminResource
            , groupUserResource adminResource
            ]
      , featureState = [
            abstractStateComponent usersState
          , abstractStateComponent adminsState
          ]
      , featureCaches = [
            CacheComponent {
              cacheDesc       = "user group index",
              getCacheMemSize = memSize <$> readMemState groupIndex
            }
          ]
      }

    userResource = fix $ \r -> UserResource {
        userList = (resourceAt "/users/.:format")
      , userPage = (resourceAt "/user/:username.:format") {
            resourceDesc   = [ (PUT,    "create user")
                             , (DELETE, "delete user")
                             ]
          , resourcePut    = [ ("", handleUserPut) ]
          , resourceDelete = [ ("", handleUserDelete) ]
          }
      , passwordResource = resourceAt "/user/:username/password.:format"
                           --TODO: PUT
      , enabledResource  = resourceAt "/user/:username/enabled.:format"
                           --TODO: GET & PUT
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
      }


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
    guardAuthorised_ :: [PrivilegeCondition] -> ServerPartE ()
    guardAuthorised_ = void . guardAuthorised

    guardAuthorised :: [PrivilegeCondition] -> ServerPartE UserId
    guardAuthorised privconds = do
        users    <- queryGetUserDb
        (uid, _) <- Auth.checkAuthenticated realm users
                >>= either handleAuthError return
        Auth.guardPriviledged users uid privconds
        return uid
      where
        realm = Auth.hackageRealm --TODO: should be configurable

        handleAuthError :: Auth.AuthError -> ServerPartE a
        handleAuthError err = do
          defaultResponse  <- Auth.authErrorResponse realm err
          overrideResponse <- msum <$> runFilter' authFailHook err
          finishWith $! fromMaybe defaultResponse overrideResponse

    guardAuthenticated :: ServerPartE UserId
    guardAuthenticated = do
        users   <- queryGetUserDb
        (uid,_) <- Auth.guardAuthenticated Auth.hackageRealm users
        return uid

    -- result: either not-found, not-authenticated, or 204 (success)
    deleteAccount :: UserName -> ServerPartE ()
    deleteAccount uname = do
      void $ guardAuthorised [InGroup adminGroup]
      uid <- lookupUserName uname
      void $ updateState usersState (DeleteUser uid)

    -- result: not-found, not authenticated, or ok (success)
    enabledAccount :: UserName -> ServerPartE ()
    enabledAccount uname = do
      _        <- guardAuthorised [InGroup adminGroup]
      uid      <- lookupUserName uname
      enabled  <- optional $ look "enabled"
      -- for a checkbox, presence in data string means 'checked'
      let isenabled = maybe False (const True) enabled
      void $ updateState usersState (SetUserEnabledStatus uid isenabled)

    handleUserPut :: DynamicPath -> ServerPartE Response
    handleUserPut dpath = do
      _        <- guardAuthorised [InGroup adminGroup]
      username <- userNameInPath dpath
      muid     <- updateState usersState $ AddUserDisabled username
      case muid of
        -- the only possible error is that the user exists already
        -- but that's ok too
        Left  _ -> noContent $ toResponse ()
        Right _ -> noContent $ toResponse ()

    handleUserDelete :: DynamicPath -> ServerPartE Response
    handleUserDelete dpath = do
      _    <- guardAuthorised [InGroup adminGroup]
      uid  <- lookupUserName =<< userNameInPath dpath
      merr <- updateState usersState $ DeleteUser uid
      case merr of
        Nothing   -> noContent $ toResponse ()
        --TODO: need to be able to delete user by name to fix this race condition
        Just _err -> errInternalError [MText "uid does not exist (but lookup was sucessful)"]


    -- | Resources representing the collection of known users.
    --
    -- Features:
    --
    -- * listing the collection of users
    -- * adding and deleting users
    -- * enabling and disabling accounts
    -- * changing user's name and password
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

    --
    runUserFilter :: UserId -> IO (Maybe ErrorResponse)
    runUserFilter uid =
      runFilter' packageMutate uid >>= \bs -> case or bs of
        True  -> return . Just $ ErrorResponse 403 "Upload failed" [MText "Your account can't upload packages."]
        False -> return Nothing

    ------ User group management
    adminGroupDesc :: UserGroup
    adminGroupDesc = UserGroup {
          groupDesc      = nullDescription { groupTitle = "Hackage admins" },
          queryUserList  = queryState adminsState GetAdminList,
          addUserList    = updateState adminsState . AddHackageAdmin,
          removeUserList = updateState adminsState . RemoveHackageAdmin,
          groupExists    = return True,
          canAddGroup    = [adminGroupDesc],
          canRemoveGroup = [adminGroupDesc]
        }

    groupAddUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupAddUser group _ = do
        guardAuthorised_ (map InGroup (canAddGroup group))
        users <- queryState usersState GetUserDb
        muser <- optional $ look "user"
        case muser of
            Nothing -> addError "Bad request (could not find 'user' argument)"
            Just ustr -> case simpleParse ustr >>= \uname -> Users.lookupUserName uname users of
                Nothing      -> addError $ "No user with name " ++ show ustr ++ " found"
                Just (uid,_) -> liftIO $ addUserList group uid
       where addError = errBadRequest "Failed to add user" . return . MText

    groupDeleteUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupDeleteUser group dpath = do
      guardAuthorised_ (map InGroup (canRemoveGroup group))
      uid <- lookupUserName =<< userNameInPath dpath
      liftIO $ removeUserList group uid

    lookupGroupEditAuth :: UserGroup -> ServerPartE (Bool, Bool)
    lookupGroupEditAuth group = do
      addList    <- liftIO . Group.queryGroups $ canAddGroup group
      removeList <- liftIO . Group.queryGroups $ canRemoveGroup group
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
              { addUserList = \uid -> do
                    addGroupIndex uid groupUri descr
                    addUserList group uid
              , removeUserList = \uid -> do
                    removeGroupIndex uid groupUri
                    removeUserList group uid
              }
        ulist <- queryUserList group
        initGroupIndex ulist groupUri descr
        let groupr = GroupResource {
                groupResource = (extendResourcePath "/.:format" mainr) {
                    resourceDesc = [ (GET, "Description of the group and a list of its members (defined in 'users' feature)") ]
                  , resourceGet  = [ ("json", handleUserGroupGet group') ]
                  }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                    resourceDesc   = [ (PUT, "Add a user to the group (defined in 'users' feature)")
                                     , (DELETE, "Remove a user from the group (defined in 'users' feature)")
                                     ]
                  , resourcePut    = [ ("", handleUserGroupUserPut group groupr) ]
                  , resourceDelete = [ ("", handleUserGroupUserDelete group groupr) ]
                  }
              , getGroup = \_ -> group'
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
                     -> GroupGen -> [DynamicPath]
                     -> IO (GroupGen, GroupResource)
    groupResourcesAt uri groupGen dpaths = do
        let mainr = resourceAt uri
            collectUserList genpath = do
                let group = groupGen genpath
                exists <- groupExists group
                case exists of
                    False -> return ()
                    True  -> queryUserList group >>= \ulist ->
                        initGroupIndex ulist (renderResource' mainr genpath) (groupDesc group)
            getGroupFunc dpath =
                let group = groupGen dpath
                in  group
                  { addUserList = \uid -> do
                        addGroupIndex uid (renderResource' mainr dpath) (groupDesc group)
                        addUserList group uid
                  , removeUserList = \uid -> do
                        removeGroupIndex uid (renderResource' mainr dpath)
                        removeUserList group uid
                  }
        mapM_ collectUserList dpaths
        let groupr = GroupResource {
                groupResource = (extendResourcePath "/.:format" mainr) {
                    resourceDesc = [ (GET, "Description of the group and a list of the members (defined in 'users' feature)") ]
                  , resourceGet  = [ ("json", \dpath -> handleUserGroupGet (getGroupFunc dpath) dpath) ]
                  }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                    resourceDesc   = [ (PUT,    "Add a user to the group (defined in 'users' feature)")
                                     , (DELETE, "Delete a user from the group (defined in 'users' feature)")
                                     ]
                  , resourcePut    = [ ("", \dpath -> handleUserGroupUserPut    (getGroupFunc dpath) groupr dpath) ]
                  , resourceDelete = [ ("", \dpath -> handleUserGroupUserDelete (getGroupFunc dpath) groupr dpath) ]
                  }
              , getGroup = getGroupFunc
              }
        return (getGroupFunc, groupr)

    handleUserGroupGet group _dpath = do
      guardGroupExists group
      userDb   <- queryGetUserDb
      userlist <- liftIO $ queryUserList group
      let unames = [ Users.userIdToName userDb uid
                   | uid <- Group.enumerate userlist ]
      return . toResponse . Resource.JSON $
          JSObject $ toJSObject [
            ("title",       JSString $ toJSString $ groupTitle $ groupDesc group)
          , ("description", JSString $ toJSString $ groupPrologue $ groupDesc group)
          , ("members",     JSArray [ JSString $ toJSString $ display uname
                                    | uname <- unames ])
          ]

    --TODO: add handleUserGroupUserPost for the sake of the html frontend
    --      and then remove groupAddUser & groupDeleteUser
    handleUserGroupUserPut group groupr dpath = do
      guardGroupExists group
      guardAuthorised_ (map InGroup (canAddGroup group))
      uid <- lookupUserName =<< userNameInPath dpath
      liftIO $ addUserList group uid
      goToList groupr dpath

    handleUserGroupUserDelete group groupr dpath = do
      guardGroupExists group
      guardAuthorised_ (map InGroup (canRemoveGroup group))
      uid <- lookupUserName =<< userNameInPath dpath
      liftIO $ removeUserList group uid
      goToList groupr dpath

    goToList group dpath = seeOther (renderResource' (groupResource group) dpath)
                                    (toResponse ())

    guardGroupExists group = do
      exists <- liftIO (groupExists group)
      unless exists $ errNotFound "User group doesn't exist" [MText "User group doesn't exist"]

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

    initGroupIndex :: MonadIO m => UserList -> String -> GroupDescription -> m ()
    initGroupIndex ulist uri desc =
        modifyMemState groupIndex $
          adjustGroupIndex
            (IntMap.unionWith Set.union (IntMap.fromList . map mkEntry $ Group.enumerate ulist))
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
