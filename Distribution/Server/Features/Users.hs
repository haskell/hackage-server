{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec #-}
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
import qualified Distribution.Server.Framework.Cache as Cache
import qualified Distribution.Server.Framework.ResourceTypes as Resource

import Distribution.Server.Users.Types
import Distribution.Server.Users.State as State
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
import qualified Data.ByteString.Lazy.Char8 as LBS
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

    queryGetUserDb    :: forall m. MonadIO m => m Users.Users,

    updateAddUser     :: forall m. MonadIO m => UserName -> UserAuth -> m (Either String UserId),
    updateRequireUserName :: forall m. MonadIO m => UserName -> m UserId,

    groupAddUser      :: UserGroup -> DynamicPath -> ServerPartE (),
    groupDeleteUser   :: UserGroup -> DynamicPath -> ServerPartE (),

    withUserNamePath  :: forall a. DynamicPath -> (UserName -> ServerPartE a) -> ServerPartE a,
    withUserPath      :: forall a. DynamicPath -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a,
    withUserName      :: forall a. UserName -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a,
    changePassword    :: UserName -> ServerPartE (),
    canChangePassword :: forall m. MonadIO m => UserId -> UserId -> m Bool,
    newUserWithAuth   :: String -> PasswdPlain -> PasswdPlain -> ServerPartE UserName,
    adminAddUser      :: ServerPartE Response,
    enabledAccount    :: UserName -> ServerPartE (),
    deleteAccount     :: UserName -> ServerPartE (),
    runUserFilter     :: UserId -> IO (Maybe ErrorResponse),
    groupResourceAt   :: String -> UserGroup -> IO (UserGroup, GroupResource),
    groupResourcesAt  :: String -> GroupGen -> [DynamicPath] -> IO (GroupGen, GroupResource),
    withGroupEditAuth :: forall a. UserGroup -> (Bool -> Bool -> ServerPartE a) -> ServerPartE a,
    getGroupIndex     :: forall m. (Functor m, MonadIO m) => UserId -> m [String],
    getIndexDesc      :: forall m. MonadIO m => String -> m GroupDescription
}

instance IsHackageFeature UserFeature where
  getFeatureInterface = userFeatureInterface

data UserResource = UserResource {
    userList :: Resource,
    userPage :: Resource,
    passwordResource :: Resource,
    htpasswordResource :: Resource,
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

data ChangePassword = ChangePassword { first :: String, second :: String, tryUpgrade :: Bool } deriving (Eq, Show)
instance FromData ChangePassword where
        fromData = liftM3 ChangePassword (look "password" `mplus` return "")
                                   (look "repeat-password" `mplus` return "")
                                   (liftM (const True) (look "try-upgrade") `mplus` return False)

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
    usersToGroupUri :: IntMap (Set String),
    groupUrisToDesc :: Map String GroupDescription
}
emptyGroupIndex :: GroupIndex
emptyGroupIndex = GroupIndex IntMap.empty Map.empty


-- TODO: add renaming
initUserFeature :: ServerEnv -> IO UserFeature
initUserFeature ServerEnv{serverStateDir} = do

  -- Canonical state
  usersState  <- openLocalStateFrom
                   (serverStateDir </> "db" </> "Users")
                   initialUsers
  adminsState <- openLocalStateFrom
                   (serverStateDir </> "db" </> "HackageAdmins")
                   initialHackageAdmins

  -- Cached state
  groupIndex   <- Cache.newCache emptyGroupIndex id

  -- Extension hooks
  userAdded     <- newHook
  packageMutate <- newHook

  -- Slightly tricky: we have an almost recursive knot between the group
  -- resource management functions, and creating the admin group
  -- resource that is part of the user feature.
  --
  -- Instead of trying to pull it apart, we just use a 'do rec'
  --
  rec let (feature@UserFeature{groupResourceAt}, adminGroupDesc)
            = userFeature usersState adminsState
                          groupIndex userAdded packageMutate
                          adminG adminR

      (adminG, adminR) <- groupResourceAt "/users/admins/" adminGroupDesc

  return feature

userFeature :: AcidState Users.Users
            -> AcidState HackageAdmins
            -> Cache.Cache GroupIndex
            -> Hook (IO ())
            -> Filter (UserId -> IO Bool)
            -> UserGroup
            -> GroupResource
            -> (UserFeature, UserGroup)
userFeature  usersState adminsState
             groupIndex userAdded packageMutate
             adminGroup adminResource
  = (UserFeature {..}, adminGroupDesc)
  where
    userFeatureInterface = (emptyHackageFeature "users") {
        featureDesc = "Manipulate the user database."
      , featureResources = map ($ userResource)
            [userList, userPage, passwordResource, htpasswordResource, enabledResource]
            ++ [groupResource adminResource, groupUserResource adminResource]
      , featureDumpRestore = Just (dumpBackup, restoreBackup, testRoundtrip)
      , featureCheckpoint = do
          createCheckpoint usersState
          createCheckpoint adminsState
      , featureShutdown = do
          closeAcidState usersState
          closeAcidState adminsState
      }

    userResource = fix $ \r -> UserResource {
        userList = (resourceAt "/users/.:format")
      , userPage = (resourceAt "/user/:username.:format") {
            resourceDesc   = [ "PUT: create user; DELETE: delete user." ]
          , resourcePut    = [ ("", handleUserPut) ]
          , resourceDelete = [ ("", handleUserDelete) ]
          }
      , passwordResource = resourceAt "/user/:username/password.:format"
      , htpasswordResource = (resourceAt "/user/:username/htpasswd") {
            resourcePut = [ ("", handleUserHtpasswdPut) ]
          }
      , enabledResource = (resourceAt "/user/:username/enabled.:format")
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

    dumpBackup = do
      users    <- query usersState  GetUserDb
      admins   <- query adminsState GetHackageAdmins
      return [ csvToBackup ["users.csv"]  (usersToCSV users)
             , csvToBackup ["admins.csv"] (groupToCSV admins) ]

    restoreBackup =
      mconcat [ userBackup  usersState
              , groupBackup adminsState ["admins.csv"] ReplaceHackageAdmins ]

    testRoundtrip = testRoundtripByQuery ((,) <$> query usersState  GetUserDb
                                              <*> query adminsState GetHackageAdmins)

    queryGetUserDb :: MonadIO m => m Users.Users
    queryGetUserDb = query' usersState GetUserDb

    updateRequireUserName :: MonadIO m => UserName -> m UserId
    updateRequireUserName uname = update' usersState (RequireUserName uname)

    updateAddUser :: MonadIO m => UserName -> UserAuth -> m (Either String UserId)
    updateAddUser uname auth = update' usersState (AddUser uname auth)

    {- result: see-other for user page or authentication error
    requireAuth :: ServerPartE Response
    requireAuth = do
        users <- query' usersState GetUserDb
        (_, info) <- Auth.guardAuthenticated hackageRealm users
        fmap Right $ seeOther ("/user/" ++ display (userName info)) $ toResponse ()
    -}

    -- result: either not-found, not-authenticated, or 204 (success)
    deleteAccount :: UserName -> ServerPartE ()
    deleteAccount uname =
      withUserName  uname $ \uid _ -> do
        users  <- query' usersState GetUserDb
        admins <- query' adminsState State.GetHackageAdmins
        void $ guardAuthorised hackageRealm users admins
        void $ update' usersState (DeleteUser uid)

    -- result: not-found, not authenticated, or ok (success)
    enabledAccount :: UserName -> ServerPartE ()
    enabledAccount uname =
      withUserName uname $ \uid _ -> do
        users  <- query' usersState GetUserDb
        admins <- query' adminsState GetHackageAdmins
        void $ guardAuthorised hackageRealm users admins
        enabled <- optional $ look "enabled"
        -- for a checkbox, prescence in data string means 'checked'
        void $ case enabled of
               Nothing -> update' usersState (SetEnabledUser uid False)
               Just _  -> update' usersState (SetEnabledUser uid True)

    handleUserPut :: DynamicPath -> ServerPart Response
    handleUserPut dpath =
        runServerPartE $ do
          users  <- query' usersState GetUserDb
          admins <- query' adminsState GetHackageAdmins
          _ <- guardAuthorised hackageRealm users admins
          withUserNamePath dpath $ \username -> do
            muid <- update' usersState $ AddUser username NoUserAuth
            case muid of
              -- the only possible error is that the user exists already
              -- but that's ok too
              Left  _err -> noContent $ toResponse ()
              Right _    -> noContent $ toResponse ()


    handleUserDelete :: DynamicPath -> ServerPart Response
    handleUserDelete dpath =
        runServerPartE $ do
          users  <- query' usersState GetUserDb
          admins <- query' adminsState GetHackageAdmins
          _ <- guardAuthorised hackageRealm users admins
          withUserPath dpath $ \uid _uinfo -> do
            merr <- update' usersState $ DeleteUser uid
            case merr of
              Nothing   -> noContent $ toResponse ()
              --TODO: need to be able to delete user by name to fix this race condition
              Just _err -> errInternalError [MText "uid does not exist (but lookup was sucessful)"]

    handleUserHtpasswdPut :: DynamicPath -> ServerPart Response
    handleUserHtpasswdPut dpath =
        runServerPartE $ do
          users  <- query' usersState GetUserDb
          admins <- query' adminsState GetHackageAdmins
          _admin <- guardAuthorised hackageRealm users admins
          withUserPath dpath $ \uid _ -> do
            expectTextPlain
            Body htpasswd <- consumeRequestBody
            if validHtpasswd htpasswd
              then do let auth = OldUserAuth (HtPasswdHash (LBS.unpack htpasswd))
                      update' usersState $ ReplaceUserAuth uid auth
                      noContent $ toResponse ()
              else badRequest (toResponse "invalid htpasswd hash")
      where
        validHtpasswd str = LBS.length str == 13

    -- | Resources representing the collection of known users.
    --
    -- Features:
    --
    -- * listing the collection of users
    -- * adding and deleting users
    -- * enabling and disabling accounts
    -- * changing user's name and password
    --

    withUserNamePath :: DynamicPath -> (UserName -> ServerPartE a) -> ServerPartE a
    withUserNamePath dpath =
      require (return $ simpleParse =<< lookup "username" dpath)

    withUserName :: UserName -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
    withUserName uname func = do
        users <- query' usersState GetUserDb
        case Users.lookupName uname users of
          Nothing  -> userLost "Could not find user: not presently registered"
          Just uid -> case Users.lookupId uid users of
            Nothing   -> userLost "Could not find user: internal server error"
            Just info -> func uid info
      where userLost = errNotFound "User not found" . return . MText

    withUserPath :: DynamicPath -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
    withUserPath dpath func =
      withUserNamePath dpath $ \name ->
        withUserName name func

    adminAddUser :: ServerPartE Response
    adminAddUser = do
        -- with these lines commented out, self-registration is allowed
      --admins <- query State.GetHackageAdmins
      --users <- query State.GetUserDb
      --void $ guardAuthorised hackageRealm users admins
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
          let auth = newDigestPass uname password
          muid <- update' usersState $ AddUser uname auth
          case muid of
            Left err  -> errForbidden "Error registering user" [MText err]
            Right _   -> return uname

    -- Arguments: the auth'd user id, the user path id (derived from the :username)
    canChangePassword :: MonadIO m => UserId -> UserId -> m Bool
    canChangePassword uid userPathId = do
        admins <- query' adminsState State.GetHackageAdmins
        return $ uid == userPathId || (uid `Group.member` admins)

    changePassword :: UserName -> ServerPartE ()
    changePassword userPathName = do
        users  <- query' usersState State.GetUserDb
        pwd <- either (const $ return $ ChangePassword "not" "valid" True) return =<< getData
        if (first pwd == second pwd && first pwd /= "")
         then do
           let passwd = PasswdPlain (first pwd)
               auth   = newDigestPass userPathName passwd
           res <- case Users.lookupName userPathName users of
                    Just userPathId
                      | tryUpgrade pwd -> do
                        update' usersState $ UpgradeUserAuth userPathId passwd auth
                      | otherwise -> do
                        (uid, _) <- guardAuthenticated hackageRealm users
                        -- if this user's id corresponds to the one in the path, or is an admin
                        canChange <- canChangePassword uid userPathId
                        if canChange
                          then update' usersState $ ReplaceUserAuth userPathId auth
                          else forbidChange $ "Not authorized to change password for " ++ display userPathName
                    Nothing -> errForbidden "Error changing password"
                                     [MText $ "User " ++ display userPathName ++ " doesn't exist"]
           maybe (return ()) forbidChange res
         else forbidChange "Copies of new password do not match or is an invalid password (ex: blank)"
      where
        forbidChange = errForbidden "Error changing password" . return . MText

    newDigestPass :: UserName -> PasswdPlain -> UserAuth
    newDigestPass name pwd = NewUserAuth (newPasswdHash hackageRealm name pwd)

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
          queryUserList  = query adminsState GetHackageAdmins,
          addUserList    = update adminsState . AddHackageAdmin,
          removeUserList = update adminsState . RemoveHackageAdmin,
          groupExists    = return True,
          canAddGroup    = [adminGroupDesc],
          canRemoveGroup = [adminGroupDesc]
        }

    groupAddUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupAddUser group _ = do
        users <- query' usersState GetUserDb
        muser <- optional $ look "user"
        case muser of
            Nothing -> addError "Bad request (could not find 'user' argument)"
            Just ustr -> case simpleParse ustr >>= \uname -> Users.lookupName uname users of
                Nothing -> addError $ "No user with name " ++ show ustr ++ " found"
                Just uid -> do
                    ulist <- liftIO . Group.queryGroups $ canAddGroup group
                    void $ guardAuthorised hackageRealm users ulist
                    liftIO $ addUserList group uid
       where addError = errBadRequest "Failed to add user" . return . MText

    groupDeleteUser :: UserGroup -> DynamicPath -> ServerPartE ()
    groupDeleteUser group dpath =
      withUserPath dpath $ \uid _ -> do
        users <- query' usersState GetUserDb
        ulist <- liftIO . Group.queryGroups $ canRemoveGroup group
        void $ guardAuthorised hackageRealm users ulist
        liftIO $ removeUserList group uid

    withGroupEditAuth :: UserGroup -> (Bool -> Bool -> ServerPartE a) -> ServerPartE a
    withGroupEditAuth group func = do
        users  <- query' usersState GetUserDb
        addList    <- liftIO . Group.queryGroups $ canAddGroup group
        removeList <- liftIO . Group.queryGroups $ canRemoveGroup group
        (uid, _) <- guardAuthenticated hackageRealm users
        let (canAdd, canDelete) = (uid `Group.member` addList, uid `Group.member` removeList)
        if not (canAdd || canDelete)
            then errForbidden "Forbidden" [MText "Can't edit permissions for user group"]
            else func canAdd canDelete


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
                                  resourceGet = [("json", handleUserGroupGet group')]
                                }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                                  resourcePut    = [("", handleUserGroupUserPut group groupr)],
                                  resourceDelete = [("", handleUserGroupUserDelete group groupr)]
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
                                  resourceGet = [("json", \dpath -> handleUserGroupGet (getGroupFunc dpath) dpath)]
                                }
              , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) {
                                      resourcePut    = [("", \dpath -> handleUserGroupUserPut    (getGroupFunc dpath) groupr dpath)],
                                      resourceDelete = [("", \dpath -> handleUserGroupUserDelete (getGroupFunc dpath) groupr dpath)]
                                    }
              , getGroup = getGroupFunc
              }
        return (getGroupFunc, groupr)

    handleUserGroupGet group _dpath =
      runServerPartE $
      withGroup group $ \g -> do
        userDb   <- queryGetUserDb
        userlist <- liftIO $ queryUserList g
        let unames = [ Users.idToName userDb uid
                     | uid <- Group.enumerate userlist ]
        return . toResponse . Resource.JSON $
            JSObject $ toJSObject [
              ("title",       JSString $ toJSString $ groupTitle $ groupDesc g)
            , ("description", JSString $ toJSString $ groupPrologue $ groupDesc g)
            , ("members",     JSArray [ JSString $ toJSString $ display uname
                                      | uname <- unames ])
            ]

    handleUserGroupUserPut group groupr dpath =
      runServerPartE $
      withGroup group $ \g ->
        withUserNamePath dpath $ \uname -> do

          -- check the acting user is authorised to modify this group
          users <- queryGetUserDb
          ulist <- liftIO . Group.queryGroups $ canAddGroup g
          guardAuthorised hackageRealm users ulist

          withUserName uname $ \uid _ ->
            liftIO $ addUserList g uid

          goToList groupr dpath

    handleUserGroupUserDelete group groupr dpath =
      runServerPartE $
      withUserNamePath dpath $ \uname -> do

          -- check the acting user is authorised to modify this group
        users <- queryGetUserDb
        ulist <- liftIO . Group.queryGroups $ canRemoveGroup group
        guardAuthorised hackageRealm users ulist

        withUserName uname $ \uid _ ->
          liftIO $ removeUserList group uid

        goToList groupr dpath

    goToList group dpath = seeOther (renderResource' (groupResource group) dpath)
                                    (toResponse ())

    withGroup group func = do
        exists <- liftIO (groupExists group)
        if exists then func group
                  else errNotFound "User group doesn't exist" [MText "User group doesn't exist"]

    ---------------------------------------------------------------
    addGroupIndex :: MonadIO m => UserId -> String -> GroupDescription -> m ()
    addGroupIndex (UserId uid) uri desc =
        Cache.modifyCache groupIndex $
          adjustGroupIndex
            (IntMap.insertWith Set.union uid (Set.singleton uri))
            (Map.insert uri desc)

    removeGroupIndex :: MonadIO m => UserId -> String -> m ()
    removeGroupIndex (UserId uid) uri =
        Cache.modifyCache groupIndex $
          adjustGroupIndex
            (IntMap.update (keepSet . Set.delete uri) uid)
            id
      where
        keepSet m = if Set.null m then Nothing else Just m

    initGroupIndex :: MonadIO m => UserList -> String -> GroupDescription -> m ()
    initGroupIndex ulist uri desc =
        Cache.modifyCache groupIndex $
          adjustGroupIndex
            (IntMap.unionWith Set.union (IntMap.fromList . map mkEntry $ Group.enumerate ulist))
            (Map.insert uri desc)
      where
        mkEntry (UserId uid) = (uid, Set.singleton uri)

    getGroupIndex :: (Functor m, MonadIO m) => UserId -> m [String]
    getGroupIndex (UserId uid) =
      liftM (maybe [] Set.toList . IntMap.lookup uid . usersToGroupUri) $ Cache.getCache groupIndex

    getIndexDesc :: MonadIO m => String -> m GroupDescription
    getIndexDesc uri =
      liftM (Map.findWithDefault nullDescription uri . groupUrisToDesc) $ Cache.getCache groupIndex

    -- partitioning index modifications, a cheap combinator
    adjustGroupIndex :: (IntMap (Set String) -> IntMap (Set String))
                     -> (Map String GroupDescription -> Map String GroupDescription)
                     -> GroupIndex -> GroupIndex
    adjustGroupIndex f g (GroupIndex a b) = GroupIndex (f a) (g b)
