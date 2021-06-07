{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.Users (
    -- * Users type
    Users,

    -- * Construction
    emptyUsers,
    addUserEnabled,
    addUserDisabled,
    addUser,
    insertUserAccount,

    -- * Modification
    deleteUser,
    setUserEnabledStatus,
    setUserAuth,
    setUserName,
    addAuthToken,
    revokeAuthToken,

    -- * Lookup
    lookupUserId,
    lookupUserName,
    lookupAuthToken,

    -- ** Lookup utils
    userIdToName,

    -- * Enumeration
    enumerateAllUsers,
    enumerateActiveUsers,

    -- * Error codes
    ErrUserNameClash(..),
    ErrUserIdClash(..),
    ErrNoSuchUserId(..),
    ErrDeletedUser(..),
    ErrTokenNotOwned(..)
  ) where

import Distribution.Server.Users.Types

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Control.Monad (guard, unless)
import Data.Maybe (fromMaybe)
import Data.List  (sort, group, foldl')
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Data.Typeable (Typeable)
import Control.Exception (assert)
import qualified Data.Text as T


-- | The entire collection of users. Manages the mapping between 'UserName'
-- and 'UserId'.
--
data Users = Users {
    -- | A map from UserId to UserInfo
    userIdMap   :: !(IntMap.IntMap UserInfo),
    -- | A map from active UserNames to the UserId for that name
    userNameMap :: !(Map.Map UserName UserId),
    -- | The next available UserId
    nextId      :: !UserId,
    -- | A map from 'AuthToken' to 'UserId' for quick token based auth
    authTokenMap :: !(Map.Map AuthToken UserId)
  }
  deriving (Eq, Typeable, Show)

instance MemSize Users where
  memSize (Users a b c d) = memSize4 a b c d

checkinvariant :: Users -> Users
checkinvariant users = assert (invariant users) users

invariant :: Users -> Bool
invariant Users{userIdMap, userNameMap, nextId, authTokenMap} =
      nextIdIsRight
   && noUserNameOverlap
   && userNameMapComplete
   && userNameMapConsistent
   && authTokenMapConsistent
  where
    nextIdIsRight =
      --  1) the next id should be 0 if the userIdMap is empty
      --     or one bigger than the maximum allocated id
      let UserId nextid = nextId
       in nextid == case IntMap.maxViewWithKey userIdMap of
                      Nothing                     -> 0
                      Just ((maxAllocatedId,_),_) -> maxAllocatedId + 1

    noUserNameOverlap =
      --  2) there must be no overlap in the user names of active accounts
      --     (active are enabled or disabled but not deleted)
          all (\g -> length g == 1)
        . group . sort
        . map userName . filter (isActiveAccount . userStatus)
        . IntMap.elems
        $ userIdMap

    userNameMapComplete =
      -- 3) the userNameMap must map every active user name to the id of the
      --    corresponding user info
          Map.keys userNameMap
       == sort [ userName uinfo
               | uinfo <- IntMap.elems userIdMap
               , isActiveAccount (userStatus uinfo)]

    userNameMapConsistent =
      and [ case IntMap.lookup uid userIdMap of
              Nothing    -> False
              Just uinfo -> userName uinfo == uname
          | (uname, UserId uid) <- Map.toList userNameMap ]
  -- the point is, user names can be recycled but user ids never are
  -- this simplifies things because other user groups in the system do not
  -- need to be adjusted when an account is enabled/disabled/deleted
  -- it also allows us to track historical info, like name of uploader
  -- even if that user name has been recycled, the user ids will be distinct.

    -- every registered token must map to a users token set
    -- and vice versa
    authTokenMapConsistent =
      and
      [ and
        [ case IntMap.lookup uid userIdMap of
            Nothing    -> False
            Just uinfo -> Map.member token (userTokens uinfo)
        | (token, UserId uid) <- Map.toList authTokenMap
        ]
      , and
        [ Map.lookup token authTokenMap == Just uid
        | (token, uid) <- concatMap getUserTokList (IntMap.toList userIdMap)
        ]
      ]
    getUserTokList (uid, uinfo) = [ (t, UserId uid)
                                  | t <- Map.keys (userTokens uinfo) ]

emptyUsers :: Users
emptyUsers = Users {
    userIdMap   = IntMap.empty,
    userNameMap = Map.empty,
    nextId      = UserId 0,
    authTokenMap = Map.empty
  }

-- error codes
data ErrUserNameClash = ErrUserNameClash deriving Typeable
data ErrUserIdClash   = ErrUserIdClash   deriving Typeable
data ErrNoSuchUserId  = ErrNoSuchUserId  deriving Typeable
data ErrDeletedUser   = ErrDeletedUser   deriving Typeable
data ErrTokenNotOwned = ErrTokenNotOwned deriving Typeable

$(deriveSafeCopy 0 'base ''ErrUserNameClash)
$(deriveSafeCopy 0 'base ''ErrUserIdClash)
$(deriveSafeCopy 0 'base ''ErrNoSuchUserId)
$(deriveSafeCopy 0 'base ''ErrDeletedUser)
$(deriveSafeCopy 0 'base ''ErrTokenNotOwned)

(?!) :: Maybe a -> e -> Either e a
ma ?! e = maybe (Left e) Right ma



lookupUserId :: UserId -> Users -> Maybe UserInfo
lookupUserId (UserId userId) users = IntMap.lookup userId (userIdMap users)

lookupUserName :: UserName -> Users -> Maybe (UserId, UserInfo)
lookupUserName uname users = do
    case Map.lookup uname (userNameMap users) of
      Nothing  -> Nothing
      Just uid -> Just (uid, fromMaybe impossible (lookupUserId uid users))
  where
    impossible = error "lookupUserName: invariant violation"

lookupAuthToken :: AuthToken -> Users -> Maybe (UserId, UserInfo)
lookupAuthToken authTok users =
    do uid <- Map.lookup authTok (authTokenMap users)
       uinfo <- lookupUserId uid users
       return (uid, uinfo)

-- | Convert a 'UserId' to a 'UserName'. If the user id doesn't exist,
-- an ugly placeholder is used instead.
--
userIdToName :: Users -> UserId -> UserName
userIdToName users userId@(UserId idNum) =
    case lookupUserId userId users of
      Just user -> userName user
      Nothing   -> UserName $ "~id#" ++ show idNum


-- | Add a new user account, in the enabled state.
--
addUserEnabled :: UserName -> UserAuth -> Users
               -> Either ErrUserNameClash (Users, UserId)
addUserEnabled name auth = addUser name (AccountEnabled auth)

-- | Add a new user account, in the disabled state and with no password.
--
addUserDisabled :: UserName -> Users
                -> Either ErrUserNameClash (Users, UserId)
addUserDisabled name = addUser name (AccountDisabled Nothing)

-- | Add a new user account with the given user status.
--
addUser :: UserName -> UserStatus -> Users -> Either ErrUserNameClash (Users, UserId)
addUser name status users =
  case Map.lookup name (userNameMap users) of
    Just _  -> Left ErrUserNameClash
    Nothing -> users' `seq` Right (users', userid)
      where
        userid@(UserId uid) = nextId users
        uinfo = UserInfo {
          userName   = name,
          userStatus = status,
          userTokens = Map.empty
        }
        users' = checkinvariant users {
          userIdMap   = IntMap.insert uid uinfo (userIdMap users),
          userNameMap = Map.insert name userid (userNameMap users),
          nextId      = UserId (uid + 1)
        }

-- | Insert pre-existing user info. This should only be used for constructing
-- a user db manually or from a backup.
--
insertUserAccount :: UserId -> UserInfo -> Users
                  -> Either (Either ErrUserIdClash ErrUserNameClash) Users
insertUserAccount userId@(UserId uid) uinfo users = do
    guard (not userIdInUse)                     ?! Left  ErrUserIdClash
    guard (not userNameInUse || isUserDeleted)  ?! Right ErrUserNameClash
    return $! checkinvariant users {
          userIdMap   = IntMap.insert uid uinfo (userIdMap users),
          authTokenMap =
              foldl' (\om tok -> Map.insert tok userId om)
                     (authTokenMap users)
                     (Map.keys (userTokens uinfo)),
          userNameMap = if isUserDeleted
                          then userNameMap users
                          else Map.insert (userName uinfo) userId (userNameMap users),
          nextId      = let UserId nextid = nextId users
                        in UserId (max nextid (uid + 1))
        }
  where
    userIdInUse   = IntMap.member uid (userIdMap users)
    userNameInUse = Map.member (userName uinfo) (userNameMap users)
    isUserDeleted = case userStatus uinfo of
                      AccountDeleted -> True
                      _              -> False


-- | Delete a user account.
--
-- Prevents the given user from performing authenticated operations.
-- This operation is idempotent but not reversible. Deleting an account forgets
-- any authentication credentials and the user name becomes available for
-- re-use in a new account.
--
-- Unlike 'UserName's, 'UserId's are never actually deleted or re-used. This is
-- what distinguishes disabling and deleting an account; a disabled account can
-- be enabled again and a disabled account does not release the user name for
-- re-use.
--
deleteUser :: UserId -> Users -> Either ErrNoSuchUserId Users
deleteUser (UserId userId) users = do
  userInfo     <- lookupUserId (UserId userId) users ?! ErrNoSuchUserId
  let userInfo' = userInfo { userStatus = AccountDeleted }
  return $! checkinvariant users {
    userIdMap   = IntMap.insert userId userInfo' (userIdMap users),
    userNameMap = Map.delete (userName userInfo) (userNameMap users)
  }

-- | Change the status of a user account to enabled or disabled.
--
-- Prevents the given user from performing any authenticated operations.
-- This operation is idempotent and reversible. Use 'enable' to re-enable a
-- disabled account.
--
-- The disabled state is intended to be temporary. Use 'delete' to permanently
-- delete the account and release the user name to be re-used.
--
setUserEnabledStatus :: UserId -> Bool -> Users -> Either (Either ErrNoSuchUserId ErrDeletedUser) Users
setUserEnabledStatus (UserId uid) enable users = do
    userInfo  <- lookupUserId (UserId uid) users ?! Left  ErrNoSuchUserId
    userInfo' <- changeStatus userInfo           ?! Right ErrDeletedUser
    return $! checkinvariant users {
        userIdMap = IntMap.insert uid userInfo' (userIdMap users)
    }
  where
    changeStatus userInfo | enable = case userStatus userInfo of
        AccountEnabled  _           -> Just userInfo
        AccountDisabled (Just auth) -> Just userInfo { userStatus = AccountEnabled auth }
        AccountDisabled Nothing     -> Nothing
        AccountDeleted              -> Nothing

    changeStatus userInfo          = case userStatus userInfo of
        AccountEnabled  auth       -> Just userInfo { userStatus = AccountDisabled (Just auth) }
        AccountDisabled _          -> Just userInfo
        AccountDeleted             -> Nothing

-- | Replace the user authentication for the given user.
--
setUserAuth :: UserId -> UserAuth -> Users -> Either (Either ErrNoSuchUserId ErrDeletedUser) Users
setUserAuth (UserId uid) newauth users = do
    userInfo  <- lookupUserId (UserId uid) users ?! Left  ErrNoSuchUserId
    userInfo' <- changeAuth userInfo             ?! Right ErrDeletedUser
    return $! checkinvariant users {
      userIdMap = IntMap.insert uid userInfo' (userIdMap users)
    }
  where
    changeAuth userInfo = case userStatus userInfo of
        AccountEnabled  _oldauth -> Just $ userInfo { userStatus = AccountEnabled newauth }
        AccountDisabled _oldauth -> Just $ userInfo { userStatus = AccountDisabled (Just newauth) }
        AccountDeleted           -> Nothing

-- | Change the username for a user account. The new name must not be in use.
--
setUserName :: UserId -> UserName -> Users
            -> Either (Either ErrNoSuchUserId ErrUserNameClash) Users
setUserName (UserId uid) newname users = do
    userinfo  <- lookupUserId (UserId uid) users ?! Left  ErrNoSuchUserId
    guard (not (userNameInUse newname))          ?! Right ErrUserNameClash
    let oldname   = userName userinfo
        userinfo' = userinfo { userName = newname }
    return $! checkinvariant users {
      userIdMap   = IntMap.insert uid userinfo' (userIdMap users),
      userNameMap = Map.insert newname (UserId uid) . Map.delete oldname $ userNameMap users
    }
  where
    userNameInUse uname = Map.member uname (userNameMap users)

-- | Register a new auth token for a user account
addAuthToken :: UserId -> AuthToken -> T.Text -> Users
             -> Either ErrNoSuchUserId Users
addAuthToken (UserId uid) token description users = do
    userinfo <- lookupUserId (UserId uid) users ?! ErrNoSuchUserId
    let userinfo' = userinfo {
                      userTokens = Map.insert token description
                                              (userTokens userinfo)
                    }
    return $! checkinvariant users {
      userIdMap    = IntMap.insert uid userinfo' (userIdMap users),
      authTokenMap = Map.insert token (UserId uid) (authTokenMap users)
    }

-- | Revoke an auth token from a user account
revokeAuthToken :: UserId -> AuthToken -> Users
                -> Either (Either ErrNoSuchUserId ErrTokenNotOwned) Users
revokeAuthToken (UserId uid) token users = do
    userinfo <- lookupUserId (UserId uid) users ?! Left ErrNoSuchUserId
    unless (Map.member token (userTokens userinfo)) $
      Left (Right ErrTokenNotOwned)
    let userinfo' = userinfo {
                      userTokens = Map.delete token (userTokens userinfo)
                    }
    return $! checkinvariant users {
      userIdMap    = IntMap.insert uid userinfo' (userIdMap users),
      authTokenMap = Map.delete token (authTokenMap users)
    }

enumerateAllUsers :: Users -> [(UserId, UserInfo)]
enumerateAllUsers users =
    [ (UserId uid, uinfo) | (uid, uinfo) <- IntMap.assocs (userIdMap users) ]

enumerateActiveUsers :: Users -> [(UserId, UserInfo)]
enumerateActiveUsers users =
    [ (UserId uid, uinfo) | (uid, uinfo) <- IntMap.assocs (userIdMap users)
                          , isActiveAccount (userStatus uinfo) ]

data Users_v0 = Users_v0 {
    -- | A map from UserId to UserInfo
    userIdMap_v0   :: !(IntMap.IntMap UserInfo),
    -- | A map from active UserNames to the UserId for that name
    userNameMap_v0 :: !(Map.Map UserName UserId),
    -- | The next available UserId
    nextId_v0      :: !UserId
  }
  deriving (Eq, Typeable, Show)

instance Migrate Users where
    type MigrateFrom Users = Users_v0
    migrate v0 =
        Users
        { userIdMap = userIdMap_v0 v0
        , userNameMap = userNameMap_v0 v0
        , nextId = nextId_v0 v0
        , authTokenMap = Map.empty
        }

$(deriveSafeCopy 0 'base ''Users_v0)
$(deriveSafeCopy 1 'extension ''Users)
