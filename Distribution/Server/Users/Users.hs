{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, NamedFieldPuns #-}
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

    -- * Lookup
    lookupUserId,
    lookupUserName,

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
  ) where

import Distribution.Server.Users.Types

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Control.Monad (guard)
import Control.Monad.Error (Error(..))
import Data.Maybe (fromMaybe)
import Data.List  (sort, group)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Control.Exception (assert)


-- | The entire collection of users. Manages the mapping between 'UserName'
-- and 'UserId'.
--
data Users = Users {
    -- | A map from UserId to UserInfo
    userIdMap   :: !(IntMap.IntMap UserInfo),
    -- | A map from active UserNames to the UserId for that name
    userNameMap :: !(Map.Map UserName UserId),
    -- | The next available UserId
    nextId      :: !UserId
  }
  deriving (Eq, Typeable, Show)

instance MemSize Users where
  memSize (Users a b c) = memSize3 a b c

$(deriveSafeCopy 0 'base ''Users)

checkinvariant :: Users -> Users
checkinvariant users = assert (invariant users) users 

invariant :: Users -> Bool
invariant Users{userIdMap, userNameMap, nextId} =
      nextIdIsRight
   && noUserNameOverlap
   && userNameMapComplete
   && userNameMapConsistent
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


emptyUsers :: Users
emptyUsers = Users {
    userIdMap   = IntMap.empty,
    userNameMap = Map.empty,
    nextId      = UserId 0
  }

-- error codes
data ErrUserNameClash = ErrUserNameClash deriving Typeable
data ErrUserIdClash   = ErrUserIdClash   deriving Typeable
data ErrNoSuchUserId  = ErrNoSuchUserId  deriving Typeable
data ErrDeletedUser   = ErrDeletedUser   deriving Typeable

instance Error ErrUserNameClash
instance Error ErrUserIdClash
instance Error ErrNoSuchUserId
instance Error ErrDeletedUser

$(deriveSafeCopy 0 'base ''ErrUserNameClash)
$(deriveSafeCopy 0 'base ''ErrUserIdClash)
$(deriveSafeCopy 0 'base ''ErrNoSuchUserId)
$(deriveSafeCopy 0 'base ''ErrDeletedUser)

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
          userStatus = status
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
-- This operation is idempotent and reversable. Use 'enable' to re-enable a
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

enumerateAllUsers :: Users -> [(UserId, UserInfo)]
enumerateAllUsers users =
    [ (UserId uid, uinfo) | (uid, uinfo) <- IntMap.assocs (userIdMap users) ]

enumerateActiveUsers :: Users -> [(UserId, UserInfo)]
enumerateActiveUsers users =
    [ (UserId uid, uinfo) | (uid, uinfo) <- IntMap.assocs (userIdMap users)
                          , isActiveAccount (userStatus uinfo) ]

