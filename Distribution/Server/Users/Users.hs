{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Users.Users (
    -- * Users type
    Users,

    -- * Construction
    empty,
    add,

    -- * Modification
    delete,
    disable,
    enable,

    -- * Lookup
    lookupId,
    lookupName,

    -- ** Lookup utils
    idToName,
    nameToId,

    -- * Enumeration
    enumerateAll,
    enumerateEnabled,
  ) where

import Distribution.Server.Users.Types

import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Control.Applicative ((<$>), (<*>))

-- | The entrie collection of users. Manages the mapping between 'UserName'
-- and 'UserId'.
--
data Users = Users {
    userIdMap   :: !(IntMap.IntMap UserInfo),
    userNameMap :: !(Map.Map UserName UserId),
    nextId      :: !UserId
  }
  deriving Typeable

invariant :: Users -> Bool
invariant _ = True
  --TODO: 1) the next id should be 0 if the userIdMap is empty
  --         or one bigger than the maximum allocated id
  --      2) there must be no overlap in the user names of active accounts
  --         (active are enabled or disabled but not deleted)
  --      3) the userNameMap must map every active user name to the id of the
  --         corresponding user info

  -- the point is, user names can be recycled but user ids never are
  -- this simplifies things because other user groups in the system do not
  -- need to be adjusted when an account is enabled/disabled/deleted
  -- it also allows us to track historical info, like name of uploader
  -- even if that user name has been recycled, the user ids will be distinct.

empty :: Users
empty = Users {
    userIdMap   = IntMap.empty,
    userNameMap = Map.empty,
    nextId      = UserId 0
  }

-- | Add a new user account.
--
-- The new account is created in the enabled state.
--
-- * Returns 'Nothing' if the user name is already in use.
--
add :: UserName -> UserAuth -> Users -> Maybe (Users, UserId)
add name auth users =
  case Map.lookup name (userNameMap users) of
    Just _  -> Nothing -- user name already exists
    Nothing -> users' `seq` Just (users', UserId userId)
      where
        UserId userId = nextId users
        userInfo = UserInfo {
          userName   = name,
          userStatus = Enabled auth
        }
        users'   = users {
          userIdMap   = IntMap.insert userId userInfo (userIdMap users),
          userNameMap =    Map.insert name (UserId userId) (userNameMap users),
          nextId      = UserId (userId + 1)
        }

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
-- * Returns 'Nothing' if the user id does not exist.
--
delete :: UserId -> Users -> Maybe Users
delete (UserId userId) users = do
  userInfo     <- IntMap.lookup userId (userIdMap users)
  let userInfo' = userInfo { userStatus = Deleted }
  return $! users {
    userIdMap   = IntMap.insert userId userInfo' (userIdMap users),
    userNameMap = Map.delete (userName userInfo) (userNameMap users)
  }

-- | Disable a user account.
--
-- Prevents the given user from performing any authenticated operations.
-- This operation is idempotent and reversable. Use 'enable' to re-enable a
-- disabled account.
--
-- The disabled state is intended to be temporary. Use 'delete' to permanently
-- delete the account and release the user name to be re-used.
--
-- * Returns 'Nothing' if the user id does not exist or is deleted
--
disable :: UserId -> Users -> Maybe Users
disable (UserId userId) users = do
  userInfo   <- IntMap.lookup userId (userIdMap users)
  userInfo'  <- disableAccount userInfo
  return $! users {
    userIdMap = IntMap.insert userId userInfo' (userIdMap users)
  }
  where
    disableAccount userInfo = case userStatus userInfo of
      Deleted       -> Nothing
      Disabled _    -> Just userInfo
      Enabled  auth -> Just userInfo { userStatus = Disabled auth }

-- | Enable a user account.
--
-- This operation is idempotent and reversable. The ordinary state of accounts
-- is enabled. Accounts can be 'disable'd and this operation is used to
-- re-enable them.
--
enable :: UserId -> Users -> Maybe Users
enable (UserId userId) users = do
  userInfo   <- IntMap.lookup userId (userIdMap users)
  userInfo'  <- enableAccount userInfo
  return $! users {
    userIdMap = IntMap.insert userId userInfo' (userIdMap users)
  }
  where
    enableAccount userInfo = case userStatus userInfo of
      Deleted       -> Nothing
      Disabled auth -> Just userInfo { userStatus = Enabled auth }
      Enabled  _    -> Just userInfo

lookupId :: UserId -> Users -> Maybe UserInfo
lookupId (UserId userId) users = IntMap.lookup userId (userIdMap users)

lookupName :: UserName -> Users -> Maybe UserId
lookupName name users = Map.lookup name (userNameMap users)

-- | Convert a 'UserId' to a 'UserName'. The user id must exist.
--
idToName :: Users -> UserId -> UserName
idToName users userId = case lookupId userId users of
  Just user -> userName user
  Nothing   -> error $ "Users.idToName: no such user id " ++ show userId

-- | Convert a 'UserName' to a 'UserId'. The user name must exist.
--
nameToId :: Users -> UserName -> UserId
nameToId users name = case lookupName name users of
  Just userId -> userId
  Nothing     -> error $ "Users.nameToId: no such user name " ++ show name

enumerateAll :: Users -> [UserInfo]
enumerateAll users = IntMap.elems (userIdMap users)

enumerateEnabled :: Users -> [UserInfo]
enumerateEnabled users =
  [ user | user@UserInfo { userStatus = Enabled _ } <- enumerateAll users ]


instance Binary Users where
  put (Users a b c) = Binary.put a >> Binary.put b >> Binary.put c
  get = Users <$> Binary.get <*> Binary.get <*> Binary.get
