{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Users.Users (
    -- * Users type
    Users(..),

    -- * Construction
    empty,
    add,
    insert,
    requireName,

    -- * Modification
    delete,
    setEnabled,
    replaceAuth,
    rename,

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
import Distribution.Server.Instances ()

import Data.Maybe (maybeToList)
import Data.List (find)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Happstack.Data

-- | The entrie collection of users. Manages the mapping between 'UserName'
-- and 'UserId'.
--
data Users = Users {
    -- | A map from UserId to UserInfo
    userIdMap   :: !(IntMap.IntMap UserInfo),
    -- | A map from active UserNames to the UserId for that name
    userNameMap :: !(Map.Map UserName UserId),
    -- | A map from a UserName to all UserIds which ever used that name
    totalNameMap :: !(Map.Map UserName IntSet.IntSet),
    -- | The next available UserId
    nextId      :: !UserId
  }
  deriving (Typeable, Show)

-- invariant :: Users -> Bool
-- invariant _ = True
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
    totalNameMap = Map.empty,
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
      userStatus = Active Enabled auth
    }
    users'   = users {
      userIdMap   = IntMap.insert userId userInfo (userIdMap users),
      userNameMap = Map.insert name (UserId userId) (userNameMap users),
      totalNameMap = Map.insertWith IntSet.union name (IntSet.singleton userId) (totalNameMap users),
      nextId      = UserId (userId + 1)
    }

-- | Inserts the given info with the given id.
-- If a user is already present with the passed in
-- id, 'Nothing' is returned.
--
-- If the 'UserInfo' does not correspond to that of a
-- deleted user and the user name is already in use,
-- 'Nothing' will be returned.
insert :: UserId -> UserInfo -> Users -> Maybe Users
insert user@(UserId ident) info users = do
    let name = userName info
        isDeleted = case userStatus info of
            Active {} -> False
            _ -> True
        idMap' = intInsertMaybe ident info (userIdMap users)
        nameMap' = insertMaybe name user (userNameMap users)
        totalMap = Map.insertWith IntSet.union name (IntSet.singleton ident) (totalNameMap users)
        nextIdent | user >= nextId users = UserId (ident + 1)
                  | otherwise = nextId users
    -- Nothing on id clash, always fatal
    idMap <- idMap'
    if isDeleted
      then return $ Users idMap (userNameMap users) totalMap nextIdent
      else do
        -- Nothing on name clash only fatal if non-deleted user
        nameMap <- nameMap'
        return $ Users idMap nameMap totalMap nextIdent

-- | Try to find an id for a given user name (either active or historical),
-- and if one doesn't exist, make one as an historical account.
requireName :: UserName -> Users -> (Maybe Users, UserId)
requireName name users =
    case findGoodId =<< Map.lookup name (totalNameMap users) of
        Just uid -> (Nothing, UserId uid)
        Nothing  -> (Just users', UserId userId)
  where
    -- bit of a complicated way to say: preferred existing accounts,
    -- but historical accounts are fine as a second option
    findGoodId iset =
        let infos = do
                uid <- IntSet.toList iset
                info <- maybeToList $ IntMap.lookup uid (userIdMap users)
                return (uid, userStatus info)
        in case fmap fst $ find (isActive . snd) infos of
            Just uid -> Just uid
            Nothing  -> fmap fst $ find (isHistorical . snd) infos
    isActive (Active{}) = True
    isActive _ = False
    isHistorical Historical = True
    isHistorical _ = False
    UserId userId = nextId users
    userInfo = UserInfo {
      userName   = name,
      userStatus = Historical
    }
    users'   = users {
      userIdMap   = IntMap.insert userId userInfo (userIdMap users),
      totalNameMap = Map.insertWith IntSet.union name (IntSet.singleton userId) (totalNameMap users),
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
    -- but total name map remains the same
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
-- * Returns 'Nothing' if the user id does not exist or is deleted
--
setEnabled :: Bool -> UserId -> Users -> Maybe Users
setEnabled newStatus (UserId userId) users = do
    userInfo  <- IntMap.lookup userId (userIdMap users)
    userInfo' <- changeStatus userInfo
    return $! users {
        userIdMap = IntMap.insert userId userInfo' (userIdMap users)
    }
  where
    changeStatus userInfo = case userStatus userInfo of
        Active _ auth -> Just userInfo { userStatus = Active (if newStatus then Enabled else Disabled) auth }
        _ -> Nothing

lookupId :: UserId -> Users -> Maybe UserInfo
lookupId (UserId userId) users = IntMap.lookup userId (userIdMap users)

lookupName :: UserName -> Users -> Maybe UserId
lookupName name users = Map.lookup name (userNameMap users)

-- | Convert a 'UserId' to a 'UserName'. If the user id doesn't exist,
-- an ugly placeholder is used instead.
--
idToName :: Users -> UserId -> UserName
idToName users userId@(UserId idNum) = case lookupId userId users of
  Just user -> userName user
  Nothing   -> UserName $ "~id#" ++ show idNum

-- | Convert a 'UserName' to a 'UserId'. The user name must exist.
--
nameToId :: Users -> UserName -> UserId
nameToId users name = case lookupName name users of
  Just userId -> userId
  Nothing     -> error $ "Users.nameToId: no such user name " ++ show name

-- | Replace the user authentication for the given user.
--   Returns 'Nothing' if the user does not exist.
--
--   If the given user exists and is deleted, 'Just'
--   is returned even though the user still may not
--   authenticate.
replaceAuth :: Users -> UserId -> UserAuth -> Maybe Users
replaceAuth users userId newAuth
    = modifyUser users userId $ \userInfo ->
      case userStatus userInfo of
        Active status _ -> userInfo { userStatus = Active status newAuth }
        _ -> userInfo 

-- | Modify a single user. Returns 'Nothing' if the user does not
--   exist. This function isn't exported.
modifyUser :: Users -> UserId -> (UserInfo -> UserInfo) -> Maybe Users
modifyUser users (UserId userId) fn =
    -- I'm using 'updateLookupWithKey' so I can tell if the lookup succeded
    case IntMap.updateLookupWithKey (\_ user -> Just (fn user)) userId (userIdMap users) of
      (Nothing,_) -> Nothing
      (_,newMap)  -> Just $ users { userIdMap = newMap }

-- | Rename a single user, regardless of account status. Returns either the
-- successfully altered Users structure or a `Maybe UserId` to indicate an
-- error: `Nothing` if the user does not exist and `Just uid` if there is
-- already a user by that name.
rename :: Users -> UserId -> UserName -> Either (Maybe UserId) Users
rename users uid uname = 
    case Map.lookup uname (userNameMap users) of
        Nothing   -> case modifyUser users uid (\user -> user { userName = uname }) of
            Nothing  -> Left Nothing
            Just new -> Right new
        Just uid' -> Left $ Just uid'

enumerateAll :: Users -> [(UserId, UserInfo)]
enumerateAll = mapFst UserId . IntMap.assocs . userIdMap
  where mapFst f = map $ \(x,y) -> (f x, y)

enumerateEnabled :: Users -> [(UserId, UserInfo)]
enumerateEnabled users = [ x | x@(_, UserInfo { userStatus = Active Enabled _ }) <- enumerateAll users ]


-- | Insertion fails if key is present
insertMaybe :: Ord k => k -> a -> Map.Map k a -> (Maybe (Map.Map k a))
insertMaybe k a m
    = case Map.insertLookupWithKey undefined k a m of
        (Nothing, m') -> Just m'
        _ -> Nothing

intInsertMaybe
    :: IntMap.Key -> a -> IntMap.IntMap a -> (Maybe (IntMap.IntMap a))
intInsertMaybe k a m
    = case IntMap.insertLookupWithKey undefined k a m of
        (Nothing, m') -> Just m'
        _ -> Nothing

instance Version Users where
$(deriveSerialize ''Users)


