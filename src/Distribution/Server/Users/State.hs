{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, StandaloneDeriving, DeriveGeneric, DeriveAnyClass, 
             DerivingStrategies, GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Users.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Server.Users.Types
import Distribution.Server.Users.Group (UserIdSet)
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Users.Users as Users

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Control.Monad.Reader
import qualified Control.Monad.State as State
import qualified Data.Text as T

import Data.Int
import Data.Text
import Database.Beam
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..), autoSqlValueSyntax)
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax (..))

initialUsers :: Users.Users
initialUsers = Users.emptyUsers

--------------------------------------------

-- Returns 'Nothing' if the user name is in use
addUserEnabled :: UserName -> UserAuth -> Update Users.Users (Either Users.ErrUserNameClash UserId)
addUserEnabled uname auth =
  updateUsers $ Users.addUserEnabled uname auth

addUserDisabled :: UserName -> Update Users.Users (Either Users.ErrUserNameClash UserId)
addUserDisabled uname =
  updateUsers $ Users.addUserDisabled uname

-- Enables or disables the indicated user's account
setUserEnabledStatus :: UserId -> Bool -> Update Users.Users (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser))
setUserEnabledStatus uid en =
  updateUsers_ $ Users.setUserEnabledStatus uid en

-- Deletes the indicated user. Cannot be re-enabled. The associated
-- user name is available for re-use
deleteUser :: UserId -> Update Users.Users (Maybe Users.ErrNoSuchUserId)
deleteUser uid =
  updateUsers_ $ Users.deleteUser uid

-- Set the user authentication info
setUserAuth :: UserId -> UserAuth -> Update Users.Users (Maybe (Either Users.ErrNoSuchUserId Users.ErrDeletedUser))
setUserAuth userId auth =
  updateUsers_ $ Users.setUserAuth userId auth

setUserName :: UserId -> UserName -> Update Users.Users (Maybe (Either Users.ErrNoSuchUserId Users.ErrUserNameClash))
setUserName uid uname =
  updateUsers_ $ Users.setUserName uid uname

addAuthToken :: UserId -> AuthToken -> T.Text
             -> Update Users.Users (Maybe Users.ErrNoSuchUserId)
addAuthToken uid authToken description =
  updateUsers_ $ Users.addAuthToken uid authToken description

revokeAuthToken :: UserId -> AuthToken
                -> Update Users.Users (Maybe (Either Users.ErrNoSuchUserId Users.ErrTokenNotOwned))
revokeAuthToken uid authToken =
  updateUsers_ $ Users.revokeAuthToken uid authToken

-- updates the user db with a simpler function
updateUsers_ :: (Users.Users -> Either err Users.Users) -> Update Users.Users (Maybe err)
updateUsers_ upd = do
  users <- State.get
  case upd users of
    Left err     -> return (Just err)
    Right users' -> do State.put users'
                       return Nothing

-- Helper function for updating the users db
updateUsers :: (Users.Users -> Either err (Users.Users, a)) -> Update Users.Users (Either err a)
updateUsers upd = do
  users <- State.get
  case upd users of
    Left err         -> return (Left err)
    Right (users',a) -> do State.put users'
                           return (Right a)

getUserDb :: Query Users.Users Users.Users
getUserDb = ask

replaceUserDb :: Users.Users -> Update Users.Users ()
replaceUserDb = State.put

$(makeAcidic ''Users.Users [ 'addUserEnabled
                          , 'addUserDisabled
                          , 'setUserEnabledStatus
                          , 'setUserAuth
                          , 'setUserName
                          , 'deleteUser
                          , 'getUserDb
                          , 'replaceUserDb
                          , 'addAuthToken
                          , 'revokeAuthToken
                          ])

-----------------------------------------------------

data HackageAdmins = HackageAdmins {
    adminList :: !Group.UserIdSet
} deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''HackageAdmins)

instance MemSize HackageAdmins where
    memSize (HackageAdmins a) = memSize1 a

getHackageAdmins :: Query HackageAdmins HackageAdmins
getHackageAdmins = ask

getAdminList :: Query HackageAdmins UserIdSet
getAdminList = asks adminList

modifyHackageAdmins :: (UserIdSet -> UserIdSet) -> Update HackageAdmins ()
modifyHackageAdmins func = State.modify (\users -> users { adminList = func (adminList users) })

addHackageAdmin :: UserId -> Update HackageAdmins ()
addHackageAdmin uid = modifyHackageAdmins (Group.insert uid)

removeHackageAdmin :: UserId -> Update HackageAdmins ()
removeHackageAdmin uid = modifyHackageAdmins (Group.delete uid)

replaceHackageAdmins :: UserIdSet -> Update HackageAdmins ()
replaceHackageAdmins ulist = modifyHackageAdmins (const ulist)

initialHackageAdmins :: HackageAdmins
initialHackageAdmins = HackageAdmins Group.empty

$(makeAcidic ''HackageAdmins
                 ['getHackageAdmins
                 ,'getAdminList
                 ,'addHackageAdmin
                 ,'removeHackageAdmin
                 ,'replaceHackageAdmins])

--------------------------------------------------------------------------
data MirrorClients = MirrorClients {
    mirrorClients :: !Group.UserIdSet
} deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''MirrorClients)

instance MemSize MirrorClients where
    memSize (MirrorClients a) = memSize1 a

getMirrorClients :: Query MirrorClients MirrorClients
getMirrorClients = ask

getMirrorClientsList :: Query MirrorClients UserIdSet
getMirrorClientsList = asks mirrorClients

modifyMirrorClients :: (UserIdSet -> UserIdSet) -> Update MirrorClients ()
modifyMirrorClients func = State.modify (\users -> users { mirrorClients = func (mirrorClients users) })

addMirrorClient :: UserId -> Update MirrorClients ()
addMirrorClient uid = modifyMirrorClients (Group.insert uid)

removeMirrorClient :: UserId -> Update MirrorClients ()
removeMirrorClient uid = modifyMirrorClients (Group.delete uid)

replaceMirrorClients :: UserIdSet -> Update MirrorClients ()
replaceMirrorClients ulist = modifyMirrorClients (const ulist)

initialMirrorClients :: MirrorClients
initialMirrorClients = MirrorClients Group.empty

$(makeAcidic ''MirrorClients
                    ['getMirrorClients
                    ,'getMirrorClientsList
                    ,'addMirrorClient
                    ,'removeMirrorClient
                    ,'replaceMirrorClients])

-- Database

data UsersT f
  = UsersRow
  { _uId :: Columnar f UserId,
    _uUsername :: Columnar f UserName,
    _uStatus :: Columnar f UsersStatus,
    _uAuthInfo :: Columnar f (Maybe UserAuth), 
    _uAdmin :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type UserRow = UsersT Identity

deriving instance Show UserRow

deriving instance Eq UserRow

type UsersId = PrimaryKey UsersT Identity

instance Table UsersT where
  data PrimaryKey UsersT f = UsersId (Columnar f UserId) deriving (Generic, Beamable)
  primaryKey = UsersId . _uId

data UsersStatus = Enabled | Disabled | Deleted
  deriving (Eq, Show, Read, Enum, Bounded)

instance HasSqlValueSyntax SqliteValueSyntax UsersStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite UsersStatus where
  fromBackendRow = read . unpack <$> fromBackendRow

newtype DBUserName = DBUserName Text
  deriving newtype (Eq, Ord, Read, Show, FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax)

data UserTokensT f
  = UserTokensRow
  { _utId :: Columnar f Int32,
    _utUserId :: Columnar f UserId,
    _utDescription :: Columnar f Text,
    _utToken :: Columnar f AuthToken
  }
  deriving (Generic, Beamable)

type UserTokenRow = UserTokensT Identity

deriving instance Show UserTokenRow

deriving instance Eq UserTokenRow

type UserTokensId = PrimaryKey UserTokensT Identity

instance Table UserTokensT where
  data PrimaryKey UserTokensT f = UserTokensId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = UserTokensId . _utId  