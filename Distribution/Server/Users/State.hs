{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Users.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Server.Users.Types
import Distribution.Server.Users.Group as Group (UserList(..), add, remove, empty)
import qualified Distribution.Server.Users.Users as Users

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

import Control.Monad.Reader
import qualified Control.Monad.State as State

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
                          ])

-----------------------------------------------------

data HackageAdmins = HackageAdmins {
    adminList :: !Group.UserList
} deriving (Typeable, Eq, Show)

$(deriveSafeCopy 0 'base ''HackageAdmins)

instance MemSize HackageAdmins where
    memSize (HackageAdmins a) = memSize1 a

getHackageAdmins :: Query HackageAdmins HackageAdmins
getHackageAdmins = ask

getAdminList :: Query HackageAdmins UserList
getAdminList = asks adminList

modifyHackageAdmins :: (UserList -> UserList) -> Update HackageAdmins ()
modifyHackageAdmins func = State.modify (\users -> users { adminList = func (adminList users) })

addHackageAdmin :: UserId -> Update HackageAdmins ()
addHackageAdmin uid = modifyHackageAdmins (Group.add uid)

removeHackageAdmin :: UserId -> Update HackageAdmins ()
removeHackageAdmin uid = modifyHackageAdmins (Group.remove uid)

replaceHackageAdmins :: UserList -> Update HackageAdmins ()
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
    mirrorClients :: !Group.UserList
} deriving (Eq, Typeable, Show)

$(deriveSafeCopy 0 'base ''MirrorClients)

instance MemSize MirrorClients where
    memSize (MirrorClients a) = memSize1 a

getMirrorClients :: Query MirrorClients MirrorClients
getMirrorClients = ask

getMirrorClientsList :: Query MirrorClients UserList
getMirrorClientsList = asks mirrorClients

modifyMirrorClients :: (UserList -> UserList) -> Update MirrorClients ()
modifyMirrorClients func = State.modify (\users -> users { mirrorClients = func (mirrorClients users) })

addMirrorClient :: UserId -> Update MirrorClients ()
addMirrorClient uid = modifyMirrorClients (Group.add uid)

removeMirrorClient :: UserId -> Update MirrorClients ()
removeMirrorClient uid = modifyMirrorClients (Group.remove uid)

replaceMirrorClients :: UserList -> Update MirrorClients ()
replaceMirrorClients ulist = modifyMirrorClients (const ulist)

initialMirrorClients :: MirrorClients
initialMirrorClients = MirrorClients Group.empty

$(makeAcidic ''MirrorClients
                    ['getMirrorClients
                    ,'getMirrorClientsList
                    ,'addMirrorClient
                    ,'removeMirrorClient
                    ,'replaceMirrorClients])

