{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Users.State where

import Distribution.Server.Framework.Instances ()

import Distribution.Server.Users.Types
import Distribution.Server.Users.Group as Group (UserList(..), add, remove, empty)
import Distribution.Server.Users.Users as Users

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

import Control.Monad.Reader
import qualified Control.Monad.State as State

initialUsers :: Users
initialUsers = Users.empty

--------------------------------------------

-- Returns 'Nothing' if the user name is in use
addUser :: UserName -> UserAuth -> Update Users (Either String UserId)
addUser uname auth = updateUsers' updateFn
  where updateFn = Users.add uname auth

-- Requires that a user name exists, either by returning
-- a reference to an active one, returning a reference to
-- an historical one, or creating an historical one,
-- in order of precedence.
requireUserName :: UserName -> Update Users UserId
requireUserName uname = do
    users <- State.get
    let (musers, uid) = Users.requireName uname users
    case musers of
        Just users' -> State.put users'
        Nothing -> return ()
    return uid

-- Disables the indicated user
setEnabledUser :: UserId -> Bool -> Update Users (Maybe String)
setEnabledUser uid en = updateUsers $ Users.setEnabled en uid

-- Deletes the indicated user. Cannot be re-enabled. The associated
-- user name is available for re-use
deleteUser :: UserId -> Update Users (Maybe String)
deleteUser = updateUsers . Users.delete

-- Re-set the user autenication info
replaceUserAuth :: UserId -> UserAuth -> Update Users (Maybe String)
replaceUserAuth userId auth
    = updateUsers $ \users -> Users.replaceAuth users userId auth

upgradeUserAuth :: UserId -> PasswdPlain -> UserAuth -> Update Users (Maybe String)
upgradeUserAuth userId passwd auth
    = updateUsers $ \users -> Users.upgradeAuth users userId passwd auth

renameUser :: UserId -> UserName -> Update Users (Maybe (Maybe UserId))
renameUser uid uname = do
  users <- State.get
  case Users.rename users uid uname of
    Left err -> return $ Just err
    Right users' -> do
      State.put users'
      return Nothing

-- updates the user db with a simpler function
updateUsers :: (Users -> Either String Users) -> Update Users (Maybe String)
updateUsers f = liftM finish $ updateUsers' updateFn
    where updateFn users = fmap (flip (,) ()) $ f users
          finish (Left msg) = Just msg
          finish (Right ()) = Nothing

-- Helper function for updating the users db
updateUsers' :: (Users -> Either String (Users, a)) -> Update Users (Either String a)
updateUsers' f = do
  users <- State.get
  case (f users) of
    Left err -> return (Left err)
    Right (users',a) -> do
      State.put users'
      return (Right a)

lookupUserName :: UserName -> Query Users (Maybe UserId)
lookupUserName = queryUsers . Users.lookupName

queryUsers :: (Users -> a) -> Query Users a
queryUsers queryFn = liftM queryFn ask

getUserDb :: Query Users Users
getUserDb = ask

replaceUserDb :: Users -> Update Users ()
replaceUserDb = State.put

$(makeAcidic ''Users ['addUser
                     ,'requireUserName
                     ,'setEnabledUser
                     ,'deleteUser
                     ,'replaceUserAuth
                     ,'upgradeUserAuth
                     ,'renameUser
                     ,'lookupUserName
                     ,'getUserDb
                     ,'replaceUserDb
                     ])

-----------------------------------------------------

data HackageAdmins = HackageAdmins {
    adminList :: !Group.UserList
} deriving (Typeable)

$(deriveSafeCopy 0 'base ''HackageAdmins)

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
} deriving (Eq, Typeable)
$(deriveSafeCopy 0 'base ''MirrorClients)

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

--------------------------------------------------------------------------
data IndexUsers = IndexUsers {
    indexUsers :: !Group.UserList
} deriving (Typeable)
$(deriveSafeCopy 0 'base ''IndexUsers)

getIndexUsers :: Query IndexUsers UserList
getIndexUsers = asks indexUsers

modifyIndexUsers :: (UserList -> UserList) -> Update IndexUsers ()
modifyIndexUsers func = State.modify (\users -> users { indexUsers = func (indexUsers users) })

addIndexUser :: UserId -> Update IndexUsers ()
addIndexUser uid = modifyIndexUsers (Group.add uid)

removeIndexUser :: UserId -> Update IndexUsers ()
removeIndexUser uid = modifyIndexUsers (Group.remove uid)

replaceIndexUsers :: UserList -> Update IndexUsers ()
replaceIndexUsers ulist = modifyIndexUsers (const ulist)

initialIndexUsers :: IndexUsers
initialIndexUsers = IndexUsers Group.empty

$(makeAcidic ''IndexUsers
                    ['getIndexUsers
                    ,'addIndexUser
                    ,'removeIndexUser
                    ,'replaceIndexUsers])
