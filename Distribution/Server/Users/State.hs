{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Users.State where

import Distribution.Server.Instances ()

import Distribution.Server.Users.Types (UserId,UserName,UserAuth)
import Distribution.Server.Users.Group as Group (UserList(..), enumerate, add, remove, empty)
import Distribution.Server.Users.Users as Users

import Data.Typeable (Typeable)
import Data.Maybe (isJust)

import Happstack.State

import Control.Monad.Reader
import qualified Control.Monad.State as State

instance Component Users where
  type Dependencies Users = End
  initialValue = Users.empty

--------------------------------------------

-- Returns 'Nothing' if the user name is in use
addUser :: UserName -> UserAuth -> Update Users (Maybe UserId)
addUser userName auth = updateUsers' updateFn formatFn
  where updateFn = Users.add userName auth
        formatFn = id

-- Disables the indicated user
setEnabledUser :: UserId -> Bool -> Update Users Bool
setEnabledUser uid en = updateUsers $ Users.setEnabled en uid

-- Deletes the indicated user. Cannot be re-enabled. The associated
-- user name is available for re-use 
deleteUser :: UserId -> Update Users Bool
deleteUser = updateUsers . Users.delete

-- Re-set the user autenication info
replaceUserAuth :: UserId -> UserAuth -> Update Users Bool
replaceUserAuth userId auth
    = updateUsers $ \users -> Users.replaceAuth users userId auth

renameUser :: UserId -> UserName -> Update Users (Maybe (Maybe UserId))
renameUser uid uname = do
  users <- State.get
  case Users.rename users uid uname of
    Left err -> return $ Just err
    Right users' -> do
      State.put users'
      return Nothing

-- updates the user db with a simpler function
updateUsers :: (Users -> Maybe Users) -> Update Users Bool
updateUsers f = updateUsers' updateFn isJust
    where updateFn users = fmap (swap . (,) ()) $ f users
          swap (x,y) = (y,x)

-- Helper function for updating the users db
updateUsers' :: (Users -> Maybe (Users, a)) -> (Maybe a -> b) -> Update Users b
updateUsers' f format = do
  users <- State.get
  liftM format $ case (f users) of
    Nothing -> return Nothing
    Just (users',a) -> do
      State.put users'
      return (Just a)

lookupUserName :: UserName -> Query Users (Maybe UserId)
lookupUserName = queryUsers . Users.lookupName

queryUsers :: (Users -> a) -> Query Users a
queryUsers queryFn = liftM queryFn ask

getUserDb :: Query Users Users
getUserDb = ask

replaceUserDb :: Users -> Update Users ()
replaceUserDb = State.put

listGroupMembers :: UserList -> Query Users [UserName]
listGroupMembers userList
    = do users <- ask
         return [ Users.idToName users uid | uid <- Group.enumerate userList ]

$(mkMethods ''Users ['addUser
                    ,'setEnabledUser
                    ,'deleteUser
                    ,'replaceUserAuth
                    ,'renameUser
                    ,'lookupUserName
                    ,'getUserDb
                    ,'replaceUserDb
                    ,'listGroupMembers
                    ])

-----------------------------------------------------

data HackageAdmins = HackageAdmins {
    adminList :: !Group.UserList
} deriving (Typeable)
$(deriveSerialize ''HackageAdmins)
instance Version HackageAdmins where
    mode = Versioned 0 Nothing

getHackageAdmins :: Query HackageAdmins UserList
getHackageAdmins = asks adminList

modifyHackageAdmins :: (UserList -> UserList) -> Update HackageAdmins ()
modifyHackageAdmins func = State.modify (\users -> users { adminList = func (adminList users) })

addHackageAdmin :: UserId -> Update HackageAdmins ()
addHackageAdmin uid = modifyHackageAdmins (Group.add uid)

removeHackageAdmin :: UserId -> Update HackageAdmins ()
removeHackageAdmin uid = modifyHackageAdmins (Group.remove uid)

replaceHackageAdmins :: UserList -> Update HackageAdmins ()
replaceHackageAdmins ulist = modifyHackageAdmins (const ulist)

instance Component HackageAdmins where
    type Dependencies HackageAdmins = End
    initialValue = HackageAdmins Group.empty

$(mkMethods ''HackageAdmins
                    ['getHackageAdmins
                    ,'addHackageAdmin
                    ,'removeHackageAdmin
                    ,'replaceHackageAdmins])

--------------------------------------------------------------------------
data MirrorClients = MirrorClients {
    mirrorClients :: !Group.UserList
} deriving (Typeable)
$(deriveSerialize ''MirrorClients)
instance Version MirrorClients where
    mode = Versioned 0 Nothing

getMirrorClients :: Query MirrorClients UserList
getMirrorClients = asks mirrorClients

modifyMirrorClients :: (UserList -> UserList) -> Update MirrorClients ()
modifyMirrorClients func = State.modify (\users -> users { mirrorClients = func (mirrorClients users) })

addMirrorClient :: UserId -> Update MirrorClients ()
addMirrorClient uid = modifyMirrorClients (Group.add uid)

removeMirrorClient :: UserId -> Update MirrorClients ()
removeMirrorClient uid = modifyMirrorClients (Group.remove uid)

replaceMirrorClients :: UserList -> Update MirrorClients ()
replaceMirrorClients ulist = modifyMirrorClients (const ulist)

instance Component MirrorClients where
    type Dependencies MirrorClients = End
    initialValue = MirrorClients Group.empty

$(mkMethods ''MirrorClients
                    ['getMirrorClients
                    ,'addMirrorClient
                    ,'removeMirrorClient
                    ,'replaceMirrorClients])

--------------------------------------------------------------------------
data IndexUsers = IndexUsers {
    indexUsers :: !Group.UserList
} deriving (Typeable)
$(deriveSerialize ''IndexUsers)
instance Version IndexUsers where
    mode = Versioned 0 Nothing

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

instance Component IndexUsers where
    type Dependencies IndexUsers = End
    initialValue = IndexUsers Group.empty

$(mkMethods ''IndexUsers
                    ['getIndexUsers
                    ,'addIndexUser
                    ,'removeIndexUser
                    ,'replaceIndexUsers])
