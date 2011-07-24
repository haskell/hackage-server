{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Users.State where

import Distribution.Server.Framework.Instances ()

import Distribution.Server.Users.Types
import Distribution.Server.Users.Group as Group (UserList(..), enumerate, add, remove, empty)
import Distribution.Server.Users.Users as Users

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Data.Maybe (isJust, maybeToList)

import Control.Monad.Reader
import qualified Control.Monad.State as State

initialUsers :: Users
initialUsers = Users.empty

--------------------------------------------

-- Returns 'Nothing' if the user name is in use
addUser :: UserName -> UserAuth -> Update Users (Maybe UserId)
addUser uname auth = updateUsers' updateFn formatFn
  where updateFn = Users.add uname auth
        formatFn = id

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
listGroupMembers userList = do
    users <- ask
    return $ do
        uid <- Group.enumerate userList
        uinfo <- maybeToList $ Users.lookupId uid users
        return $ userName uinfo

$(makeAcidic ''Users ['addUser
                     ,'requireUserName
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

$(deriveSafeCopy 0 'base ''HackageAdmins)

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

initialHackageAdmins :: HackageAdmins
initialHackageAdmins = HackageAdmins Group.empty

$(makeAcidic ''HackageAdmins
                 ['getHackageAdmins
                 ,'addHackageAdmin
                 ,'removeHackageAdmin
                 ,'replaceHackageAdmins])

--------------------------------------------------------------------------
data MirrorClients = MirrorClients {
    mirrorClients :: !Group.UserList
} deriving (Typeable)
$(deriveSafeCopy 0 'base ''MirrorClients)

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
     
initialMirrorClients :: MirrorClients
initialMirrorClients = MirrorClients Group.empty

$(makeAcidic ''MirrorClients
                    ['getMirrorClients
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
