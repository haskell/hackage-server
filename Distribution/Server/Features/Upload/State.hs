{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Upload.State where

import Distribution.Server.Framework.Instances ()

import Distribution.Package
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserList)
import Distribution.Server.Users.Types (UserId)

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserList
} deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''PackageMaintainers

initialPackageMaintainers :: PackageMaintainers
initialPackageMaintainers = PackageMaintainers Map.empty

getPackageMaintainers :: PackageName -> Query PackageMaintainers UserList
getPackageMaintainers name = asks $ fromMaybe Group.empty . Map.lookup name . maintainers

modifyPackageMaintainers :: PackageName -> (UserList -> UserList) -> Update PackageMaintainers ()
modifyPackageMaintainers name func = State.modify (\pm -> pm {maintainers = alterFunc (maintainers pm) })
    where alterFunc = Map.alter (Just . func . fromMaybe Group.empty) name

addPackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
addPackageMaintainer name uid = modifyPackageMaintainers name (Group.add uid)

removePackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
removePackageMaintainer name uid = modifyPackageMaintainers name (Group.remove uid)

setPackageMaintainers :: PackageName -> UserList -> Update PackageMaintainers ()
setPackageMaintainers name ulist = modifyPackageMaintainers name (const ulist)

allPackageMaintainers :: Query PackageMaintainers PackageMaintainers
allPackageMaintainers = ask

replacePackageMaintainers :: PackageMaintainers -> Update PackageMaintainers ()
replacePackageMaintainers = State.put

makeAcidic ''PackageMaintainers ['getPackageMaintainers
                                ,'addPackageMaintainer
                                ,'removePackageMaintainer
                                ,'setPackageMaintainers
                                ,'replacePackageMaintainers
                                ,'allPackageMaintainers
                                ]

-------------------------------- Trustee list
-- this could be reasonably merged into the above, as a PackageGroups data structure
data HackageTrustees = HackageTrustees {
    trusteeList :: UserList
} deriving (Show, Typeable)

deriveSafeCopy 0 'base ''HackageTrustees

initialHackageTrustees :: HackageTrustees
initialHackageTrustees = HackageTrustees Group.empty

getHackageTrustees :: Query HackageTrustees UserList
getHackageTrustees = asks trusteeList

modifyHackageTrustees :: (UserList -> UserList) -> Update HackageTrustees ()
modifyHackageTrustees func = State.modify (\ht -> ht {trusteeList = func (trusteeList ht) })

addHackageTrustee :: UserId -> Update HackageTrustees ()
addHackageTrustee uid = modifyHackageTrustees (Group.add uid)

removeHackageTrustee :: UserId -> Update HackageTrustees ()
removeHackageTrustee uid = modifyHackageTrustees (Group.remove uid)

replaceHackageTrustees :: UserList -> Update HackageTrustees ()
replaceHackageTrustees ulist = modifyHackageTrustees (const ulist)

makeAcidic ''HackageTrustees ['getHackageTrustees
                             ,'addHackageTrustee
                             ,'removeHackageTrustee
                             ,'replaceHackageTrustees
                             ]

-------------------------------- Uploader list
data HackageUploaders = HackageUploaders {
    uploaderList :: UserList
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''HackageUploaders)

initialHackageUploaders :: HackageUploaders
initialHackageUploaders = HackageUploaders Group.empty

getHackageUploaders :: Query HackageUploaders UserList
getHackageUploaders = asks uploaderList

modifyHackageUploaders :: (UserList -> UserList) -> Update HackageUploaders ()
modifyHackageUploaders func = State.modify (\ht -> ht {uploaderList = func (uploaderList ht) })

addHackageUploader :: UserId -> Update HackageUploaders ()
addHackageUploader uid = modifyHackageUploaders (Group.add uid)

removeHackageUploader :: UserId -> Update HackageUploaders ()
removeHackageUploader uid = modifyHackageUploaders (Group.remove uid)

replaceHackageUploaders :: UserList -> Update HackageUploaders ()
replaceHackageUploaders ulist = modifyHackageUploaders (const ulist)

makeAcidic ''HackageUploaders ['getHackageUploaders
                              ,'addHackageUploader
                              ,'removeHackageUploader
                              ,'replaceHackageUploaders
                              ]
