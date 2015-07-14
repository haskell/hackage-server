{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Upload.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.Group (UserIdSet)

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserIdSet
} deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''PackageMaintainers

instance MemSize PackageMaintainers where
    memSize (PackageMaintainers a) = memSize1 a

initialPackageMaintainers :: PackageMaintainers
initialPackageMaintainers = PackageMaintainers Map.empty

getPackageMaintainers :: PackageName -> Query PackageMaintainers UserIdSet
getPackageMaintainers name = asks $ fromMaybe Group.empty . Map.lookup name . maintainers

modifyPackageMaintainers :: PackageName -> (UserIdSet -> UserIdSet) -> Update PackageMaintainers ()
modifyPackageMaintainers name func = State.modify (\pm -> pm {maintainers = alterFunc (maintainers pm) })
    where alterFunc = Map.alter (Just . func . fromMaybe Group.empty) name

addPackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
addPackageMaintainer name uid = modifyPackageMaintainers name (Group.insert uid)

removePackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
removePackageMaintainer name uid = modifyPackageMaintainers name (Group.delete uid)

setPackageMaintainers :: PackageName -> UserIdSet -> Update PackageMaintainers ()
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
    trusteeList :: !UserIdSet
} deriving (Show, Typeable, Eq)

deriveSafeCopy 0 'base ''HackageTrustees

instance MemSize HackageTrustees where
    memSize (HackageTrustees a) = memSize1 a

initialHackageTrustees :: HackageTrustees
initialHackageTrustees = HackageTrustees Group.empty

getHackageTrustees :: Query HackageTrustees HackageTrustees
getHackageTrustees = ask

getTrusteesList :: Query HackageTrustees UserIdSet
getTrusteesList = asks trusteeList

modifyHackageTrustees :: (UserIdSet -> UserIdSet) -> Update HackageTrustees ()
modifyHackageTrustees func = State.modify (\ht -> ht {trusteeList = func (trusteeList ht) })

addHackageTrustee :: UserId -> Update HackageTrustees ()
addHackageTrustee uid = modifyHackageTrustees (Group.insert uid)

removeHackageTrustee :: UserId -> Update HackageTrustees ()
removeHackageTrustee uid = modifyHackageTrustees (Group.delete uid)

replaceHackageTrustees :: UserIdSet -> Update HackageTrustees ()
replaceHackageTrustees ulist = modifyHackageTrustees (const ulist)

makeAcidic ''HackageTrustees ['getHackageTrustees
                             ,'getTrusteesList
                             ,'addHackageTrustee
                             ,'removeHackageTrustee
                             ,'replaceHackageTrustees
                             ]

-------------------------------- Uploader list
data HackageUploaders = HackageUploaders {
    uploaderList :: !UserIdSet
} deriving (Show, Typeable, Eq)

$(deriveSafeCopy 0 'base ''HackageUploaders)

instance MemSize HackageUploaders where
    memSize (HackageUploaders a) = memSize1 a

initialHackageUploaders :: HackageUploaders
initialHackageUploaders = HackageUploaders Group.empty

getHackageUploaders :: Query HackageUploaders HackageUploaders
getHackageUploaders = ask

getUploadersList :: Query HackageUploaders UserIdSet
getUploadersList = asks uploaderList

modifyHackageUploaders :: (UserIdSet -> UserIdSet) -> Update HackageUploaders ()
modifyHackageUploaders func = State.modify (\ht -> ht {uploaderList = func (uploaderList ht) })

addHackageUploader :: UserId -> Update HackageUploaders ()
addHackageUploader uid = modifyHackageUploaders (Group.insert uid)

removeHackageUploader :: UserId -> Update HackageUploaders ()
removeHackageUploader uid = modifyHackageUploaders (Group.delete uid)

replaceHackageUploaders :: UserIdSet -> Update HackageUploaders ()
replaceHackageUploaders ulist = modifyHackageUploaders (const ulist)

makeAcidic ''HackageUploaders ['getHackageUploaders
                              ,'getUploadersList
                              ,'addHackageUploader
                              ,'removeHackageUploader
                              ,'replaceHackageUploaders
                              ]
