{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Packages.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Users.State ()

import Distribution.Package
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..), CandPkgInfo(..), pkgUploadUser, pkgUploadTime)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserList)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.BlobStorage (BlobId)
import Data.TarIndex (TarIndex)

import qualified Data.Serialize as Serialize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.Map as Map

---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageList  :: !(PackageIndex PkgInfo)
  }
  deriving (Eq, Typeable, Show)

$(deriveSafeCopy 0 'base ''PackagesState)

initialPackagesState :: PackagesState
initialPackagesState = PackagesState {
    packageList = mempty
  }

instance SafeCopy PkgInfo where
  putCopy = contain . Serialize.put
  getCopy = contain Serialize.get

insertPkgIfAbsent :: PkgInfo -> Update PackagesState Bool
insertPkgIfAbsent pkg = do
    pkgsState <- State.get
    case PackageIndex.lookupPackageId (packageList pkgsState) (packageId pkg) of   
        Nothing -> do State.put $ pkgsState { packageList = PackageIndex.insert pkg (packageList pkgsState) }
                      return True
        Just{}  -> do return False

-- could also return something to indicate existence
mergePkg :: PkgInfo -> Update PackagesState ()
mergePkg pkg = State.modify $ \pkgsState -> pkgsState { packageList = PackageIndex.insertWith mergeFunc pkg (packageList pkgsState) }
  where
    mergeFunc newPkg oldPkg = oldPkg {
        pkgData = pkgData newPkg,
        pkgTarball = sortByDate $ pkgTarball newPkg ++ pkgTarball oldPkg,
        -- the old package data paired with when and by whom it was replaced
        pkgDataOld = sortByDate $ (pkgData oldPkg, pkgUploadData newPkg):(pkgDataOld oldPkg ++ pkgDataOld newPkg)
      }

    sortByDate :: Ord a => [(a1, (a, b))] -> [(a1, (a, b))]
    sortByDate xs = sortBy (comparing (fst . snd)) xs

deletePackageVersion :: PackageId -> Update PackagesState ()
deletePackageVersion pkg = State.modify $ \pkgsState -> pkgsState { packageList = deleteVersion (packageList pkgsState) }
    where deleteVersion = PackageIndex.deletePackageId pkg

replacePackageUploader :: PackageId -> UserId -> Update PackagesState (Maybe String)
replacePackageUploader pkg uid = modifyPkgInfo pkg $ \pkgInfo -> pkgInfo { pkgUploadData = (pkgUploadTime pkgInfo, uid) }

replacePackageUploadTime :: PackageId -> UTCTime -> Update PackagesState (Maybe String)
replacePackageUploadTime pkg time = modifyPkgInfo pkg $ \pkgInfo -> pkgInfo { pkgUploadData = (time, pkgUploadUser pkgInfo) }

modifyPkgInfo :: PackageId -> (PkgInfo -> PkgInfo) -> Update PackagesState (Maybe String)
modifyPkgInfo pkg f = do
  pkgsState <- State.get
  case PackageIndex.lookupPackageId (packageList pkgsState) pkg of
    Nothing -> return (Just "No such package")
    Just pkgInfo -> do
      State.put $ pkgsState { packageList = PackageIndex.insert (f pkgInfo) (packageList pkgsState) }
      return Nothing

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

-- TODO: add more querying functions; there are too many
-- `fmap packageList $ query GetPackagesState' throughout code

$(makeAcidic ''PackagesState ['getPackagesState
                             ,'replacePackagesState
                             ,'replacePackageUploadTime
                             ,'replacePackageUploader
                             ,'insertPkgIfAbsent
                             ,'mergePkg
                             ,'deletePackageVersion
                             ])

---------------------------------- Index of candidate tarballs and metadata
-- boilerplate code based on PackagesState
data CandidatePackages = CandidatePackages {
    candidateList :: !(PackageIndex.PackageIndex CandPkgInfo)
} deriving (Typeable, Show)

$(deriveSafeCopy 0 'base ''CandidatePackages)

initialCandidatePackages :: CandidatePackages
initialCandidatePackages = CandidatePackages {
    candidateList = mempty
  }

$(deriveSafeCopy 0 'base ''CandPkgInfo)

replaceCandidate :: CandPkgInfo -> Update CandidatePackages ()
replaceCandidate pkg = State.modify $ \candidates -> candidates { candidateList = replaceVersions (candidateList candidates) }
    where replaceVersions = PackageIndex.insert pkg . PackageIndex.deletePackageName (packageName pkg)

addCandidate :: CandPkgInfo -> Update CandidatePackages ()
addCandidate pkg = State.modify $ \candidates -> candidates { candidateList = addVersion (candidateList candidates) }
    where addVersion = PackageIndex.insert pkg

deleteCandidate :: PackageId -> Update CandidatePackages ()
deleteCandidate pkg = State.modify $ \candidates -> candidates { candidateList = deleteVersion (candidateList candidates) }
    where deleteVersion = PackageIndex.deletePackageId pkg

deleteCandidates :: PackageName -> Update CandidatePackages ()
deleteCandidates pkg = State.modify $ \candidates -> candidates { candidateList = deleteVersions (candidateList candidates) }
    where deleteVersions = PackageIndex.deletePackageName pkg

-- |Replace all existing packages and reports
replaceCandidatePackages :: CandidatePackages -> Update CandidatePackages ()
replaceCandidatePackages = State.put

getCandidatePackages :: Query CandidatePackages CandidatePackages
getCandidatePackages = ask


$(makeAcidic ''CandidatePackages ['getCandidatePackages
                                 ,'replaceCandidatePackages
                                 ,'replaceCandidate
                                 ,'addCandidate
                                 ,'deleteCandidate
                                 ,'deleteCandidates
                                 ])


---------------------------------- Documentation
data Documentation = Documentation {
     documentation :: Map.Map PackageIdentifier (BlobId, TarIndex)
   } deriving (Typeable, Show)

initialDocumentation :: Documentation
initialDocumentation = Documentation Map.empty

instance SafeCopy Documentation where
    putCopy (Documentation m) = contain $ safePut m
    getCopy = contain $ liftM Documentation safeGet

instance SafeCopy BlobId where
  putCopy = contain . Serialize.put
  getCopy = contain Serialize.get

lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe (BlobId, TarIndex))
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

hasDocumentation :: PackageIdentifier -> Query Documentation Bool
hasDocumentation pkgId
    = lookupDocumentation pkgId >>= \x -> case x of
         Just{} -> return True
         _      -> return False

insertDocumentation :: PackageIdentifier -> BlobId -> TarIndex -> Update Documentation ()
insertDocumentation pkgId blob index
    = State.modify $ \doc -> doc {documentation = Map.insert pkgId (blob, index) (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

$(makeAcidic ''Documentation ['insertDocumentation
                             ,'lookupDocumentation
                             ,'hasDocumentation
                             ,'getDocumentation
                             ,'replaceDocumentation
                             ])

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserList
} deriving (Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''PackageMaintainers)

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

$(makeAcidic ''PackageMaintainers ['getPackageMaintainers
                                  ,'addPackageMaintainer
                                  ,'removePackageMaintainer
                                  ,'setPackageMaintainers
                                  ,'replacePackageMaintainers
                                  ,'allPackageMaintainers
                                  ])

-------------------------------- Trustee list
-- this could be reasonably merged into the above, as a PackageGroups data structure
data HackageTrustees = HackageTrustees {
    trusteeList :: UserList
} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''HackageTrustees)

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

$(makeAcidic ''HackageTrustees ['getHackageTrustees
                               ,'addHackageTrustee
                               ,'removeHackageTrustee
                               ,'replaceHackageTrustees
                               ])

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

$(makeAcidic ''HackageUploaders ['getHackageUploaders
                                ,'addHackageUploader
                                ,'removeHackageUploader
                                ,'replaceHackageUploaders
                                ])
