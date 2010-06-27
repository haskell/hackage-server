{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Packages.State where

import Distribution.Server.Instances ()
import Distribution.Server.Users.State ()

import Distribution.Package
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..), CandPkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserList)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Util.BlobStorage (BlobId)

import Happstack.State
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.Map as Map

---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageList  :: !(PackageIndex.PackageIndex PkgInfo)
  }
  deriving (Typeable, Show)

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState {
    packageList = mempty
  }

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx) = contain $ do
    safePut $ PackageIndex.allPackages idx
  getCopy = contain $ do
    packages <- safeGet
    return PackagesState {
      packageList = PackageIndex.fromList packages
    }

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

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
        pkgDesc = pkgDesc newPkg,
        pkgData = pkgData newPkg,
        pkgTarball = sortByDate $ pkgTarball newPkg ++ pkgTarball oldPkg,
        -- the old package data paired with when and by whom it was replaced
        pkgDataOld = sortByDate $ (pkgData oldPkg, pkgUploadData newPkg):(pkgDataOld oldPkg ++ pkgDataOld newPkg)
      }
    sortByDate xs = sortBy (comparing (fst . snd)) xs

deletePkg :: PackageId -> Update PackagesState ()
deletePkg pkg = State.modify $ \pkgsState -> pkgsState { packageList = deleteVersion (packageList pkgsState) }
    where deleteVersion = PackageIndex.deletePackageId (packageId pkg)

-- NOTE! overwrites any existing data
-- TODO: get rid of this, now that PackagesState is just packages (not userdb etc.)
bulkImport :: [PkgInfo] -> Update PackagesState ()
bulkImport newIndex = do
  pkgsState <- State.get
  State.put pkgsState {
    packageList = PackageIndex.fromList newIndex
  }

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask


$(mkMethods ''PackagesState ['getPackagesState
                            ,'bulkImport
                            ,'replacePackagesState
                            ,'insertPkgIfAbsent
                            ,'mergePkg
                            ,'deletePkg
                            ])

---------------------------------- Index of candidate tarballs and metadata
-- boilerplate code based on PackagesState
data CandidatePackages = CandidatePackages {
    candidateList :: !(PackageIndex.PackageIndex CandPkgInfo)
} deriving (Typeable, Show)

instance Component CandidatePackages where
  type Dependencies CandidatePackages = End
  initialValue = CandidatePackages {
    candidateList = mempty
  }

instance Version CandidatePackages where
    mode = Versioned 0 Nothing

instance Serialize CandidatePackages where
  putCopy (CandidatePackages idx) = contain $ do
    safePut $ PackageIndex.allPackages idx
  getCopy = contain $ do
    packages <- safeGet
    return CandidatePackages {
      candidateList  = PackageIndex.fromList packages
    }

instance Version CandPkgInfo where
  mode = Versioned 0 Nothing

$(deriveSerialize ''CandPkgInfo)

setCandidate :: CandPkgInfo -> Update CandidatePackages ()
setCandidate pkg = State.modify $ \candidates -> candidates { candidateList = replaceVersions (candidateList candidates) }
    where replaceVersions = PackageIndex.insert pkg . PackageIndex.deletePackageName (packageName pkg)

deleteCandidate :: PackageName -> Update CandidatePackages ()
deleteCandidate pkg = State.modify $ \candidates -> candidates { candidateList = deleteVersions (candidateList candidates) }
    where deleteVersions = PackageIndex.deletePackageName pkg

-- |Replace all existing packages and reports
replaceCandidatePackages :: CandidatePackages -> Update CandidatePackages ()
replaceCandidatePackages = State.put

getCandidatePackages :: Query CandidatePackages CandidatePackages
getCandidatePackages = ask


$(mkMethods ''CandidatePackages ['getCandidatePackages
                                ,'replaceCandidatePackages
                                ,'setCandidate
                                ,'deleteCandidate
                                ])


---------------------------------- Documentation
data Documentation = Documentation {
     documentation :: Map.Map PackageIdentifier BlobId
     } deriving (Typeable, Show)

instance Component Documentation where
    type Dependencies Documentation = End
    initialValue = Documentation Map.empty

instance Version Documentation where
    mode = Versioned 0 Nothing -- Version 0, no previous types

instance Serialize Documentation where
    putCopy (Documentation m) = contain $ safePut m
    getCopy = contain $ liftM Documentation safeGet

instance Version BlobId where
  mode = Versioned 0 Nothing

instance Serialize BlobId where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe BlobId)
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

hasDocumentation :: PackageIdentifier -> Query Documentation Bool
hasDocumentation pkgId
    = lookupDocumentation pkgId >>= \x -> case x of
         Just{} -> return True
         _      -> return False

insertDocumentation :: PackageIdentifier -> BlobId -> Update Documentation ()
insertDocumentation pkgId blob
    = State.modify $ \doc -> doc {documentation = Map.insert pkgId blob (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

$(mkMethods ''Documentation ['insertDocumentation
                            ,'lookupDocumentation
                            ,'hasDocumentation
                            ,'getDocumentation
                            ,'replaceDocumentation
                            ])

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserList
} deriving (Show, Typeable)

instance Version PackageMaintainers where
  mode = Versioned 0 Nothing
$(deriveSerialize ''PackageMaintainers)

instance Component PackageMaintainers where
  type Dependencies PackageMaintainers = End
  initialValue = PackageMaintainers Map.empty

getPackageMaintainers :: PackageName -> Query PackageMaintainers UserList
getPackageMaintainers name = fmap (fromMaybe Group.empty . Map.lookup name) (asks maintainers)

modifyPackageMaintainers :: PackageName -> (UserList -> UserList) -> Update PackageMaintainers ()
modifyPackageMaintainers name func = State.modify (\pm -> pm {maintainers = alterFunc (maintainers pm) })
    where alterFunc = Map.alter (Just . func . fromMaybe Group.empty) name

addPackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
addPackageMaintainer name uid = modifyPackageMaintainers name (Group.add uid)

removePackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
removePackageMaintainer name uid = modifyPackageMaintainers name (Group.remove uid)

replacePackageMaintainers :: PackageName -> UserList -> Update PackageMaintainers ()
replacePackageMaintainers name ulist = modifyPackageMaintainers name (const ulist)

$(mkMethods ''PackageMaintainers ['getPackageMaintainers
                                 ,'addPackageMaintainer
                                 ,'removePackageMaintainer
                                 ,'replacePackageMaintainers
                                 ])

-------------------------------- Trustee list
-- this could be reasonably merged into the above, as a PackageGroups data structure
data HackageTrustees = HackageTrustees {
    trustees :: UserList
} deriving (Show, Typeable)

instance Version HackageTrustees where
  mode = Versioned 0 Nothing
$(deriveSerialize ''HackageTrustees)

instance Component HackageTrustees where
  type Dependencies HackageTrustees = End
  initialValue = HackageTrustees Group.empty

getHackageTrustees :: Query HackageTrustees UserList
getHackageTrustees = asks trustees

modifyHackageTrustees :: (UserList -> UserList) -> Update HackageTrustees ()
modifyHackageTrustees func = State.modify (\ht -> ht {trustees = func (trustees ht) })

addHackageTrustee :: UserId -> Update HackageTrustees ()
addHackageTrustee uid = modifyHackageTrustees (Group.add uid)

removeHackageTrustee :: UserId -> Update HackageTrustees ()
removeHackageTrustee uid = modifyHackageTrustees (Group.remove uid)

replaceHackageTrustees :: UserList -> Update HackageTrustees ()
replaceHackageTrustees ulist = modifyHackageTrustees (const ulist)

$(mkMethods ''HackageTrustees ['getHackageTrustees
                              ,'addHackageTrustee
                              ,'removeHackageTrustee
                              ,'replaceHackageTrustees
                              ])

---------------------------------------------
data PackageUpload = PackageUpload deriving (Typeable)
instance Version PackageUpload where
  mode = Versioned 0 Nothing
$(deriveSerialize ''PackageUpload)

instance Component PackageUpload where
    type Dependencies PackageUpload = PackageMaintainers :+: HackageTrustees :+: End
    initialValue = PackageUpload

$(mkMethods ''PackageUpload [])
