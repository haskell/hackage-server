{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, NamedFieldPuns #-}

module Distribution.Server.Features.PackageCandidates.State where

import Distribution.Server.Prelude

import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Framework.MemSize
import Distribution.Server.Packages.Types

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Package

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (Migrate(..), deriveSafeCopy, base, extension)
import Control.Monad.Reader
import qualified Control.Monad.State as State


---------------------------------- Index of candidate tarballs and metadata
-- boilerplate code based on PackagesState
data CandidatePackages = CandidatePackages {
    candidateList :: !(PackageIndex.PackageIndex CandPkgInfo)

    -- | Did we do the migration for PkgTarball, computing hashes for candidates?
  , candidateMigratedPkgTarball :: Bool
  } deriving (Typeable, Show, Eq)

data CandidatePackages_v0 = CandidatePackages_v0 {
    candidateList_v0 :: !(PackageIndex.PackageIndex CandPkgInfo)
  } deriving (Typeable, Show, Eq)

deriveSafeCopy 1 'extension ''CandidatePackages
deriveSafeCopy 0 'base ''CandidatePackages_v0

instance Migrate CandidatePackages where
  type MigrateFrom CandidatePackages = CandidatePackages_v0
  migrate (CandidatePackages_v0 cs) = CandidatePackages cs False

instance MemSize CandidatePackages where
    memSize (CandidatePackages a b) = memSize2 a b

-- | See comments in 'initialPackagesState' about 'freshDB'.
initialCandidatePackages :: Bool -> CandidatePackages
initialCandidatePackages freshDB = CandidatePackages {
    candidateList               = mempty
  , candidateMigratedPkgTarball = freshDB
  }

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

updateCandidatePkgInfo :: PackageId -> PkgInfo -> Update CandidatePackages Bool
updateCandidatePkgInfo pkgId pkgInfo = do
    st@CandidatePackages{candidateList} <- State.get
    case PackageIndex.lookupPackageId candidateList pkgId of
      Nothing   -> return False
      Just cand -> do
        let cand'          = cand { candPkgInfo = pkgInfo }
            candidateList' = PackageIndex.insert cand' candidateList
        State.put $! st { candidateList = candidateList' }
        return True

-- |Replace all existing packages and reports
replaceCandidatePackages :: CandidatePackages -> Update CandidatePackages ()
replaceCandidatePackages = State.put

getCandidatePackages :: Query CandidatePackages CandidatePackages
getCandidatePackages = ask

setMigratedPkgTarball :: Update CandidatePackages ()
setMigratedPkgTarball = State.modify $ \st -> st { candidateMigratedPkgTarball = True }

makeAcidic ''CandidatePackages ['getCandidatePackages
                               ,'replaceCandidatePackages
                               ,'replaceCandidate
                               ,'addCandidate
                               ,'deleteCandidate
                               ,'deleteCandidates
                               ,'setMigratedPkgTarball
                               ,'updateCandidatePkgInfo
                               ]
