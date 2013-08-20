{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.PackageCandidates.State where

import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Framework.MemSize

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Package

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid


---------------------------------- Index of candidate tarballs and metadata
-- boilerplate code based on PackagesState
data CandidatePackages = CandidatePackages {
    candidateList :: !(PackageIndex.PackageIndex CandPkgInfo)
} deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''CandidatePackages

instance MemSize CandidatePackages where
    memSize (CandidatePackages a) = memSize1 a

initialCandidatePackages :: CandidatePackages
initialCandidatePackages = CandidatePackages {
    candidateList = mempty
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

-- |Replace all existing packages and reports
replaceCandidatePackages :: CandidatePackages -> Update CandidatePackages ()
replaceCandidatePackages = State.put

getCandidatePackages :: Query CandidatePackages CandidatePackages
getCandidatePackages = ask


makeAcidic ''CandidatePackages ['getCandidatePackages
                               ,'replaceCandidatePackages
                               ,'replaceCandidate
                               ,'addCandidate
                               ,'deleteCandidate
                               ,'deleteCandidates
                               ]

