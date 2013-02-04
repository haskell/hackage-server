module Distribution.Server.Features.PackageCandidates.Backup where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Packages.Types

import Distribution.Server.Features.Core.Backup as CoreBackup

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

backupCandidates :: CandidatePackages -> [BackupEntry]
backupCandidates st = concatMap CoreBackup.infoToAllEntries packages
  where
    candidates :: [CandPkgInfo]
    candidates = PackageIndex.allPackages (candidateList st)

    packages :: [PkgInfo]
    packages = map candPkgInfo candidates

