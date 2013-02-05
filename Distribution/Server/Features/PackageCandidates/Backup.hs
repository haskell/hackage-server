module Distribution.Server.Features.PackageCandidates.Backup where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Features.Core.Backup as CoreBackup
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Package (packageId)
import Text.CSV (CSV)
import Data.Version (Version(Version), showVersion)

{-------------------------------------------------------------------------------
  Backup
-------------------------------------------------------------------------------}

backupCandidates :: CandidatePackages -> [BackupEntry]
backupCandidates st = concatMap backupCandidate candidates
  where
    candidates :: [CandPkgInfo]
    candidates = PackageIndex.allPackages (candidateList st)

backupCandidate :: CandPkgInfo -> [BackupEntry]
backupCandidate candidate =
    csvToBackup (CoreBackup.pkgPath (packageId candidate) "candidate.csv")
                (backupExtraInfo candidate)
  : CoreBackup.infoToAllEntries (candPkgInfo candidate)

-- | Backup the information CandPkgInfo adds on top of PkgInfo
backupExtraInfo :: CandPkgInfo -> CSV
backupExtraInfo candidate = [
    [showVersion versionCSV]
  , ["public", show (candPublic candidate)]
  , "warnings" : candWarnings candidate
  ]
  where
    versionCSV = Version [0,1] ["unstable"]
