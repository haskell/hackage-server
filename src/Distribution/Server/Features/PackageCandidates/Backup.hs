module Distribution.Server.Features.PackageCandidates.Backup where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Features.Core.Backup as CoreBackup
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Package (PackageId, packageId)
import Text.CSV (CSV)
import Data.Version (Version(Version), showVersion)
import Data.Map (Map)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  Restore backup
-------------------------------------------------------------------------------}

data PartialCandidate = PartialCandidate {
    partialWarnings :: [String]
  , partialPublic   :: Bool
  }

type PartialCandidates = Map PackageId PartialCandidate

restoreCandidates :: RestoreBackup CandidatePackages
restoreCandidates = updateCandidates (PartialIndex Map.empty Nothing) Map.empty

-- We keep the partial packages separate from the rest of the candidate info
-- so that we can reuse more of CoreBackup
updateCandidates :: PartialIndex -> PartialCandidates -> RestoreBackup CandidatePackages
updateCandidates packageIndex@(PartialIndex packageMap _) candidateMap =
  RestoreBackup {
    restoreEntry = \entry -> do
      packageIndex'   <- doPackageImport   packageIndex   entry
      candidateMap' <- doCandidateImport candidateMap entry
      return (updateCandidates packageIndex' candidateMap')
  , restoreFinalize = do
      let combined = combineMaps packageMap candidateMap
      results <- mapM mkCandidate (Map.toList combined)
      return $ CandidatePackages {
          candidateList = PackageIndex.fromList results
          -- If we restore from a back up there can be no pending transactions
          -- so nothing to migrate
        , candidateMigratedPkgTarball = True
        }
  }
  where
    mkCandidate :: (PackageId, (Maybe PartialPkg, Maybe PartialCandidate)) -> Restore CandPkgInfo
    mkCandidate (pkgId, (Just partialPkg, Just partialCand)) = do
      pkg <- CoreBackup.partialToFullPkg (pkgId, partialPkg)
      return CandPkgInfo { candPkgInfo  = pkg
                         , candWarnings = partialWarnings partialCand
                         , candPublic   = partialPublic   partialCand
                         }
    mkCandidate (pkgId, (Nothing, Just _)) =
      fail $ show pkgId ++ ": candidate.csv without corresponding package"
    mkCandidate (pkgId, (Just _, Nothing)) =
      fail $ show pkgId ++ ": missing candidate.csv"
    mkCandidate _ =
      fail "mkCandidate: the impossible happened"

combineMaps :: Ord k => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
combineMaps map1 map2 =
  let map1' = Map.map (\x -> (Just x, Nothing)) map1
      map2' = Map.map (\y -> (Nothing, Just y)) map2
  in Map.unionWith (\e1 e2 -> (fst e1, snd e2)) map1' map2'

doCandidateImport :: PartialCandidates -> BackupEntry -> Restore PartialCandidates
doCandidateImport candidates (BackupByteString ["package", pkgStr, "candidate.csv"] bs) = do
  pkgId <- CoreBackup.parsePackageId pkgStr
  csv <- importCSV "candidate.csv" bs
  partial <- case csv of
    [_version, ["public", public], "warnings" : warnings] ->
      return PartialCandidate { partialWarnings = warnings
                              , partialPublic   = read public
                              }
    _ ->
      fail "candidate.csv has an invalid format"
  return (Map.insert pkgId partial candidates)
doCandidateImport candidates _ =
  return candidates

{-------------------------------------------------------------------------------
  Create backup
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
