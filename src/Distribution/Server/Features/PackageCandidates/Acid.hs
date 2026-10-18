module Distribution.Server.Features.PackageCandidates.Acid
  ( candidatesStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.PackageCandidates.State as State
import Distribution.Server.Features.PackageCandidates.Backup
import qualified Distribution.Server.Features.PackageCandidates.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Features.PackageCandidates.Types (CandPkgInfo)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  candidatesState <- candidatesStateComponent False stateDir
  let withCandidates :: MonadIO m => (PackageIndex.PackageIndex CandPkgInfo -> a) -> m a
      withCandidates f = f . State.candidateList <$> queryState candidatesState State.GetCandidatePackages
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupCandidateName = \pkgname -> withCandidates (`PackageIndex.lookupPackageName` pkgname)
        , Store.lookupCandidateId = \pkgid -> withCandidates (`PackageIndex.lookupPackageId` pkgid)
        , Store.allCandidates = withCandidates PackageIndex.allPackages
        , Store.candidateTarballIsMigrated = State.candidateMigratedPkgTarball <$> queryState candidatesState State.GetCandidatePackages
        , Store.setCandidateTarballMigrated = updateState candidatesState State.SetMigratedPkgTarball
        , Store.addCandidate = \candidate -> updateState candidatesState (State.AddCandidate candidate)
        , Store.deleteCandidate = \pkgid -> updateState candidatesState (State.DeleteCandidate pkgid)
        , Store.deleteCandidates = \pkgname -> updateState candidatesState (State.DeleteCandidates pkgname)
        , Store.updateCandidatePkgInfo = \pkgid pkginfo -> updateState candidatesState (State.UpdateCandidatePkgInfo pkgid pkginfo)
        }
    , Store.backendState = [abstractAcidStateComponent candidatesState]
    }

candidatesStateComponent :: Bool -> FilePath -> IO (StateComponent AcidState State.CandidatePackages)
candidatesStateComponent freshDB stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "CandidatePackages")
                           (State.initialCandidatePackages freshDB)
  return StateComponent {
      stateDesc    = "Candidate packages"
    , stateHandle  = st
    , getState     = query st State.GetCandidatePackages
    , putState     = update st . State.ReplaceCandidatePackages
    , resetState   = candidatesStateComponent True
    , backupState  = \_ -> backupCandidates
    , restoreState = restoreCandidates
  }
