module Distribution.Server.Features.Distro.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Distro.State as State
import qualified Distribution.Server.Features.Distro.Store as Store
import Distribution.Server.Features.Distro.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Framework

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  distrosState <- distrosStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.queryDistroMaintainers = \name -> queryState distrosState (State.GetDistroMaintainers name)
        , Store.addDistroMaintainer = \name uid -> updateState distrosState (State.AddDistroMaintainer name uid)
        , Store.removeDistroMaintainer = \name uid -> updateState distrosState (State.RemoveDistroMaintainer name uid)
        , Store.enumerateDistros = queryState distrosState State.EnumerateDistros
        , Store.queryPackageStatus = \pkgname -> queryState distrosState (State.PackageStatus pkgname)
        , Store.queryDistroStatus = \name -> queryState distrosState (State.DistroStatus name)
        , Store.isDistribution = \name -> queryState distrosState (State.IsDistribution name)
        , Store.queryDistroPackageStatus = \name pkgname -> queryState distrosState (State.DistroPackageStatus name pkgname)
        , Store.removeDistro = \name -> updateState distrosState (State.RemoveDistro name)
        , Store.dropDistroPackage = \name pkgname -> updateState distrosState (State.DropPackage name pkgname)
        , Store.addDistroPackage = \name pkgname info -> updateState distrosState (State.AddPackage name pkgname info)
        , Store.addDistro = \name -> updateState distrosState (State.AddDistro name)
        , Store.putDistroPackageList = \name pkgs -> updateState distrosState (State.PutDistroPackageList name pkgs)
        }
    , Store.backendState = [abstractAcidStateComponent distrosState]
    }

distrosStateComponent :: FilePath -> IO (StateComponent AcidState State.Distros)
distrosStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Distros") State.initialDistros
  return StateComponent {
      stateDesc    = ""
    , stateHandle  = st
    , getState     = query st State.GetDistributions
    , putState     = \(State.Distros dists versions) -> update st (State.ReplaceDistributions dists versions)
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = distrosStateComponent
    }
