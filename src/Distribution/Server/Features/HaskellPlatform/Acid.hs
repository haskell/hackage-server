{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Server.Features.HaskellPlatform.Acid
  ( acidStore
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import qualified Distribution.Server.Features.HaskellPlatform.State as Acid
import Distribution.Server.Features.HaskellPlatform.Store

import qualified Data.Map as Map
import qualified Data.Set as Set

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  platformState <- platformStateComponent stateDir
  pure Backend
    { backendStore = Store
        { platformVersions = \pkgname -> fmap Set.toList $ queryState platformState $ Acid.GetPlatformPackage pkgname
        , platformPackageLatest = fmap (Map.toList . Map.map Set.findMax . Acid.blessedPackages) $ queryState platformState Acid.GetPlatformPackages
        , setPlatform = \pkgname versions -> updateState platformState $ Acid.SetPlatformPackage pkgname (Set.fromList versions)
        , removePlatform = \pkgname -> updateState platformState $ Acid.SetPlatformPackage pkgname Set.empty
        }
    , backendState = [abstractAcidStateComponent platformState]
    }

platformStateComponent :: FilePath -> IO (StateComponent AcidState Acid.PlatformPackages)
platformStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Acid.PlatformPackages") Acid.initialPlatformPackages
  return StateComponent {
      stateDesc    = "Platform packages"
    , stateHandle  = st
    , getState     = query st Acid.GetPlatformPackages
    , putState     = update st . Acid.ReplacePlatformPackages
    , resetState   = platformStateComponent
    -- TODO: backup
    -- For now backup is just empty, as this package is basically featureless
    -- It defines state, but there is no way at all to modify this state
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry for platform"
                       , restoreFinalize = return Acid.initialPlatformPackages
                       }
    }
