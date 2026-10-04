module Distribution.Server.Features.AnalyticsPixels.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.AnalyticsPixels.State as Acid
import Distribution.Server.Features.AnalyticsPixels.Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  analyticsPixelsState <- analyticsPixelsStateComponent stateDir
  return Backend {
      backendStore = Store {
        getPackageAnalyticsPixels = \name -> queryState analyticsPixelsState (Acid.AnalyticsPixelsForPackage name)
      , addPackageAnalyticsPixel = \name pixel -> updateState analyticsPixelsState (Acid.AddPackageAnalyticsPixel name pixel)
      , removePackageAnalyticsPixel = \name pixel -> updateState analyticsPixelsState (Acid.RemovePackageAnalyticsPixel name pixel)
      }
    , backendState = [abstractAcidStateComponent analyticsPixelsState]
    }

-- | Define the backing store (i.e. database component)
analyticsPixelsStateComponent :: FilePath -> IO (StateComponent AcidState Acid.AnalyticsPixelsState)
analyticsPixelsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AnalyticsPixels") Acid.initialAnalyticsPixelsState
  return StateComponent {
      stateDesc    = "Backing store for AnalyticsPixels feature"
    , stateHandle  = st
    , getState     = query st Acid.GetAnalyticsPixelsState
    , putState     = update st . Acid.ReplaceAnalyticsPixelsState
    , resetState   = analyticsPixelsStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return Acid.initialAnalyticsPixelsState
                       }
   }
