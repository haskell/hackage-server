module Distribution.Server.Features.Vouch.Acid
  ( acidStore
  ) where

import qualified Distribution.Server.Features.Vouch.State as Acid
import qualified Distribution.Server.Features.Vouch.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore (RestoreBackup(..))


acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  vouchState <- vouchStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getVouchesFor = \uid -> queryState vouchState (Acid.GetVouchesFor uid)
        , Store.addVouchIfAllowed = \vouchee voucher now voucherIsUploader voucheeIsUploader ->
            updateState vouchState (Acid.AddVouchIfAllowed vouchee voucher now voucherIsUploader voucheeIsUploader)
        , Store.drainQueuedNotifications = updateState vouchState Acid.DrainQueuedNotifications
        }
    , Store.backendState = [abstractAcidStateComponent vouchState]
    }

vouchStateComponent :: FilePath -> IO (StateComponent AcidState Acid.VouchData)
vouchStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Vouch") (Acid.VouchData mempty mempty)
  let initialVouchData = Acid.VouchData mempty mempty
      restore =
        RestoreBackup
          { restoreEntry = error "Unexpected backup entry"
          , restoreFinalize = return initialVouchData
          }
  pure StateComponent
    { stateDesc = "Keeps track of vouches"
    , stateHandle = st
    , getState = query st Acid.GetVouchesData
    , putState = update st . Acid.ReplaceVouchesData
    , backupState = \_ _ -> []
    , restoreState = restore
    , resetState = vouchStateComponent
    }
