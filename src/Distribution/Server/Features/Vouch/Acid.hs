module Distribution.Server.Features.Vouch.Acid
  ( vouchStateComponent
  ) where

import qualified Distribution.Server.Features.Vouch.State as Acid
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore (RestoreBackup(..))

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
