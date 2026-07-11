module Distribution.Server.Features.TarIndexCache.Acid
  ( acidStore
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Features.TarIndexCache.Store
import Distribution.Server.Features.TarIndexCache.State as Acid

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  state <- tarIndexCacheStateComponent stateDir
  let st = stateHandle state
  pure Backend {
      backendStore = Store {
          getTarIndexCache     = query st Acid.GetTarIndexCache
        , replaceTarIndexCache  = update st . Acid.ReplaceTarIndexCache
        , findTarIndex          = query st . Acid.FindTarIndex
        , setTarIndex           = \tar index -> update st (Acid.SetTarIndex tar index)
        }
    , backendState = [abstractAcidStateComponent' (\_ _ -> []) state]
    }

tarIndexCacheStateComponent :: FilePath -> IO (StateComponent AcidState Acid.TarIndexCache)
tarIndexCacheStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "TarIndexCache") Acid.initialTarIndexCache
  return StateComponent {
      stateDesc    = "Mapping from tarball blob IDs to tarindex blob IDs"
    , stateHandle  = st
    , getState     = query st Acid.GetTarIndexCache
    , putState     = update st . Acid.ReplaceTarIndexCache
    , resetState   = tarIndexCacheStateComponent
    -- We don't backup the tar indices, but reconstruct them on demand
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "The impossible happened"
                       , restoreFinalize = return Acid.initialTarIndexCache
                       }
    }
