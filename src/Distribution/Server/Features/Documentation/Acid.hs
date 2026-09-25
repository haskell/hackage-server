module Distribution.Server.Features.Documentation.Acid
  ( documentationStateComponent
  ) where

import qualified Distribution.Server.Features.Documentation.State as State
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BlobStorage (BlobId)

import Distribution.Package (PackageId)
import Distribution.Text (display, simpleParse)
import qualified Data.Map as Map

documentationStateComponent :: String -> FilePath -> IO (StateComponent AcidState State.Documentation)
documentationStateComponent name stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> name) State.initialDocumentation
  return StateComponent {
      stateDesc    = "Package documentation"
    , stateHandle  = st
    , getState     = query st State.GetDocumentation
    , putState     = update st . State.ReplaceDocumentation
    , backupState  = \_ -> dumpBackup
    , restoreState = updateDocumentation (State.Documentation Map.empty)
    , resetState   = documentationStateComponent name
    }
  where
    dumpBackup doc =
        let exportFunc (pkgid, blob) = BackupBlob [display pkgid, "documentation.tar"] blob
        in map exportFunc . Map.toList $ State.documentation doc

    updateDocumentation :: State.Documentation -> RestoreBackup State.Documentation
    updateDocumentation docs = RestoreBackup {
        restoreEntry = \entry ->
          case entry of
            BackupBlob [str, "documentation.tar"] blobId | Just pkgId <- simpleParse str -> do
              docs' <- importDocumentation pkgId blobId docs
              return (updateDocumentation docs')
            _ ->
              return (updateDocumentation docs)
      , restoreFinalize = return docs
      }

    importDocumentation :: PackageId -> BlobId -> State.Documentation -> Restore State.Documentation
    importDocumentation pkgId blobId (State.Documentation docs) =
      return (State.Documentation (Map.insert pkgId blobId docs))
