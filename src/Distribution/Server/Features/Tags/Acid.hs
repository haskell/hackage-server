module Distribution.Server.Features.Tags.Acid
  ( tagsStateComponent
  , tagsAliasComponent
  ) where

import qualified Distribution.Server.Features.Tags.State as State
import Distribution.Server.Features.Tags.Backup
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

tagsStateComponent :: FilePath -> IO (StateComponent AcidState State.PackageTags)
tagsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Tags" </> "Existing") State.initialPackageTags
  return StateComponent {
      stateDesc    = "Package tags"
    , stateHandle  = st
    , getState     = query st State.GetPackageTags
    , putState     = update st . State.ReplacePackageTags
    , backupState  = \_ pkgTags -> [csvToBackup ["tags.csv"] $ tagsToCSV pkgTags]
    , restoreState = tagsBackup
    , resetState   = tagsStateComponent
    }

tagsAliasComponent :: FilePath -> IO (StateComponent AcidState State.TagAlias)
tagsAliasComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Tags" </> "Alias") State.emptyTagAlias
  return StateComponent {
      stateDesc    = "Tags Alias"
    , stateHandle  = st
    , getState     = query st State.GetTagAliasesState
    , putState     = update st . State.AddTagAliasesState
    , backupState  = \_ aliases -> [csvToBackup ["aliases.csv"] $ aliasToCSV aliases]
    , restoreState = aliasBackup
    , resetState   = tagsAliasComponent
    }
