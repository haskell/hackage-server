module Distribution.Server.Features.Tags.Acid
  ( tagsStateComponent
  , tagsAliasComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Tags.State as State
import qualified Distribution.Server.Features.Tags.Store as Store
import Distribution.Server.Features.Tags.Backup
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  tagsState <- tagsStateComponent stateDir
  tagAlias <- tagsAliasComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTagList = queryState tagsState State.GetTagList
        , Store.getTagsForPackage = \pkgname -> queryState tagsState (State.TagsForPackage pkgname)
        , Store.getReviewTagsForPackage = \pkgname -> queryState tagsState (State.LookupReviewTags pkgname)
        , Store.getTagAlias = \tag -> queryState tagAlias (State.GetTagAlias tag)
        , Store.getPackagesForTag = \tag -> queryState tagsState (State.PackagesForTag tag)
        , Store.getPackageTags = queryState tagsState State.GetPackageTags
        , Store.setPackageTags = \pkgname tags -> updateState tagsState (State.SetPackageTags pkgname tags)
        , Store.setTagPackages = \tag pkgs -> updateState tagsState (State.SetTagPackages tag pkgs)
        , Store.addTagAlias = \tag alias -> updateState tagAlias (State.AddTagAlias tag alias)
        , Store.insertReviewTags = \pkgname add del -> updateState tagsState (State.InsertReviewTags pkgname add del)
        , Store.replaceReviewTags = \pkgname add del -> updateState tagsState (State.InsertReviewTags' pkgname add del)
        }
    , Store.backendState = [ abstractAcidStateComponent tagsState
                           , abstractAcidStateComponent tagAlias
                           ]
    }

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
