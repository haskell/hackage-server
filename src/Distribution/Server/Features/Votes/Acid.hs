module Distribution.Server.Features.Votes.Acid
  ( acidStore
  , votesStateComponent
  ) where

import Distribution.Server.Features.Votes.Store
import qualified Distribution.Server.Features.Votes.State as Acid

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import qualified Data.Map as Map

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  votesState <- votesStateComponent stateDir
  return Backend {
    backendStore = Store {
        getAllPackageVoteSets = queryState votesState Acid.GetAllPackageVoteSets
      , addVote               = \pkgname uid score -> updateState votesState (Acid.AddVote pkgname uid score)
      , removeVote            = \pkgname uid -> updateState votesState (Acid.RemoveVote pkgname uid)
      , getPackageVoteCount   = \pkgname -> queryState votesState (Acid.GetPackageVoteCount pkgname)
      , getPackageVoteScore   = \pkgname -> queryState votesState (Acid.GetPackageVoteScore pkgname)
      , getPackageUserVoted   = \pkgname uid -> queryState votesState (Acid.GetPackageUserVoted pkgname uid)
      , getPackageUserVote    = \pkgname uid -> queryState votesState (Acid.GetPackageUserVote pkgname uid)
      }
    , backendState = [abstractAcidStateComponent votesState]
    }

-- | Define the backing store (i.e. database component)
votesStateComponent :: FilePath -> IO (StateComponent AcidState Acid.VotesState)
votesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Votes") Acid.initialVotesState
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who voted for it"
    , stateHandle  = st
    , getState     = query st Acid.GetVotesState
    , putState     = update st . Acid.ReplaceVotesState
    , resetState   = votesStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return $ Acid.VotesState Map.empty
                       }
   }
