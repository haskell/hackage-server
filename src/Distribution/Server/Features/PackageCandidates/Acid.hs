module Distribution.Server.Features.PackageCandidates.Acid
  ( candidatesStateComponent
  ) where

import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Backup
import Distribution.Server.Framework

candidatesStateComponent :: Bool -> FilePath -> IO (StateComponent AcidState CandidatePackages)
candidatesStateComponent freshDB stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "CandidatePackages")
                           (initialCandidatePackages freshDB)
  return StateComponent {
      stateDesc    = "Candidate packages"
    , stateHandle  = st
    , getState     = query st GetCandidatePackages
    , putState     = update st . ReplaceCandidatePackages
    , resetState   = candidatesStateComponent True
    , backupState  = \_ -> backupCandidates
    , restoreState = restoreCandidates
  }
