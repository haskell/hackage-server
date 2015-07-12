{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.Votes
  ( VotesFeature(..)
  , initVotesFeature
  ) where

import Distribution.Server.Features.Votes.Types
  ( Votes(..)
  , initialVotes
  , getNumberOfVotesFor
  , enumerate
  )

import Distribution.Server.Framework as F

import Distribution.Package
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Users.Types (UserId(..))

import qualified Distribution.Server.Features.Votes.State as RState
import qualified Distribution.Server.Features.Votes.Types as RTypes
import qualified Distribution.Server.Features.Votes.Render as Render

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap

import Control.Arrow (first)
import qualified Text.XHtml.Strict as X


-- | Define the prototype for this feature
data VotesFeature = VotesFeature {
    votesFeatureInterface :: HackageFeature
  , didUserVote             :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , pkgNumVotes             :: forall m. MonadIO m => PackageName -> m Int
  , renderVotesHtml         :: PackageName -> ServerPartE (String, X.Html)
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature VotesFeature where
  getFeatureInterface = votesFeatureInterface

-- | Called from Features.hs to initialize this feature
initVotesFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> UserFeature
                      -> IO VotesFeature)
initVotesFeature env@ServerEnv{serverStateDir} = do
  dbVotesState      <- votesStateComponent serverStateDir

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = votesFeature env
                  dbVotesState
                  coref userf
    return feature

-- | Define the backing store (i.e. database component)
votesStateComponent :: FilePath -> IO (StateComponent AcidState Votes)
votesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Votes") initialVotes
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who voted for it"
    , stateHandle  = st
    , getState     = query st RState.DbGetVotes
    , putState     = F.update st . RState.DbReplaceVotes
    , resetState   = votesStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return $ Votes Map.empty
                       }
   }


-- | Default constructor for building this feature.
votesFeature ::  ServerEnv
                -> StateComponent AcidState Votes
                -> CoreFeature                    -- To get site package list
                -> UserFeature                    -- To authenticate users
                -> VotesFeature

votesFeature  ServerEnv{..}
                votesState
                CoreFeature {
                  coreResource = CoreResource { packageInPath
                                              , guardValidPackageName
                                              }
                }
                UserFeature{..}
  = VotesFeature{..}
  where
    votesFeatureInterface   = (emptyHackageFeature "votes") {
        featureResources = [ packagesVotesResource
                           , packageVotesResource
                           ]
      , featureState     = [abstractAcidStateComponent votesState]
      }


    -- Define resources for this feature's URIs

    packagesVotesResource :: Resource
    packagesVotesResource = (resourceAt "/packages/votes.:format") {
      resourceDesc  = [(GET,    "Returns the entire database of package votes.")]
    , resourceGet   = [("json", servePackageVotesGet)]
    }

    packageVotesResource :: Resource
    packageVotesResource = (resourceAt "/package/:package/votes.:format") {
      resourceDesc    = [ (GET,     "Returns the number of votes a package has.")
                        , (PUT,     "Adds a vote to this package.")
                        , (DELETE,  "Remove a user's vote from this package.")
                        ]
    , resourceGet     = [("json", servePackageNumVotesGet)]
    , resourcePut     = [("",     servePackageVotePut)]
    , resourceDelete  = [("",     servePackageVoteDelete)]
    }

    -- Implementations of the how the above resources are handled.

    -- Retrive the entire map (from package names -> # of votes)
    -- (Must be authenticated as an admin.)
    servePackageVotesGet :: DynamicPath -> ServerPartE Response
    servePackageVotesGet _ = do
      guardAuthorised [InGroup adminGroup]
      dbVotesMap <- queryState votesState RState.DbGetVotes
      ok . toResponse $ toJSON $ enumerate dbVotesMap

    -- Get the number of votes a package has. If the package
    -- has never been voted for, returns 0.
    servePackageNumVotesGet :: DynamicPath -> ServerPartE Response
    servePackageNumVotesGet dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      dbVotesMap <- queryState votesState RState.DbGetVotes

      let numVotes = getNumberOfVotesFor pkgname dbVotesMap
          arr = objectL
                  [ ("packageName", string $ unPackageName pkgname)
                  , ("numVotes",    toJSON numVotes)
                  ]
      ok . toResponse $ toJSON arr

    -- Add a vote to :packageName (must match name exactly)
    servePackageVotePut :: DynamicPath -> ServerPartE Response
    servePackageVotePut dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      alreadyVoted <- didUserVote pkgname uid

      case alreadyVoted of
        True ->
          ok . toResponse $ Render.alreadyVotedPage pkgname
        False -> do
          updateState votesState $ RState.DbAddVote pkgname uid
          ok . toResponse $ Render.voteConfirmationPage
            pkgname "Package voted for successfully"

    -- Removes a user's vote from a package. If the user has not
    -- not voted for this package, does nothing.
    servePackageVoteDelete :: DynamicPath -> ServerPartE Response
    servePackageVoteDelete dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      updateState votesState $ RState.DbRemoveVote pkgname uid

      alreadyVoted <- didUserVote pkgname uid
      let responseMsg = case alreadyVoted of
            True  -> "User has not voted for this package."
            False -> "Package unvoted successfully."
      ok . toResponse $ Render.voteConfirmationPage
        pkgname responseMsg

    -- Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously voted for the
    -- package in question.
    didUserVote :: MonadIO m => PackageName -> UserId -> m Bool
    didUserVote pkgname uid = do
      dbVotesMap <- queryState votesState RState.DbGetVotes
      return $ RTypes.askUserVoted pkgname uid dbVotesMap

    -- Returns the number of votes a package has.
    pkgNumVotes :: MonadIO m => PackageName -> m Int
    pkgNumVotes pkgname =  do
      dbVotesMap <- queryState votesState RState.DbGetVotes
      return $ getNumberOfVotesFor pkgname dbVotesMap

    -- Renders the HTML for the "Votes:" section on package pages.
    renderVotesHtml :: PackageName -> ServerPartE (String, X.Html)
    renderVotesHtml pkgname = do
      numVotes <- pkgNumVotes pkgname
      return $ Render.renderVotesAnon numVotes pkgname


-- Helper functions for constructing JSON responses.

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . HashMap.fromList . map (first T.pack)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack
