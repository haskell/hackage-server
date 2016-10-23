{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.Votes
  ( VotesFeature(..)
  , initVotesFeature
  ) where

import Distribution.Server.Features.Votes.State
import qualified Distribution.Server.Features.Votes.Render as Render

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Users.Types (UserId(..))

import Distribution.Package
import Distribution.Text

import Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap

import Control.Monad (when)
import Control.Arrow (first)
import qualified Text.XHtml.Strict as X


-- | Define the prototype for this feature
data VotesFeature = VotesFeature {
    votesFeatureInterface   :: HackageFeature
  , didUserVote             :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , pkgNumVotes             :: forall m. MonadIO m => PackageName -> m Int
  , pkgNumScore             :: forall m. MonadIO m => PackageName -> m Float
  , pkgUserVote             :: forall m. MonadIO m => PackageName -> UserId -> m (Maybe Score)
  , votesUpdated            :: Hook (PackageName, Float) ()
  , renderVotesHtml         :: PackageName -> ServerPartE X.Html
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
  updateVotes       <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = votesFeature env
                  dbVotesState
                  coref userf updateVotes

    return feature

-- | Define the backing store (i.e. database component)
votesStateComponent :: FilePath -> IO (StateComponent AcidState VotesState)
votesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Votes") initialVotesState
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who voted for it"
    , stateHandle  = st
    , getState     = query st GetVotesState
    , putState     = update st . ReplaceVotesState
    , resetState   = votesStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return $ VotesState Map.empty
                       }
   }


-- | Default constructor for building this feature.
votesFeature ::  ServerEnv
             -> StateComponent AcidState VotesState
             -> CoreFeature                    -- To get site package list
             -> UserFeature                    -- To authenticate users
             -> Hook (PackageName, Float) ()
             -> VotesFeature

votesFeature  ServerEnv{..}
              votesState
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              votesUpdated
  = VotesFeature{..}
  where
    votesFeatureInterface   = (emptyHackageFeature "votes") {
        featureDesc      = "Allow users to upvote packages",
        featureResources = [ packagesVotesResource
                           , packageVotesResource
                           ]
      , featureState     = [abstractAcidStateComponent votesState]
      }


    -- Define resources for this feature's URIs

    packagesVotesResource :: Resource
    packagesVotesResource = (resourceAt "/packages/votes.:format") {
      resourceDesc  = [(GET,    "Returns the number of votes for each package")]
    , resourceGet   = [("json", servePackageVotesGet)]
    }

    packageVotesResource :: Resource
    packageVotesResource = (resourceAt "/package/:package/votes.:format") {
      resourceDesc    = [ (GET,     "Returns the number of votes a package has")
                        , (PUT,     "Adds a vote to this package")
                        , (DELETE,  "Remove a user's vote from this package")
                        ]
    , resourceGet     = [("json", servePackageNumVotesGet)]
    , resourcePost     = [("",     servePackageVotePut)]
    , resourceDelete  = [("",     servePackageVoteDelete)]
    }

    -- Implementations of the how the above resources are handled.

    -- Retrive the entire map (from package names -> # of votes)
    servePackageVotesGet :: DynamicPath -> ServerPartE Response
    servePackageVotesGet _ = do
      cacheControlWithoutETag [Public, maxAgeMinutes 10]
      votesMap <- queryState votesState GetAllPackageVoteSets
      ok . toResponse $ objectL
        [ (display pkgname, toJSON (votesScore pkgMap)) | (pkgname, pkgMap) <- Map.toList votesMap ]

    -- Get the number of votes a package has. If the package
    -- has never been voted for, returns 0.
    servePackageNumVotesGet :: DynamicPath -> ServerPartE Response
    servePackageNumVotesGet dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      cacheControlWithoutETag [Public, maxAgeMinutes 10]
      voteCount <- pkgNumVotes pkgname
      let obj = objectL
                  [ ("packageName", string $ display pkgname)
                  , ("numVotes",    toJSON voteCount)
                  ]
      ok . toResponse $ obj

    -- Add a vote to :packageName (must match name exactly)
    servePackageVotePut :: DynamicPath -> ServerPartE Response
    servePackageVotePut dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      score <- look "score"
      let score' = read score :: Int
      _ <- updateState votesState (AddVote pkgname uid score')
      pkgScore <- pkgNumScore pkgname
      runHook_ votesUpdated (pkgname, pkgScore)
      ok . toResponse $ "Package voted for successfully"

    -- Removes a user's vote from a package. If the user has not voted
    -- for this package, does nothing.
    servePackageVoteDelete :: DynamicPath -> ServerPartE Response
    servePackageVoteDelete dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      success <- updateState votesState (RemoveVote pkgname uid)
      pkgScore <- pkgNumScore pkgname
      when success $ runHook_ votesUpdated (pkgname, pkgScore)
      let responseMsg | success   = "Package vote removed successfully."
                      | otherwise = "User has not voted for this package."
      ok . toResponse $ responseMsg

    -- Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously voted for the
    -- package in question.
    didUserVote :: MonadIO m => PackageName -> UserId -> m Bool
    didUserVote pkgname uid =
      queryState votesState (GetPackageUserVoted pkgname uid)

    -- Returns the number of votes a package has.
    pkgNumVotes :: MonadIO m => PackageName -> m Int
    pkgNumVotes pkgname =
      queryState votesState (GetPackageVoteCount pkgname)

    pkgNumScore :: MonadIO m => PackageName -> m Float
    pkgNumScore pkgname =
      queryState votesState (GetPackageVoteScore pkgname)

    pkgUserVote :: MonadIO m => PackageName -> UserId -> m (Maybe Score)
    pkgUserVote pkgname uid =
      queryState votesState (GetPackageUserVote pkgname uid)

    -- Renders the HTML for the "Votes:" section on package pages.
    renderVotesHtml :: PackageName -> ServerPartE X.Html
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
