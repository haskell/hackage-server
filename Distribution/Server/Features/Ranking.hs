{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, LambdaCase #-}

-- | Implements a ranking system for all packages based on
-- | stars/upvotes supplied by users.
module Distribution.Server.Features.Ranking
  ( RankingFeature(..)
  , initRankingFeature
  ) where

import Distribution.Server.Features.Ranking.Types
  ( Stars(..)
  , initialStars
  , getNumberOfStarsFor
  , enumerate
  )

import Distribution.Server.Framework as F

import Distribution.Package
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Users.Types (UserId(..))

import qualified Distribution.Server.Features.Ranking.State as RState
import qualified Distribution.Server.Features.Ranking.Types as RTypes
import qualified Distribution.Server.Features.Ranking.Render as Render

import Data.Aeson
import Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap

import Control.Arrow (first)
import qualified Text.XHtml.Strict as X


-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
    rankingFeatureInterface :: HackageFeature
  , didUserStar             :: forall m. MonadIO m => PackageName -> UserId -> m Bool
  , pkgNumStars             :: forall m. MonadIO m => PackageName -> m Int
  , renderStarsHtml         :: PackageName -> ServerPartE (String, X.Html)
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature RankingFeature where
  getFeatureInterface = rankingFeatureInterface

-- | Called from Features.hs to initialize this feature
initRankingFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> UserFeature
                      -> IO RankingFeature)
initRankingFeature env@ServerEnv{serverStateDir} = do
  dbStarsState      <- starsStateComponent serverStateDir

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature env
                  dbStarsState
                  coref userf
    return feature

-- | Define the backing store (i.e. database component)
starsStateComponent :: FilePath -> IO (StateComponent AcidState Stars)
starsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Stars") initialStars
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who starred it"
    , stateHandle  = st
    , getState     = query st RState.DbGetStars
    , putState     = F.update st . RState.DbReplaceStars
    , resetState   = starsStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return $ Stars {extractMap = Map.empty}
                       }
   }


-- | Default constructor for building this feature.
rankingFeature ::  ServerEnv
                -> StateComponent AcidState Stars
                -> CoreFeature                    -- To get site package list
                -> UserFeature                    -- To authenticate users
                -> RankingFeature

rankingFeature  ServerEnv{..}
                starsState
                CoreFeature {
                  coreResource = CoreResource { packageInPath
                                              , guardValidPackageName
                                              }
                }
                UserFeature{..}
  = RankingFeature{..}
  where
    rankingFeatureInterface   = (emptyHackageFeature "Package stars") {
        featureResources      = [ getEntireMapResource
                                , handleStarsResource
                                , allUsersWhoStarredResource
          ]
      , featureState          = [abstractAcidStateComponent starsState]
      }


-- | Define resources for this feature's URIs

    getEntireMapResource :: Resource
    getEntireMapResource = (resourceAt "/stars") {
      resourceDesc  = [(GET,    "Returns the entire database of package stars.")]
    , resourceGet   = [("json", getPackageStarMap)]
    }

    handleStarsResource :: Resource
    handleStarsResource = (resourceAt "/package/:package/stars") {
      resourceDesc    = [ (GET,     "Returns the number of stars a package has.")
                        , (PUT,     "Adds a star to this package.")
                        , (DELETE,  "Remove a user's star from this package.")
                        ]
    , resourceGet     = [("json", getPackageNumStars)]
    , resourcePut     = [("",     addStar)]
    , resourceDelete  = [("",     removeStar)]
    }

    allUsersWhoStarredResource :: Resource
    allUsersWhoStarredResource = (resourceAt "/package/:package/stars/users") {
      resourceDesc  = [(GET, "Returns all of the users who have starred this package.")]
    , resourceGet   = [("",     getAllPackageStars)]
    }

-- | Implementations of the how the above resources are handled.

    -- Add a star to :packageName (must match name exactly)
    addStar :: DynamicPath -> ServerPartE Response
    addStar dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      alreadyStarred <- didUserStar pkgname uid

      case alreadyStarred of
        True ->
          ok . toResponse $ Render.alreadyStarredPage pkgname
        False -> do
          updateState starsState $ RState.DbAddStar pkgname uid
          ok . toResponse $ Render.starConfirmationPage
            pkgname "Package starred successfully"

    -- Removes a user's star from a package. If the user has not
    -- not starred this package, does nothing.
    removeStar :: DynamicPath -> ServerPartE Response
    removeStar dpath = do
      uid     <- guardAuthorised [AnyKnownUser]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      updateState starsState $ RState.DbRemoveStar pkgname uid

      alreadyStarred <- didUserStar pkgname uid
      let responseMsg = case alreadyStarred of
            True -> "User has not starred this package."
            False -> "Package unstarred successfully."
      ok . toResponse $ Render.starConfirmationPage
        pkgname responseMsg

    -- Retrive the entire map (from package names -> # of stars)
    -- (Must be authenticated as an admin.)
    getPackageStarMap :: DynamicPath -> ServerPartE Response
    getPackageStarMap _ = do
      guardAuthorised [InGroup adminGroup]
      dbStarsMap <- queryState starsState RState.DbGetStars
      ok. toResponse $ toJSON $ enumerate dbStarsMap

    -- Get the number of stars a package has. If the package
    -- has never been starred, returns 0.
    getPackageNumStars :: DynamicPath -> ServerPartE Response
    getPackageNumStars dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      dbStarsMap <- queryState starsState RState.DbGetStars

      let numStars = getNumberOfStarsFor pkgname dbStarsMap
          arr = objectL
                  [ ("packageName", string $ unPackageName pkgname)
                  , ("numStars",    toJSON numStars)
                  ]
      ok . toResponse $ toJSON arr

    getAllPackageStars :: DynamicPath -> ServerPartE Response
    getAllPackageStars dpath = do
      guardAuthorised [InGroup adminGroup]
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      dbStarsMap <- queryState starsState RState.DbGetStars

      let users = RTypes.getUsersWhoStarred pkgname dbStarsMap
          arr = Set.toList $ users

      ok . toResponse $ toJSON arr

-- | Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously starred the
    -- package in question.
    didUserStar :: MonadIO m => PackageName -> UserId -> m Bool
    didUserStar pkgname uid = do
      dbStarsMap <- queryState starsState RState.DbGetStars
      return $ RTypes.askUserStarred pkgname uid dbStarsMap

    -- Returns the number of stars a package has.
    pkgNumStars :: MonadIO m => PackageName -> m Int
    pkgNumStars pkgname =  do
      dbStarsMap <- queryState starsState RState.DbGetStars
      return $ getNumberOfStarsFor pkgname dbStarsMap

    -- Renders the HTML for the "Stars:" section on package pages.
    renderStarsHtml :: PackageName -> ServerPartE (String, X.Html)
    renderStarsHtml pkgname = do
      numStars <- pkgNumStars pkgname
      return $ Render.renderStarsAnon numStars pkgname


-- | Helper functions for constructing JSON responses.

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . HashMap.fromList . L.map (first T.pack)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack
