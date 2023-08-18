{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Distribution.Server.Features.UserNotify (
    NotifyData(..),
    NotifyPref(..),
    NotifyRevisionRange,
    NotifyTriggerBounds(..),
    UserNotifyFeature(..),
    defaultNotifyPrefs,
    getUserNotificationsOnRelease,
    importNotifyPref,
    initUserNotifyFeature,
    notifyDataToCSV,
  ) where

import Prelude hiding (lookup)
import Distribution.Package
import Distribution.Pretty
import Distribution.Version

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Group
import Distribution.Server.Users.Types (UserId, UserInfo (..))
import Distribution.Server.Users.UserIdSet as UserIdSet

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.AdminLog
import Distribution.Server.Features.BuildReports
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.Core
import Distribution.Server.Features.ReverseDependencies (ReverseFeature(..))
import Distribution.Server.Features.ReverseDependencies.State (NodeId, ReverseIndex(..), suc)
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Upload
import Distribution.Server.Features.UserDetails
import Distribution.Server.Features.Users

import Distribution.Server.Util.Email

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent (threadDelay)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Bifunctor (Bifunctor(second))
import Data.Bimap (lookup, lookupR)
import Data.Graph (Vertex)
import Data.Hashable (Hashable(..))
import Data.List (maximumBy, sortOn)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down(..), comparing)
import Data.SafeCopy (Migrate(migrate), MigrateFrom, base, deriveSafeCopy, extension)
import Data.Time (UTCTime(..), addUTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Format.Internal (buildTime)
import Data.Typeable (Typeable)
import Distribution.Text (display)
import Network.Mail.Mime
import Network.URI (uriAuthority, uriPath, uriRegName)
import Text.CSV (CSV, Record)
import Text.PrettyPrint hiding ((<>))
import Text.XHtml hiding (base, text, (</>))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector as Vec

-- A feature to manage notifications to users when package metadata, etc is updated.

{-
Some missing features:
 -- better formatting with mail templates
-}

data UserNotifyFeature = UserNotifyFeature {
    userNotifyFeatureInterface :: HackageFeature,

    queryGetUserNotifyPref  :: forall m. MonadIO m => UserId -> m (Maybe NotifyPref),
    updateSetUserNotifyPref :: forall m. MonadIO m => UserId -> NotifyPref -> m ()
}

instance IsHackageFeature UserNotifyFeature where
  getFeatureInterface = userNotifyFeatureInterface

-------------------------
-- Types of stored data
--
data NotifyPref_v0 = NotifyPref_v0
                  {
                    v0notifyOptOut :: Bool,
                    v0notifyRevisionRange :: NotifyRevisionRange,
                    v0notifyUpload :: Bool,
                    v0notifyMaintainerGroup :: Bool,
                    v0notifyDocBuilderReport :: Bool,
                    v0notifyPendingTags :: Bool
                  }
                  deriving (Eq, Read, Show, Typeable)
data NotifyPref = NotifyPref
                  {
                    notifyOptOut :: Bool,
                    notifyRevisionRange :: NotifyRevisionRange,
                    notifyUpload :: Bool,
                    notifyMaintainerGroup :: Bool,
                    notifyDocBuilderReport :: Bool,
                    notifyPendingTags :: Bool,
                    notifyDependencyForMaintained :: Bool,
                    notifyDependencyTriggerBounds :: NotifyTriggerBounds
                  }
                  deriving (Eq, Read, Show, Typeable)

defaultNotifyPrefs :: NotifyPref
defaultNotifyPrefs = NotifyPref {
                       notifyOptOut = True, -- TODO when we're comfortable with this we can change to False.
                       notifyRevisionRange = NotifyAllVersions,
                       notifyUpload = True,
                       notifyMaintainerGroup = True,
                       notifyDocBuilderReport = True,
                       notifyPendingTags = True,
                       notifyDependencyForMaintained = True,
                       notifyDependencyTriggerBounds = NewIncompatibility
                     }

data NotifyRevisionRange = NotifyAllVersions | NotifyNewestVersion | NoNotifyRevisions deriving (Bounded, Enum, Eq, Read, Show, Typeable)
instance MemSize NotifyRevisionRange where
  memSize _ = 1

instance Pretty NotifyRevisionRange where
  pretty NoNotifyRevisions = text "No"
  pretty NotifyAllVersions = text "All Versions"
  pretty NotifyNewestVersion = text "Newest Version"

instance Hashable NotifyRevisionRange where
  hash = fromEnum
  hashWithSalt s x = s `hashWithSalt` hash x

data NotifyTriggerBounds
  = Always
  | BoundsOutOfRange
  | NewIncompatibility
  deriving (Bounded, Enum, Eq, Read, Show, Typeable)

instance MemSize NotifyTriggerBounds where
  memSize _ = 1

instance Hashable NotifyTriggerBounds where
  hash = fromEnum
  hashWithSalt s x = s `hashWithSalt` hash x

instance MemSize NotifyPref_v0 where memSize _ = memSize ((True,True,True),(True,True, True))
instance MemSize NotifyPref    where memSize NotifyPref{..} = memSize8 notifyOptOut notifyRevisionRange notifyUpload notifyMaintainerGroup
                                                                       notifyDocBuilderReport notifyPendingTags notifyDependencyForMaintained
                                                                       notifyDependencyTriggerBounds

data NotifyData = NotifyData {unNotifyData :: (Map.Map UserId NotifyPref, UTCTime)} deriving (Eq, Show, Typeable)

instance MemSize NotifyData where memSize (NotifyData x) = memSize x

emptyNotifyData :: IO NotifyData
emptyNotifyData = getCurrentTime >>= \x-> return (NotifyData (Map.empty, x))

$(deriveSafeCopy 0 'base ''NotifyTriggerBounds)
$(deriveSafeCopy 0 'base ''NotifyRevisionRange)
$(deriveSafeCopy 0 'base ''NotifyPref_v0)

instance Migrate NotifyPref where
  type MigrateFrom NotifyPref = NotifyPref_v0
  migrate (NotifyPref_v0 f0 f1 f2 f3 f4 f5) =
    NotifyPref f0 f1 f2 f3 f4 f5
      False -- Users that already have opted in to notifications
            -- did so at at a time when it did not include
            -- reverse dependency emails.
            -- So let's assume they don't want these.
            -- Note that this differs from defaultNotifyPrefs.
      NewIncompatibility

$(deriveSafeCopy 1 'extension ''NotifyPref)
$(deriveSafeCopy 0 'base ''NotifyData)
$(deriveJSON defaultOptions ''NotifyRevisionRange)
$(deriveJSON defaultOptions ''NotifyTriggerBounds)

------------------------------
-- UI
--

-- | `Bool`'s 'FromJSON' instance can't parse strings:
--
-- >>> import qualified Data.Aeson as Aeson
-- >>> import qualified Data.ByteString.Lazy.Char8 as BS
-- >>> Aeson.decode (BS.pack "\"true\"") :: Maybe Bool
-- Nothing
--
-- However, form2json will pass JSON bool values as strings to the decoder.
-- So we define a newtype wrapping it up.
newtype OK = OK {unOK :: Bool} deriving (Eq, Show, Enum)

instance Pretty OK where
  pretty (OK True) = text "Yes"
  pretty (OK False) = text "No"

instance Aeson.ToJSON OK where
  toJSON = Aeson.toJSON . unOK

instance Aeson.FromJSON OK where
  parseJSON (Aeson.Bool b) = pure (OK b)
  parseJSON (Aeson.String "true") = pure (OK True)
  parseJSON (Aeson.String "false") = pure (OK False)
  parseJSON s@(Aeson.String _) = Aeson.prependFailure "parsing OK failed, " (Aeson.unexpected s)
  parseJSON invalid = Aeson.prependFailure "parsing OK failed, " (Aeson.typeMismatch "Bool or String" invalid)

instance Hashable OK where
  hashWithSalt s x = s `hashWithSalt` fromEnum x

data NotifyPrefUI
  = NotifyPrefUI
    { ui_notifyEnabled          :: OK
    , ui_notifyRevisionRange    :: NotifyRevisionRange
    , ui_notifyUpload           :: OK
    , ui_notifyMaintainerGroup  :: OK
    , ui_notifyDocBuilderReport :: OK
    , ui_notifyPendingTags      :: OK
    , ui_notifyDependencyForMaintained :: OK
    , ui_notifyDependencyTriggerBounds :: NotifyTriggerBounds
    }
  deriving (Eq, Show, Typeable)

$(deriveJSON (compatAesonOptionsDropPrefix "ui_") ''NotifyPrefUI)

instance Hashable NotifyPrefUI where
  hashWithSalt s NotifyPrefUI{..} = s
    `hashWithSalt` hash ui_notifyEnabled
    `hashWithSalt` hash ui_notifyRevisionRange
    `hashWithSalt` hash ui_notifyUpload
    `hashWithSalt` hash ui_notifyMaintainerGroup
    `hashWithSalt` hash ui_notifyDocBuilderReport
    `hashWithSalt` hash ui_notifyPendingTags

notifyPrefToUI :: NotifyPref -> NotifyPrefUI
notifyPrefToUI NotifyPref{..} = NotifyPrefUI
  { ui_notifyEnabled          = OK (not notifyOptOut)
  , ui_notifyRevisionRange    = notifyRevisionRange
  , ui_notifyUpload           = OK notifyUpload
  , ui_notifyMaintainerGroup  = OK notifyMaintainerGroup
  , ui_notifyDocBuilderReport = OK notifyDocBuilderReport
  , ui_notifyPendingTags      = OK notifyPendingTags
  , ui_notifyDependencyForMaintained = OK notifyDependencyForMaintained
  , ui_notifyDependencyTriggerBounds = notifyDependencyTriggerBounds
  }

notifyPrefFromUI :: NotifyPrefUI -> NotifyPref
notifyPrefFromUI NotifyPrefUI{..}
  = NotifyPref
  { notifyOptOut           = not (unOK ui_notifyEnabled)
  , notifyRevisionRange    = ui_notifyRevisionRange
  , notifyUpload           = unOK ui_notifyUpload
  , notifyMaintainerGroup  = unOK ui_notifyMaintainerGroup
  , notifyDocBuilderReport = unOK ui_notifyDocBuilderReport
  , notifyPendingTags      = unOK ui_notifyPendingTags
  , notifyDependencyForMaintained = unOK ui_notifyDependencyForMaintained
  , notifyDependencyTriggerBounds = ui_notifyDependencyTriggerBounds
  }

class ToRadioButtons a where
  toRadioButtons :: String -> a -> Html

renderRadioButtons :: (Eq a, Aeson.ToJSON a, Pretty a) => [a] -> String -> a -> Html
renderRadioButtons choices nm def = foldr1 (+++) $ map renderRadioButton choices
  where
    renderRadioButton choice = toHtml
      [ input ! (if (def == choice) then (checked :) else id)
          [thetype "radio", identifier htmlId, name nm, value choiceName]
      , label ! [thefor htmlId] << display choice
      ]
      where
        jsonName = Aeson.encode choice
        -- try to strip quotes
        choiceName = BS.unpack $ if BS.head jsonName == '"' && BS.last jsonName == '"'
                        then BS.init (BS.tail jsonName)
                        else jsonName
        htmlId = nm ++ "." ++ choiceName

instance ToRadioButtons NotifyRevisionRange where
  toRadioButtons = renderRadioButtons [NoNotifyRevisions, NotifyAllVersions, NotifyNewestVersion]

instance ToRadioButtons OK where
  toRadioButtons = renderRadioButtons [OK True, OK False]

------------------------------
-- State queries and updates
--

getNotifyData :: Query NotifyData NotifyData
getNotifyData = ask

replaceNotifyData :: NotifyData -> Update NotifyData ()
replaceNotifyData = put

getNotifyTime :: Query NotifyData UTCTime
getNotifyTime = fmap (snd . unNotifyData) ask

setNotifyTime :: UTCTime -> Update NotifyData ()
setNotifyTime t = do
    NotifyData (m,_) <- get
    put $! NotifyData (m,t)

lookupNotifyPref :: UserId -> Query NotifyData (Maybe NotifyPref)
lookupNotifyPref uid = do
    NotifyData (m,_) <- ask
    return $! Map.lookup uid m

addNotifyPref :: UserId -> NotifyPref -> Update NotifyData ()
addNotifyPref uid info = do
    NotifyData (m,t) <- get
    put $! NotifyData (Map.insert uid info m,t)

makeAcidic ''NotifyData [
    --queries
    'getNotifyData,
    'lookupNotifyPref,
    'getNotifyTime,
    --updates
    'replaceNotifyData,
    'addNotifyPref,
    'setNotifyTime
  ]


----------------------------
-- Data backup and restore
--

userNotifyBackup :: RestoreBackup NotifyData
userNotifyBackup = go []
  where
   go :: [(UserId, NotifyPref)] -> RestoreBackup NotifyData
   go st =
     RestoreBackup {
       restoreEntry = \entry -> case entry of
         BackupByteString ["notifydata.csv"] bs -> do
           csv <- importCSV "notifydata.csv" bs
           prefs <- importNotifyPref csv
           return (go (prefs ++ st))

         _ -> return (go st)

     , restoreFinalize =
        return (NotifyData (Map.fromList st, fromJust (buildTime defaultTimeLocale []))) -- defaults to unixstart time
     }

importNotifyPref :: CSV -> Restore [(UserId, NotifyPref)]
importNotifyPref = sequence . map fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (UserId, NotifyPref)
    fromRecord [uid,o,rr,ul,g,db,t,dep1,dep2] = do
        puid <- parseText "user id" uid
        po <- parseRead "notify opt out" o
        prr <- parseRead "notify revsion" rr
        pul <- parseRead "notify upload" ul
        pg <- parseRead "notify group mod" g
        pd <- parseRead "notify docbuilder" db
        pt <- parseRead "notify pending tags" t
        pdep1 <- parseRead "notify dependency for maintained" dep1
        pdep2 <- parseRead "notify dependency trigger bounds" dep2
        return (puid, NotifyPref po prr pul pg pd pt pdep1 pdep2)
    fromRecord x = fail $ "Error processing notify record: " ++ show x

notifyDataToCSV :: BackupType -> NotifyData -> CSV
notifyDataToCSV _backuptype (NotifyData (tbl,_))
    = ["0.1"]
    : [ "uid","freq","revisionrange","upload","group","pending_tags","dep_for_maintained","dep_trigger_bounds"]
    : flip map (Map.toList tbl) (\(uid,np) ->
        [ display uid
        , show (notifyOptOut np)
        , show (notifyRevisionRange np)
        , show (notifyUpload np)
        , show (notifyMaintainerGroup np)
        , show (notifyDocBuilderReport np)
        , show (notifyPendingTags np)
        , show (notifyDependencyForMaintained np)
        , show (notifyDependencyTriggerBounds np)
        ]
      )

----------------------------
-- State Component
--

notifyStateComponent :: FilePath -> IO (StateComponent AcidState NotifyData)
notifyStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserNotify") =<< emptyNotifyData
  return StateComponent {
      stateDesc    = "State to keep track of revision notifications"
    , stateHandle  = st
    , getState     = query st GetNotifyData
    , putState     = update st . ReplaceNotifyData
    , backupState  = \backuptype tbl ->
        [csvToBackup ["notifydata.csv"] (notifyDataToCSV backuptype tbl)]
    , restoreState = userNotifyBackup
    , resetState   = notifyStateComponent
    }

----------------------------
-- Core Feature
--

initUserNotifyFeature :: ServerEnv
                      -> IO (UserFeature
                          -> CoreFeature
                          -> UploadFeature
                          -> AdminLogFeature
                          -> UserDetailsFeature
                          -> ReportsFeature
                          -> TagsFeature
                          -> ReverseFeature
                          -> IO UserNotifyFeature)
initUserNotifyFeature env@ServerEnv{ serverStateDir, serverTemplatesDir,
                                     serverTemplatesMode } = do
    -- Canonical state
    notifyState <- notifyStateComponent serverStateDir

    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "UserNotify"]
                   [ "user-notify-form.html" ]

    return $ \users core uploadfeature adminlog userdetails reports tags revers -> do
      let feature = userNotifyFeature env
                      users core uploadfeature adminlog userdetails reports tags
                      revers notifyState templates
      return feature

data InRange = InRange | OutOfRange

-- | Get the users to notify when a new package has been released.
--   The new package (PackageId) must already be in the indexes.
--   The keys in the returned map are the user to notify, and the values are
--   the packages the user maintains that depend on the new package (i.e. the
--   reverse dependencies of the new package).
getUserNotificationsOnRelease
  :: forall m. Monad m
  => (PackageName -> m UserIdSet)
  -> PackageIndex.PackageIndex PkgInfo
  -> ReverseIndex
  -> (UserId -> m (Maybe NotifyPref))
  -> PackageId
  -> m (Map.Map UserId [PackageId])
getUserNotificationsOnRelease _ index _ _ pkgId
  | let versionsForNewRelease = packageVersion <$> PackageIndex.lookupPackageName index (pkgName pkgId)
  , pkgVersion pkgId /= maximum versionsForNewRelease
  -- If e.g. a minor bugfix release is made for an old release series, never notify maintainers.
  -- Only start checking if the new version is the highest.
  = pure mempty
getUserNotificationsOnRelease userSetIdForPackage index (ReverseIndex revs nodemap dependencies) queryGetUserNotifyPref pkgId =
  case lookup (pkgName pkgId) nodemap :: Maybe NodeId of
    Nothing -> pure mempty
    Just foundPackage -> do
      let
        vertices :: Set.Set Vertex
        vertices = suc revs foundPackage
        revDepNames :: [PackageName]
        revDepNames = mapMaybe (`lookupR` nodemap) (Set.toList vertices)
      toNotify <- traverse maintainersToNotify revDepNames
      pure $
        Map.fromListWith (++)
          [ (maintainerId, [packageId latestRevDep])
          | (ids, latestRevDep) <- toNotify
          , maintainerId <- ids
          ]
  where
    -- | Goes through the maintainers of the reverse dep identified by the PackageName passed in,
    --   finds the ones to notify.
    --   Returns the userIds and when they wanted notifications (NotifyTriggerBounds).
    --   The PkgInfo is the latest version of the reverse dependency passed in as PackageName.
    maintainersToNotify :: PackageName -> m ([UserId], PkgInfo)
    maintainersToNotify revDepName = do
      userIdSet <- userSetIdForPackage revDepName
      let ids = UserIdSet.toList userIdSet
      mPrefs <- traverse queryGetUserNotifyPref ids
      let
        idsAndTriggers :: [UserId]
        idsAndTriggers = do
          (userId, Just NotifyPref{..}) <- zip ids mPrefs
          guard $ not notifyOptOut
          guard notifyDependencyForMaintained

          Just depListWithCollisions <- [mDepList]
          -- Remove collisions on the same PackageName, amassed e.g. across
          -- multiple conditional branches. The branches could be from either
          -- side of an 'if' block conditioned on a flag. If either of them
          -- permits the newly released version, avoid sending the notification.
          let depList = unionSamePackageName depListWithCollisions

          case notifyDependencyTriggerBounds of
            NewIncompatibility -> do
              let allNewUploadPkgInfos = PackageIndex.lookupPackageName index (pkgName pkgId)
                  sortedByVersionDesc = sortOn (Down . packageVersion) allNewUploadPkgInfos
                  mSecondHighest =
                    case sortedByVersionDesc of
                      _:b:_ -> Just b
                      _     -> Nothing
              case mSecondHighest of
                Just secondHighest ->
                  guard $ any (\dep -> isDependencyMatchingAnd InRange (packageVersion secondHighest) dep
                                    && isDependencyMatchingAnd OutOfRange newestVersion dep
                              ) depList
                Nothing ->
                  -- If there is no second highest version, we just need to check whether the
                  -- newest version is out of range. Otherwise you'd get a notification for
                  -- a dependency which is within bounds.
                  guard $ any (isDependencyMatchingAnd OutOfRange newestVersion) depList
            BoundsOutOfRange -> guard $ any (isDependencyMatchingAnd OutOfRange newestVersion) depList
            Always           -> guard $ any (\(Dependency depName _ _) -> depName == pkgName pkgId) depList
          [userId]
      pure (idsAndTriggers, latestRevDep)
      where
        latestRevDep = maximumBy (comparing packageVersion) (PackageIndex.lookupPackageName index revDepName)
        mDepList :: Maybe [Dependency]
        mDepList = Map.lookup (packageId latestRevDep) dependencies
        isDependencyMatchingAnd :: InRange -> Version -> Dependency -> Bool
        isDependencyMatchingAnd InRange depVersion (Dependency depName depRange _)
          | depName /= pkgName pkgId = False
          | not (depVersion `withinRange` depRange) = False
          | otherwise = True
        isDependencyMatchingAnd OutOfRange depVersion (Dependency depName depRange _)
          | depName /= pkgName pkgId = False
          | depVersion `withinRange` depRange = False
          | otherwise = True
        newestVersion = pkgVersion pkgId

-- | Boolean OR on ranges across dependencies on the same PackageName
unionSamePackageName :: [Dependency] -> [Dependency]
unionSamePackageName collisions =
  let
    maps = [Map.singleton depName dep | dep@(Dependency depName _ _) <- collisions]
    disjunct :: Dependency -> Dependency -> Dependency
    disjunct
      (Dependency fName fRange fLibraries)
      (Dependency _     gRange gLibraries) =
        mkDependency
          fName
          (unionVersionRanges fRange gRange)
          (fLibraries <> gLibraries)
    disjunctions = Map.unionsWith disjunct maps
  in
    Map.elems disjunctions

pkgInfoToPkgId :: PkgInfo -> PackageIdentifier
pkgInfoToPkgId pkgInfo =
  PackageIdentifier (packageName pkgInfo) (packageVersion pkgInfo)

userNotifyFeature :: ServerEnv
                  -> UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> AdminLogFeature
                  -> UserDetailsFeature
                  -> ReportsFeature
                  -> TagsFeature
                  -> ReverseFeature
                  -> StateComponent AcidState NotifyData
                  -> Templates
                  -> UserNotifyFeature
userNotifyFeature serverEnv@ServerEnv{serverCron}
                  UserFeature{..}
                  CoreFeature{..}
                  UploadFeature{..}
                  AdminLogFeature{..}
                  userDetailsFeature@UserDetailsFeature{..}
                  ReportsFeature{..}
                  TagsFeature{..}
                  ReverseFeature{queryReverseIndex}
                  notifyState templates
  = UserNotifyFeature {..}

  where
    userNotifyFeatureInterface = (emptyHackageFeature "user-notify") {
        featureDesc      = "Notifications to users on metadata updates."
      , featureResources = [userNotifyResource] -- TODO we can add json features here for updating prefs
      , featureState     = [abstractAcidStateComponent notifyState]
      , featureCaches    = []
      , featureReloadFiles = reloadTemplates templates
      , featurePostInit  = setupNotifyCronJob
      }

    -- Resources
    --

    userNotifyResource =
      (resourceAt "/user/:username/notify.:format") {
        resourceDesc   = [ (GET,    "get the notify preference of a user account")
                         , (PUT,    "set the notify preference of a user account")
                         ]
      , resourceGet    = [ ("json", handlerGetUserNotify)
                         , ("html", handlerGetUserNotifyHtml)
                         ]
      , resourcePut    = [ ("json", handlerPutUserNotify) ]
      }

    -- Queries and updates
    --

    queryGetUserNotifyPref  ::  MonadIO m => UserId -> m (Maybe NotifyPref)
    queryGetUserNotifyPref uid = queryState notifyState (LookupNotifyPref uid)

    updateSetUserNotifyPref ::  MonadIO m => UserId -> NotifyPref -> m ()
    updateSetUserNotifyPref uid np = updateState notifyState (AddNotifyPref uid np)

    -- Request handlers
    --
    handlerGetUserNotify dpath = do
      uid <- lookupUserName =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      nprefui <- notifyPrefToUI . fromMaybe defaultNotifyPrefs <$> queryGetUserNotifyPref uid
      return $ toResponse (Aeson.toJSON nprefui)

    handlerGetUserNotifyHtml dpath = do
      (uid, uinfo) <- lookupUserNameFull =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      NotifyPrefUI{..} <- notifyPrefToUI . fromMaybe defaultNotifyPrefs <$> queryGetUserNotifyPref uid
      showConfirmationOfSave <- not . Prelude.null <$> queryString (lookBSs "showConfirmationOfSave")
      template <- getTemplate templates "user-notify-form.html"
      cacheControlWithoutETag [NoCache]
      let
        addNotifyDependencyForMaintainedChecked =
          case ui_notifyDependencyForMaintained of
            OK True  -> (("notifyDependencyForMaintainedTrueChecked" $= ("checked=checked" :: String)) :)
            OK False -> (("notifyDependencyForMaintainedFalseChecked" $= ("checked=checked" :: String)) :)
        addNotifyDependencyTriggerBoundsChecked =
          case ui_notifyDependencyTriggerBounds of
            Always           -> (("notifyDependencyTriggerBoundsAlwaysChecked" $= ("checked=checked" :: String)) :)
            BoundsOutOfRange -> (("notifyDependencyTriggerBoundsBoundsOutOfRangeChecked" $= ("checked=checked" :: String)) :)
            NewIncompatibility ->
              (("newIncompatibilityChecked" $= ("checked=checked" :: String)) :)
      ok . toResponse . template . addNotifyDependencyForMaintainedChecked . addNotifyDependencyTriggerBoundsChecked $
        [ "username"                $= display (userName uinfo)
        , "showConfirmationOfSave"  $= showConfirmationOfSave
        , "notifyEnabled"           $= toRadioButtons "notifyEnabled=%s"          ui_notifyEnabled
        , "notifyRevisionRange"     $= toRadioButtons "notifyRevisionRange=%s"    ui_notifyRevisionRange
        , "notifyUpload"            $= toRadioButtons "notifyUpload=%s"           ui_notifyUpload
        , "notifyMaintainerGroup"   $= toRadioButtons "notifyMaintainerGroup=%s"  ui_notifyMaintainerGroup
        , "notifyDocBuilderReport"  $= toRadioButtons "notifyDocBuilderReport=%s" ui_notifyDocBuilderReport
        , "notifyPendingTags"       $= toRadioButtons "notifyPendingTags=%s"      ui_notifyPendingTags
        ]

    handlerPutUserNotify dpath = do
      uid <- lookupUserName =<< userNameInPath dpath
      guardAuthorised_ [IsUserId uid, InGroup adminGroup]
      nprefui <- expectAesonContent
      let pref = notifyPrefFromUI nprefui
      updateSetUserNotifyPref uid pref
      noContent $ toResponse ()

    -- Engine
    --
    setupNotifyCronJob =
      addCronJob serverCron CronJob {
        cronJobName      = "send notifications",
        cronJobFrequency = TestJobFrequency (60*60*2), -- 2hr (for testing you can decrease this)
        cronJobOneShot   = False,
        cronJobAction    = notifyCronAction
      }

    notifyCronAction = do
        (notifyPrefs, lastNotifyTime) <- unNotifyData <$> queryState notifyState GetNotifyData
        now <- getCurrentTime
        let trimLastTime = if diffUTCTime now lastNotifyTime > (60*60*6) -- cap at 6hr
                             then addUTCTime (negate $ (60*60*6)) now
                             else lastNotifyTime -- for testing you can increase this
        users <- queryGetUserDb

        revisionsAndUploads <- collectRevisionsAndUploads trimLastTime now
        revisionUploadNotifications <- concatMapM (genRevUploadList notifyPrefs trimLastTime now) revisionsAndUploads

        groupActions <- collectAdminActions trimLastTime now
        groupActionNotifications <- concatMapM (genGroupUploadList notifyPrefs) groupActions

        docReports <- collectDocReport trimLastTime now
        docReportNotifications <- concatMapM (genDocReportList notifyPrefs) docReports

        tagProposals <- collectTagProposals
        tagProposalNotifications <- concatMapM (genTagProposalList notifyPrefs) tagProposals

        idx <- queryGetPackageIndex
        revIdx <- liftIO queryReverseIndex
        dependencyUpdateNotifications <- concatMapM (genDependencyUpdateList idx revIdx . pkgInfoToPkgId) revisionsAndUploads

        emails <-
          getNotificationEmails serverEnv userDetailsFeature queryGetUserNotifyPref users $
            concat
              [ revisionUploadNotifications
              , groupActionNotifications
              , docReportNotifications
              , tagProposalNotifications
              , dependencyUpdateNotifications
              ]
        mapM_ sendNotifyEmailAndDelay emails

        updateState notifyState (SetNotifyTime now)

    collectRevisionsAndUploads earlier now = do
        pkgIndex <- queryGetPackageIndex
        let isRecent pkgInfo =
               let rt = pkgLatestUploadTime pkgInfo
               in rt > earlier && rt <= now
        return $ filter isRecent $ (PackageIndex.allPackages pkgIndex)

    collectAdminActions earlier now = do
        aLog <- adminLog <$> queryGetAdminLog
        let isRecent (t,_,_,_) = t > earlier && t <= now
        return $ filter isRecent $ aLog

    collectDocReport earlier now = do
        pkgs <- PackageIndex.allPackages <$> queryGetPackageIndex
        pkgRpts <- forM pkgs $ \pkg -> do
          rpts <- queryPackageReports (packageId pkg)
          pure $ (pkg,) $ do
            -- List monad, filter out recent docbuilds
            (_, rpt@BuildReport.BuildReport{..}) <- rpts
            t <- maybeToList time
            guard $ docsOutcome /= BuildReport.NotTried && t > earlier && t <= now
            pure rpt
        let isBuildOk BuildReport.BuildReport{..} = docsOutcome == BuildReport.Ok
        pure $ map (second (all isBuildOk)) $ filter (not . Prelude.null . snd) pkgRpts

    collectTagProposals = do
        logs <- readMemState tagProposalLog
        writeMemState tagProposalLog Map.empty
        pure $ Map.toList logs

    genRevUploadList notifyPrefs earlier now pkg = do
         pkgIndex <- queryGetPackageIndex
         let actor = pkgLatestUploadUser pkg
             isRevision = pkgNumRevisions pkg > 1
             pkgName = packageName . pkgInfoId $ pkg
             mbLatest = listToMaybe . take 1 . reverse $ PackageIndex.lookupPackageName pkgIndex pkgName
             isLatestVersion = maybe False (\x -> pkgInfoId pkg == pkgInfoId x) mbLatest
         maintainers <- queryUserGroup $ maintainersGroup (packageName . pkgInfoId $ pkg)
         pure . flip mapMaybe (toList maintainers) $ \uid ->
          fmap (uid,) $ do
            let NotifyPref{..} = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
            guard $ uid /= actor
            guard $ not notifyOptOut
            if isRevision
              then do
                guard $
                  notifyRevisionRange == NotifyAllVersions ||
                  (notifyRevisionRange == NotifyNewestVersion && isLatestVersion)
                Just
                  NotifyNewRevision
                    { notifyPackageId = pkgInfoId pkg
                    , notifyRevisions =
                        filter (\(t, _) -> earlier < t && t <= now)
                          . map snd
                          . Vec.toList
                          $ pkgMetadataRevisions pkg
                    }
              else do
                guard notifyUpload
                Just
                  NotifyNewVersion
                    { notifyPackageInfo = pkg
                    }

    genGroupUploadList notifyPrefs groupAction =
      let notifyAllMaintainers actor pkg notif = do
            maintainers <- queryUserGroup $ maintainersGroup (mkPackageName $ BS.unpack pkg)
            pure . flip mapMaybe (toList maintainers) $ \uid -> do
              let NotifyPref{..} = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
              guard $ uid /= actor
              guard $ not notifyOptOut
              Just (uid, notif)
      in case groupAction of
        (time, userActor, Admin_GroupAddUser userSubject (MaintainerGroup pkg), reason) ->
          notifyAllMaintainers userActor pkg $
            NotifyMaintainerUpdate
              { notifyMaintainerUpdateType = MaintainerAdded
              , notifyUserActor = userActor
              , notifyUserSubject = userSubject
              , notifyPackageName = mkPackageName $ BS.unpack pkg
              , notifyReason = TL.toStrict $ TL.decodeUtf8 reason
              , notifyUpdatedAt = time
              }
        (time, userActor, Admin_GroupDelUser userSubject (MaintainerGroup pkg), reason) ->
          notifyAllMaintainers userActor pkg $
            NotifyMaintainerUpdate
              { notifyMaintainerUpdateType = MaintainerRemoved
              , notifyUserActor = userActor
              , notifyUserSubject = userSubject
              , notifyPackageName = mkPackageName $ BS.unpack pkg
              , notifyReason = TL.toStrict $ TL.decodeUtf8 reason
              , notifyUpdatedAt = time
              }
        _ -> pure []

    genDocReportList notifyPrefs (pkg, success) = do
      maintainers <- queryUserGroup $ maintainersGroup (packageName $ pkgInfoId pkg)
      pure . flip mapMaybe (toList maintainers) $ \uid ->
        fmap (uid,) $ do
          let NotifyPref{..} = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
          guard $ not notifyOptOut
          guard notifyDocBuilderReport
          Just
            NotifyDocsBuild
              { notifyPackageId = pkgInfoId pkg
              , notifyBuildSuccess = success
              }

    genTagProposalList notifyPrefs (pkg, (addedTags, deletedTags)) = do
      maintainers <- queryUserGroup $ maintainersGroup pkg
      pure . flip mapMaybe (toList maintainers) $ \uid ->
        fmap (uid,) $ do
          let NotifyPref{..} = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
          guard $ not notifyOptOut
          guard notifyPendingTags
          Just
            NotifyUpdateTags
              { notifyPackageName = pkg
              , notifyAddedTags = addedTags
              , notifyDeletedTags = deletedTags
              }

    genDependencyUpdateList idx revIdx pkg = do
      let toNotif watchedPkgs =
            NotifyDependencyUpdate
              { notifyPackageId = pkg
              , notifyWatchedPackages = watchedPkgs
              }
      Map.toList . fmap toNotif
        <$> getUserNotificationsOnRelease (queryUserGroup . maintainersGroup) idx revIdx queryGetUserNotifyPref pkg

    sendNotifyEmailAndDelay :: Mail -> IO ()
    sendNotifyEmailAndDelay email = do
      -- TODO: if we need any configuration of sendmail stuff, has to go here
      renderSendMail email

      -- delay sending out emails, because ???
      threadDelay 250000

data Notification
  = NotifyNewVersion
      { notifyPackageInfo :: PkgInfo
      }
  | NotifyNewRevision
      { notifyPackageId :: PackageId
      , notifyRevisions :: [UploadInfo]
      }
  | NotifyMaintainerUpdate
      { notifyMaintainerUpdateType :: NotifyMaintainerUpdateType
      , notifyUserActor :: UserId
      , notifyUserSubject :: UserId
      , notifyPackageName :: PackageName
      , notifyReason :: Text
      , notifyUpdatedAt :: UTCTime
      }
  | NotifyDocsBuild
      { notifyPackageId :: PackageId
      , notifyBuildSuccess :: Bool
      }
  | NotifyUpdateTags
      { notifyPackageName :: PackageName
      , notifyAddedTags :: Set Tag
      , notifyDeletedTags :: Set Tag
      }
  | NotifyDependencyUpdate
      { notifyPackageId :: PackageId
        -- ^ Dependency that was updated
      , notifyWatchedPackages :: [PackageId]
        -- ^ Packages maintained by user that depend on updated dep
      }

data NotifyMaintainerUpdateType = MaintainerAdded | MaintainerRemoved

-- | Notifications in the same group are batched in the same email.
--
-- TODO: How often do multiple notifications come in at once? Maybe it's
-- fine to just send one email per notification.
data NotificationGroup
  = GeneralNotification
  | DependencyNotification PackageId
  deriving (Eq, Ord)

-- | Get all the emails to send for the given notifications.
getNotificationEmails
  :: ServerEnv
  -> UserDetailsFeature
  -> (UserId -> IO (Maybe NotifyPref))
  -> Users.Users
  -> [(UserId, Notification)]
  -> IO [Mail]
getNotificationEmails
  ServerEnv{serverBaseURI}
  UserDetailsFeature{queryUserDetails}
  queryGetUserNotifyPref
  allUsers
  notifications = do
    let userIds = Set.fromList $ map fst notifications
    userIdToDetails <- Map.mapMaybe id <$> fromSetM queryUserDetails userIds
    userIdToNotifyPref <- Map.mapMaybe id <$> fromSetM queryGetUserNotifyPref userIds

    pure $
      let emails =
            groupNotifications . flip mapMaybe notifications $ \(uid, notif) ->
              fmap (uid,) $ renderNotification userIdToNotifyPref uid notif
      in flip mapMaybe (Map.toList emails) $ \((uid, group), emailContent) ->
          case uid `Map.lookup` userIdToDetails of
            Nothing -> Nothing
            Just AccountDetails{..} -> Just $
              Mail
                { mailFrom =
                    Address
                      { addressName = Just "Hackage website"
                      , addressEmail = "noreply@" <> hostname
                      }
                , mailTo =
                    [ Address
                        { addressName = Just accountName
                        , addressEmail = accountContactEmail
                        }
                    ]
                , mailCc = []
                , mailBcc = []
                , mailHeaders =
                    [ ("Subject", "[Hackage] " <> getEmailSubject group)
                    ]
                , mailParts =
                    [ fromEmailContent $ emailContent <> updatePreferencesText uid
                    ]
                }
  where
    groupNotifications :: [(UserId, (EmailContent, NotificationGroup))] -> Map (UserId, NotificationGroup) EmailContent
    groupNotifications =
      Map.fromListWith (<>)
        . map (\(uid, (emailContent, group)) -> ((uid, group), emailContent))

    getEmailSubject = \case
      GeneralNotification -> "Maintainer Notifications"
      DependencyNotification pkg -> "Dependency Update: " <> T.pack (display pkg)

    hostname =
      case uriAuthority serverBaseURI of
        Just auth -> T.pack $ uriRegName auth
        Nothing -> error $ "Could not get hostname from serverBaseURI: " <> show serverBaseURI

    updatePreferencesText uid =
      EmailContentParagraph $
        "You can adjust your notification preferences at" <> EmailContentSoftBreak
        <> emailContentUrl
            serverBaseURI
              { uriPath =
                  concatMap ("/" <>)
                    [ "user"
                    , display $ Users.userIdToName allUsers uid
                    , "notify"
                    ]
              }

    {----- Render notifications -----}

    renderNotification :: Map UserId NotifyPref -> UserId -> Notification -> Maybe (EmailContent, NotificationGroup)
    renderNotification userIdToNotifyPref uid = \case
      NotifyNewVersion{..} ->
        generalNotification $
          renderNotifyNewVersion
            notifyPackageInfo
      NotifyNewRevision{..} ->
        generalNotification $
          renderNotifyNewRevision
            notifyPackageId
            notifyRevisions
      NotifyMaintainerUpdate{..} ->
        generalNotification $
          renderNotifyMaintainerUpdate
            notifyMaintainerUpdateType
            notifyUserActor
            notifyUserSubject
            notifyPackageName
            notifyReason
            notifyUpdatedAt
      NotifyDocsBuild{..} ->
        generalNotification $
          renderNotifyDocsBuild
            notifyPackageId
            notifyBuildSuccess
      NotifyUpdateTags{..} ->
        generalNotification $
          renderNotifyUpdateTags
            notifyPackageName
            notifyAddedTags
            notifyDeletedTags
      NotifyDependencyUpdate{..} ->
        case uid `Map.lookup` userIdToNotifyPref of
          Nothing -> Nothing
          Just notifyPref ->
            Just
              ( renderNotifyDependencyUpdate
                  notifyPref
                  notifyPackageId
                  notifyWatchedPackages
              , DependencyNotification notifyPackageId
              )
      where
        generalNotification emailContent = Just (emailContent, GeneralNotification)

    renderNotifyNewVersion pkg =
      EmailContentParagraph $
        "Package upload, " <> renderPkgLink (pkgInfoId pkg) <> ", by " <>
        renderUserTime (pkgLatestUploadUser pkg) (pkgLatestUploadTime pkg)

    renderNotifyNewRevision pkg revs =
      EmailContentParagraph ("Package metadata revision(s), " <> renderPkgLink pkg <> ":")
      <> EmailContentList (map (uncurry $ flip renderUserTime) $ sortOn (Down . fst) revs)

    renderNotifyMaintainerUpdate updateType userActor userSubject pkg reason time =
      EmailContentParagraph ("Group modified by " <> renderUserTime userActor time <> ":")
      <> EmailContentList
          [ case updateType of
              MaintainerAdded ->
                renderUser userSubject <> " added to maintainers for " <> renderPackageName pkg
              MaintainerRemoved ->
                renderUser userSubject <> " removed from maintainers for " <> renderPackageName pkg
          , "Reason: " <> EmailContentText reason
          ]

    renderNotifyDocsBuild pkg success =
      EmailContentParagraph $
        "Package doc build for " <> renderPkgLink pkg <> ":" <> EmailContentSoftBreak
        <> if success
            then "Build successful."
            else "Build failed."

    renderNotifyUpdateTags pkg addedTags deletedTags =
      EmailContentParagraph ("Pending tag proposal for " <> emailContentDisplay pkg <> ":")
      <> EmailContentList
          [ "Additions: " <> showTags addedTags
          , "Deletions: " <> showTags deletedTags
          ]
      where
        showTags = emailContentIntercalate ", " . map emailContentDisplay . Set.toList

    renderNotifyDependencyUpdate NotifyPref{..} dep revDeps =
      let depName = emailContentDisplay (packageName dep)
          depVersion = emailContentDisplay (packageVersion dep)
      in
        foldMap EmailContentParagraph
          [ "The dependency " <> renderPkgLink dep <> " has been uploaded or revised."
          , case notifyDependencyTriggerBounds of
              Always ->
                "You have requested to be notified for each upload or revision \
                \of a dependency."
              _ ->
                "You have requested to be notified when a dependency isn't \
                \accepted by any of your maintained packages."
          , case notifyDependencyTriggerBounds of
              Always ->
                "These are your packages that depend on " <> depName <> ":"
              BoundsOutOfRange ->
                "These are your packages that require " <> depName
                <> " but don't accept " <> depVersion <> ":"
              NewIncompatibility ->
                "The following packages require " <> depName
                <> " but don't accept " <> depVersion
                <> " (they do accept the second-highest version):"
          ]
        <> EmailContentList (map renderPkgLink revDeps)

    {----- Rendering helpers -----}

    renderPackageName = emailContentStr . unPackageName

    renderPkgLink pkg =
      EmailContentLink
        (T.pack $ display pkg)
        serverBaseURI
          { uriPath = "/package/" <> display (packageName pkg) <> "-" <> display (packageVersion pkg)
          }

    renderUser = emailContentDisplay . Users.userIdToName allUsers

    renderTime = emailContentStr . formatTime defaultTimeLocale "%c"

    renderUserTime u t = renderUser u <> " [" <> renderTime t <> "]"

{----- Utilities -----}

fromSetM :: Monad m => (k -> m v) -> Set k -> m (Map k v)
fromSetM f = traverse id . Map.fromSet f

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f
