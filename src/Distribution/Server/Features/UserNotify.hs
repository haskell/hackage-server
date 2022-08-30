{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Distribution.Server.Features.UserNotify (
    initUserNotifyFeature,
    UserNotifyFeature(..),
    NotifyPref(..),
  ) where

import Distribution.Package
import Distribution.Pretty

import Distribution.Server.Users.Types(UserId, UserInfo (..))
import Distribution.Server.Users.UserIdSet as UserIdSet
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Group

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.AdminLog
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails
import Distribution.Server.Features.Upload
import Distribution.Server.Features.BuildReports
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport

import qualified Data.Map as Map

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy (base, deriveSafeCopy)
import Distribution.Text (display)
import Text.CSV (CSV, Record)
import Text.XHtml hiding (base, text, (</>))
import Text.PrettyPrint
import Data.List(intercalate)
import Data.Hashable (Hashable(..))
import Data.Aeson.TH ( defaultOptions, deriveJSON )

import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format.Internal (buildTime)

import Data.Maybe(fromMaybe, mapMaybe, fromJust, listToMaybe, maybeToList)

import Network.Mail.Mime
import Network.URI(uriAuthority, uriRegName)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor

-- A feature to manage notifications to users when package metadata, etc is updated.

{-
Some missing features:
 -- notifications on pending proposed tags
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

data NotifyPref = NotifyPref
                  {
                    notifyOptOut :: Bool,
                    notifyRevisionRange :: NotifyRevisionRange,
                    notifyUpload :: Bool,
                    notifyMaintainerGroup :: Bool,
                    notifyDocBuilderReport :: Bool,
                    notifyPendingTags :: Bool
                  }
                  deriving (Eq, Read, Show, Typeable)

defaultNotifyPrefs :: NotifyPref
defaultNotifyPrefs = NotifyPref {
                       notifyOptOut = True, -- TODO when we're comfortable with this we can change to False.
                       notifyRevisionRange = NotifyAllVersions,
                       notifyUpload = True,
                       notifyMaintainerGroup = True,
                       notifyDocBuilderReport = True,
                       notifyPendingTags = True
                     }

data NotifyRevisionRange = NoNotifyRevisions | NotifyAllVersions | NotifyNewestVersion deriving (Eq, Enum, Read, Show, Typeable)

instance Pretty NotifyRevisionRange where
  pretty NoNotifyRevisions = text "No notify revisions"
  pretty NotifyAllVersions = text "Notify all versions"
  pretty NotifyNewestVersion = text "Notify newest version"

instance Hashable NotifyRevisionRange where
  hash = fromEnum
  hashWithSalt s x = s `hashWithSalt` hash x

instance MemSize NotifyPref where memSize _ = memSize ((True,True,True),(True,True, True))

data NotifyData = NotifyData {unNotifyData :: (Map.Map UserId NotifyPref, UTCTime)} deriving (Eq, Show, Typeable)

instance MemSize NotifyData where memSize (NotifyData x) = memSize x

emptyNotifyData :: IO NotifyData
emptyNotifyData = getCurrentTime >>= \x-> return (NotifyData (Map.empty, x))

$(deriveSafeCopy 0 'base ''NotifyRevisionRange)
$(deriveSafeCopy 0 'base ''NotifyPref)
$(deriveSafeCopy 0 'base ''NotifyData)
$(deriveJSON defaultOptions ''NotifyRevisionRange)

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
  }

notifyPrefFromUI :: NotifyPrefUI -> NotifyPref
notifyPrefFromUI NotifyPrefUI{..} = NotifyPref
  { notifyOptOut           = not (unOK ui_notifyEnabled)
  , notifyRevisionRange    = ui_notifyRevisionRange
  , notifyUpload           = unOK ui_notifyUpload
  , notifyMaintainerGroup  = unOK ui_notifyMaintainerGroup
  , notifyDocBuilderReport = unOK ui_notifyDocBuilderReport
  , notifyPendingTags      = unOK ui_notifyPendingTags
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
    fromRecord [uid,o,rr,ul,g,db,t] = do
        puid <- parseText "user id" uid
        po <- parseRead "notify opt out" o
        prr <- parseRead "notify revsion" rr
        pul <- parseRead "notify upload" ul
        pg <- parseRead "notify group mod" g
        pd <- parseRead "notify docbuilder" db
        pt <- parseRead "notify pending tags" t
        return (puid, NotifyPref po prr pul pg pd pt)
    fromRecord x = fail $ "Error processing notify record: " ++ show x

notifyDataToCSV :: BackupType -> NotifyData -> CSV
notifyDataToCSV _backuptype (NotifyData (tbl,_))
    = ["0.1"]
    : [ "uid","freq","revisionrange","upload","group"]
    : flip map (Map.toList tbl) (\(uid,np) ->
        [display uid, show (notifyOptOut np), show (notifyRevisionRange np), show (notifyUpload np), show (notifyMaintainerGroup np), show (notifyDocBuilderReport np), show (notifyPendingTags np)]
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
                          -> IO UserNotifyFeature)
initUserNotifyFeature env@ServerEnv{ serverStateDir, serverTemplatesDir,
                                     serverTemplatesMode } = do
    -- Canonical state
    notifyState <- notifyStateComponent serverStateDir

    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "UserNotify"]
                   [ "user-notify-form.html" ]

    return $ \users core uploadfeature adminlog userdetails reports -> do
      let feature = userNotifyFeature env
                      users core uploadfeature adminlog userdetails reports
                      notifyState templates
      return feature


userNotifyFeature :: ServerEnv
                  -> UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> AdminLogFeature
                  -> UserDetailsFeature
                  -> ReportsFeature
                  -> StateComponent AcidState NotifyData
                  -> Templates
                  -> UserNotifyFeature
userNotifyFeature ServerEnv{serverBaseURI, serverCron}
                  UserFeature{..}
                  CoreFeature{..}
                  UploadFeature{..}
                  AdminLogFeature{..}
                  UserDetailsFeature{..}
                  ReportsFeature{..}
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
      nprefui@NotifyPrefUI{..} <- notifyPrefToUI . fromMaybe defaultNotifyPrefs <$> queryGetUserNotifyPref uid
      showConfirmationOfSave <- not . Prelude.null <$> queryString (lookBSs "showConfirmationOfSave")
      template <- getTemplate templates "user-notify-form.html"
      cacheControl [Private] $ etagFromHash (nprefui, showConfirmationOfSave)
      ok . toResponse $
        template
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
      updateSetUserNotifyPref uid (notifyPrefFromUI nprefui)
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
        revisionUploadNotifications <- foldM (genRevUploadList notifyPrefs) Map.empty revisionsAndUploads
        let revisionUploadEmails = map (describeRevision users trimLastTime now) <$> revisionUploadNotifications

        groupActions <- collectAdminActions trimLastTime now
        groupActionNotifications <- foldM (genGroupUploadList notifyPrefs) Map.empty groupActions
        let groupActionEmails = mapMaybe (describeGroupAction users) <$> groupActionNotifications

        docReports <- collectDocReport trimLastTime now
        docReportNotifications <- foldM (genDocReportList notifyPrefs) Map.empty docReports
        let docReportEmails = map describeDocReport <$> docReportNotifications

        mapM_ sendNotifyEmail . Map.toList $ foldr1 (Map.unionWith (++)) [revisionUploadEmails, groupActionEmails, docReportEmails]
        updateState notifyState (SetNotifyTime now)

    formatTimeUser users t u =
        display (Users.userIdToName users u) ++ " [" ++
        (formatTime defaultTimeLocale "%c" t) ++ "]"

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

    genRevUploadList notifyPrefs mp pkg = do
         pkgIndex <- queryGetPackageIndex
         let actor = pkgLatestUploadUser pkg
             isRevision = pkgNumRevisions pkg > 1
             pkgName = packageName . pkgInfoId $ pkg
             mbLatest = listToMaybe . take 1 . reverse $ PackageIndex.lookupPackageName pkgIndex pkgName
             isLatestVersion = maybe False (\x -> pkgInfoId pkg == pkgInfoId x) mbLatest
             addNotification uid m =
                if not (notifyOptOut npref) &&
                  (isRevision &&
                     ( notifyRevisionRange npref == NotifyAllVersions ||
                     ((notifyRevisionRange npref == NotifyNewestVersion) && isLatestVersion))
                   ||
                   not isRevision && notifyUpload npref)
                then Map.insertWith (++) uid [pkg] m
                else m
                    where npref = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
         maintainers <- queryUserGroup $ maintainersGroup (packageName . pkgInfoId $ pkg)
         return $ foldr addNotification mp (toList (delete actor maintainers))

    genGroupUploadList notifyPrefs mp ga =
        let (actor,gdesc) = case ga of (_,uid,Admin_GroupAddUser _ gd,_) -> (uid, gd)
                                       (_,uid,Admin_GroupDelUser _ gd,_) -> (uid, gd)
            addNotification uid m = if not (notifyOptOut npref) && notifyMaintainerGroup npref
                                      then Map.insertWith (++) uid [ga] m
                                      else m
                where npref = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
        in case gdesc of
           (MaintainerGroup pkg) -> do
              maintainers <- queryUserGroup $ maintainersGroup (mkPackageName $ BS.unpack pkg)
              return $ foldr addNotification mp (toList (delete actor maintainers))
           _ -> return mp

    genDocReportList notifyPrefs mp pkg = do
        let addNotification uid m =
                if not (notifyOptOut npref) && notifyDocBuilderReport npref
                then Map.insertWith (++) uid [pkg] m
                else m
                    where npref = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
        maintainers <- queryUserGroup $ maintainersGroup (packageName . pkgInfoId . fst $ pkg)
        return $ foldr addNotification mp (toList maintainers)

    describeRevision users earlier now pkg =
          if pkgNumRevisions pkg <= 1
            then "Package upload, " ++ display (packageName pkg) ++ ", by " ++
                 formatTimeUser users  (pkgLatestUploadTime pkg) (pkgLatestUploadUser pkg)
            else "Package metadata revision(s), " ++ display (packageName pkg) ++ ":\n" ++
                  unlines (map (uncurry (formatTimeUser users) . snd) recentRevs)
        where
           revs = reverse $ Vec.toList (pkgMetadataRevisions pkg)
           recentRevs = filter ((\x -> x > earlier && x <= now) . fst . snd) revs

    describeGroupAction users (time, uid, act, descr) =
       case act of
            (Admin_GroupAddUser tn (MaintainerGroup pkg)) -> Just $
                    "Group modified by " ++ formatTimeUser users time uid ++ ":\n" ++
                    display (Users.userIdToName users tn) ++ " added to maintainers for " ++ BS.unpack pkg ++
                    "\n" ++ "reason: " ++ BS.unpack descr
            (Admin_GroupDelUser tn (MaintainerGroup pkg)) -> Just $
                    "Group modified by " ++ formatTimeUser users time uid ++ ":\n" ++
                    display (Users.userIdToName users tn) ++ " removed from maintainers for " ++ BS.unpack pkg ++
                    "\n" ++ "reason: " ++ BS.unpack descr
            _ -> Nothing

    describeDocReport (pkg, doc) =
      "Package doc build for " ++ display (packageName pkg) ++ ":\n" ++
        if doc
          then "Build successful."
          else "Build failed."

    sendNotifyEmail :: (UserId, [String]) -> IO ()
    sendNotifyEmail (uid, ebody) = do
        mudetails <- queryUserDetails uid
        print (uid, ebody, mudetails)
        case mudetails of
             Nothing -> return ()
             Just (AccountDetails{accountContactEmail=eml, accountName=aname})-> do
                 let mailFrom = Address (Just (T.pack "Hackage website"))
                                    (T.pack ("noreply@" ++ uriRegName ourHost))
                     mail     = (emptyMail mailFrom) {
                       mailTo      = [Address (Just aname) eml],
                       mailHeaders = [(BSS.pack "Subject",
                                       T.pack "[Hackage] Maintainer Notifications")],
                       mailParts   = [[Part (T.pack "text/plain; charset=utf-8")
                                             None DefaultDisposition [] (PartContent $ BS.pack $ intercalate ("\n\n") ebody)]]
                     }
                     Just ourHost = uriAuthority serverBaseURI

                 renderSendMail mail --TODO: if we need any configuration of
                                     -- sendmail stuff, has to go here
