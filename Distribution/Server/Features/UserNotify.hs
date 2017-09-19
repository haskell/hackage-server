{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserNotify (
    initUserNotifyFeature,
    UserNotifyFeature(..),
    NotifyPref(..),
  ) where

import Distribution.Package

import Distribution.Server.Users.Types(UserId)
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

import qualified Data.Map as Map

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy (base, deriveSafeCopy)
import Distribution.Text (display)
import Text.CSV (CSV, Record)
import Data.List(intercalate)

import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime, addUTCTime, buildTime, defaultTimeLocale, formatTime)

import Data.Maybe(fromMaybe, mapMaybe, fromJust, listToMaybe)

import Network.Mail.Mime
import Network.URI(uriAuthority, uriRegName)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import qualified Data.Text as T
import qualified Data.Vector as Vec

-- A feature to manage notifications to users when package metadata, etc is updated.

{-
Some missing features:
 -- notifications on pending proposed tags
 -- notifications on docbuilder reports
 -- pref settings for notifications (new PR?)
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

data NotifyRevisionRange = NoNotifyRevisions | NotifyAllVersions | NotifyNewestVersion deriving (Eq, Read, Show, Typeable)

instance MemSize NotifyPref where memSize _ = memSize ((True,True,True),(True,True, True))

data NotifyData = NotifyData {unNotifyData :: (Map.Map UserId NotifyPref, UTCTime)} deriving (Eq, Show, Typeable)

instance MemSize NotifyData where memSize (NotifyData x) = memSize x

emptyNotifyData :: IO NotifyData
emptyNotifyData = getCurrentTime >>= \x-> return (NotifyData (Map.empty, x))

$(deriveSafeCopy 0 'base ''NotifyPref)
$(deriveSafeCopy 0 'base ''NotifyRevisionRange)
$(deriveSafeCopy 0 'base ''NotifyData)

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
                          -> IO UserNotifyFeature)
initUserNotifyFeature env@ServerEnv{ serverStateDir, serverTemplatesDir,
                                     serverTemplatesMode } = do
    -- Canonical state
    notifyState <- notifyStateComponent serverStateDir

    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "UserNotify"]
                   [ "SingleNotify.html" ]

    return $ \users core uploadfeature adminlog userdetails -> do
      let feature = userNotifyFeature env
                      users core uploadfeature adminlog userdetails
                      notifyState templates
      return feature


userNotifyFeature :: ServerEnv
                  -> UserFeature
                  -> CoreFeature
                  -> UploadFeature
                  -> AdminLogFeature
                  -> UserDetailsFeature
                  -> StateComponent AcidState NotifyData
                  -> Templates
                  -> UserNotifyFeature
userNotifyFeature ServerEnv{serverBaseURI, serverCron}
                  UserFeature{..}
                  CoreFeature{..}
                  UploadFeature{..}
                  AdminLogFeature{..}
                  UserDetailsFeature{..}
                  notifyState templates
  = UserNotifyFeature {..}

  where
    userNotifyFeatureInterface = (emptyHackageFeature "user-notify") {
        featureDesc      = "Notifications to users on metadata updates."
      , featureResources = [] -- TODO we can add json features here for updating prefs
      , featureState     = [abstractAcidStateComponent notifyState]
      , featureCaches    = []
      , featureReloadFiles = reloadTemplates templates
      , featurePostInit  = setupNotifyCronJob
      }

    -- Resources
    --


    -- Queries and updates
    --

    queryGetUserNotifyPref  ::  MonadIO m => UserId -> m (Maybe NotifyPref)
    queryGetUserNotifyPref uid = queryState notifyState (LookupNotifyPref uid)

    updateSetUserNotifyPref ::  MonadIO m => UserId -> NotifyPref -> m ()
    updateSetUserNotifyPref uid np = updateState notifyState (AddNotifyPref uid np)

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

        mapM_ sendNotifyEmail . Map.toList $ Map.unionWith (++) revisionUploadEmails groupActionEmails
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
                                             None Nothing [] (BS.pack $ intercalate ("\n\n") ebody)]]
                     }
                     Just ourHost = uriAuthority serverBaseURI

                 renderSendMail mail --TODO: if we need any configuration of
                                     -- sendmail stuff, has to go here
