{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserNotify (
    initUserNotifyFeature,
    UserNotifyFeature(..)
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

import Data.Time (UTCTime(..), getCurrentTime, diffUTCTime, addUTCTime, buildTime, defaultTimeLocale, formatTime)

import Data.Maybe(fromMaybe, fromJust) -- doh, TODO

import Network.Mail.Mime

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as Vec

-- A feature to manage notifications to users when package metadata, etc is updated.

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

--TODO notify on docbuilder report

type NotifyPref = (NotifyFreq, NotifyRevisionRange, NotifyUpload, NotifyMaintainerGroup)

defaultNotifyPrefs = (BatchNotify, NotifyAllVersions, YesNotifyUpload, YesNotifyMaintainerGroup)

data NotifyFreq = NoNotify | IndividualNotify | BatchNotify deriving (Eq, Read, Show, Typeable)

data NotifyRevisionRange = NoNotifyRevisions | NotifyAllVersions | NotifyNewestVersion deriving (Eq, Read, Show, Typeable)

data NotifyUpload = NoNotifyUpload | YesNotifyUpload deriving (Eq, Read, Show, Typeable)

data NotifyMaintainerGroup = NoNotifyMaintainerGroup | YesNotifyMaintainerGroup deriving (Eq, Read, Show, Typeable)

instance MemSize NotifyFreq where memSize _ = memSize (1::Int)
instance MemSize NotifyRevisionRange where memSize _ = memSize (1::Int)
instance MemSize NotifyUpload where memSize _ = memSize (1::Int)
instance MemSize NotifyMaintainerGroup where memSize _ = memSize (1::Int)

data NotifyData = NotifyData {unNotifyData :: (Map.Map UserId NotifyPref, UTCTime)} deriving (Eq, Show, Typeable)

instance MemSize NotifyData where memSize (NotifyData x) = memSize x

emptyNotifyData :: IO NotifyData
emptyNotifyData = getCurrentTime >>= \x-> return (NotifyData (Map.empty, x))

$(deriveSafeCopy 0 'base ''NotifyFreq)
$(deriveSafeCopy 0 'base ''NotifyRevisionRange)
$(deriveSafeCopy 0 'base ''NotifyUpload)
$(deriveSafeCopy 0 'base ''NotifyMaintainerGroup)
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
        return (NotifyData (Map.fromList st, fromJust (buildTime defaultTimeLocale []))) -- TODO gah
     }

importNotifyPref :: CSV -> Restore [(UserId, NotifyPref)]
importNotifyPref = sequence . map fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (UserId, NotifyPref)
    fromRecord [uid,freq,rr,ul,g] = do
        puid <- parseText "user id" uid
        pfreq <- parseRead "notify freq" freq
        prr <- parseRead "notify revsion" rr
        pul <- parseRead "notify upload" ul
        pg <- parseRead "notify group mod" g
        return (puid, (pfreq, prr, pul, pg))
    fromRecord x = fail $ "Error processing notify record: " ++ show x

notifyDataToCSV :: BackupType -> NotifyData -> CSV
notifyDataToCSV _backuptype (NotifyData (tbl,_))
    = ["0.1"]
    : [ "uid","freq","revisionrange","upload","group"]
    : flip map (Map.toList tbl) (\(uid,(f,rr,ul,g)) ->
        [display uid, show f, show rr, show ul, show g]
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


--type UserNotifications = (package,package,adminlog)

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
      , featureResources = []
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
        cronJobFrequency = TestJobFrequency (60*60*2), -- 2hr
        cronJobOneShot   = False,
        cronJobAction    = notifyCronAction
      }


    notifyCronAction = do
        (notifyPrefs, lastNotifyTime) <- unNotifyData <$> queryState notifyState GetNotifyData
        now <- getCurrentTime
        let trimLastTime = if diffUTCTime now lastNotifyTime > fromIntegral (60*60*6) -- 6hr
                             then addUTCTime (fromIntegral (60*60*4)) now -- 4hr
                             else lastNotifyTime

        users <- queryGetUserDb

        revisionsAndUploads <- collectRevisionsAndUploads trimLastTime now

        revisionUploadNotifications <- foldM (genRevUploadList notifyPrefs) Map.empty revisionsAndUploads

        let revisionUploadEmails = map (describeRevision users trimLastTime now) <$> revisionUploadNotifications

        groupActions <- collectAdminActions trimLastTime now

        groupActionNotifications <- foldM (genGroupUploadList notifyPrefs) Map.empty groupActions

        -- find all changes since then and now and match against all user prefs and email
        updateState notifyState (SetNotifyTime now)

    describeRevision users earlier now pkg =
          if pkgNumRevisions pkg <= 1
            then "Package upload, " ++ display (packageName pkg) ++ ", by " ++
                 display (Users.userIdToName users (pkgLatestUploadUser pkg)) ++ "[" ++
                 (formatTime defaultTimeLocale "%c" (pkgLatestUploadTime pkg)) ++ "]"
            else "Package metadata revision, " ++ display (packageName pkg) -- ++ map go revPairs -- TODO FILL ME IN
        where
           revs = reverse $ Vec.toList (pkgMetadataRevisions pkg)
           recentRevCount = length (filter ((>earlier) . fst . snd) revs)

    genRevUploadList notifyPrefs mp pkg = do
         let actor = pkgLatestUploadUser pkg
             isRevision = pkgNumRevisions pkg > 1
             isLatestVersion = True --todo
             addNotification uid m =
                if (isRevision &&
                     ( rr == NotifyAllVersions ||
                     ((rr == NotifyNewestVersion) && isLatestVersion))
                 ||
                   not isRevision && ul == YesNotifyUpload)
                then Map.insertWith (++) uid [pkg] m
                else m
                    where (_,rr,ul,_) = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
         maintainers <- queryUserGroup $ maintainersGroup (packageName . pkgInfoId $ pkg)
         return $ foldr addNotification mp (toList (delete actor maintainers))

    genGroupUploadList notifyPrefs mp ga =
        let (actor,gdesc) = case ga of (_,uid,Admin_GroupAddUser _ gd,_) -> (uid, gd)
                                       (_,uid,Admin_GroupDelUser _ gd,_) -> (uid, gd)
            addNotification uid m = if gn == YesNotifyMaintainerGroup
                                      then Map.insertWith (++) uid [ga] m
                                      else m
                where (_,_,_,gn) = fromMaybe defaultNotifyPrefs (Map.lookup uid notifyPrefs)
        in case gdesc of
           (MaintainerGroup pkg) -> do
              maintainers <- queryUserGroup $ maintainersGroup (PackageName $ BS.unpack pkg)
              return $ foldr addNotification mp (toList (delete actor maintainers))
           _ -> return mp



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

{-
    handlerPostSignupRequestNew :: DynamicPath -> ServerPartE Response
    handlerPostSignupRequestNew _ = do
        templateEmail        <- getTemplate templates "SignupConfirmation.email"
        templateConfirmation <- getTemplate templates "SignupEmailSent.html"

        (username, realname, useremail) <- lookUserNameEmail

        nonce     <- liftIO (newRandomNonce 10)
        timestamp <- liftIO getCurrentTime
        let signupInfo = SignupInfo {
              signupUserName     = username,
              signupRealName     = realname,
              signupContactEmail = useremail,
              nonceTimestamp     = timestamp
            }

        let mailFrom = Address (Just (T.pack "Hackage website"))
                               (T.pack ("noreply@" ++ uriRegName ourHost))
            mail     = (emptyMail mailFrom) {
              mailTo      = [Address (Just realname) useremail],
              mailHeaders = [(BS.pack "Subject",
                              T.pack "Hackage account confirmation")],
              mailParts   = [[Part (T.pack "text/plain; charset=utf-8")
                                    None Nothing [] mailBody]]
            }
            mailBody = renderTemplate $ templateEmail
              [ "realname"    $= realname
              , "confirmlink" $= serverBaseURI {
                                   uriPath = "/users/register-request/"
                                          ++ renderNonce nonce
                                 }
              , "serverhost"  $= serverBaseURI
              ]
            Just ourHost = uriAuthority serverBaseURI

        updateAddSignupResetInfo nonce signupInfo

        liftIO $ renderSendMail mail --TODO: if we need any configuration of
                                     -- sendmail stuff, has to go here

        resp 202 $ toResponse $
          templateConfirmation
            [ "useremail" $= useremail ]
      where
        lookUserNameEmail = do
          (username, realname, useremail) <-
            msum [ body $ (,,) <$> lookText' "username"
                               <*> lookText' "realname"
                               <*> lookText' "email"
                 , errBadRequest "Missing form fields" [] ]

          guardValidLookingUserName username
          guardValidLookingName     realname
          guardValidLookingEmail    useremail

          return (username, realname, useremail)

        guardValidLookingName str = either errBadUserName return $ do
          guard (T.length str <= 70) ?! "Sorry, we didn't expect names to be longer than 70 characters."
          guard (T.all isPrint str)  ?! "Unexpected character in name, please use only printable Unicode characters."

        guardValidLookingUserName str = either errBadRealName return $ do
          guard (T.length str <= 50)    ?! "Sorry, we didn't expect login names to be longer than 50 characters."
          guard (T.all isValidUserNameChar str) ?! "Sorry, login names have to be ASCII characters only or _, no spaces or other symbols."

        guardValidLookingEmail str = either errBadEmail return $ do
          guard (T.length str <= 100)     ?! "Sorry, we didn't expect email addresses to be longer than 100 characters."
          guard (T.all isPrint str)       ?! "Unexpected character in email address, please use only printable Unicode characters."
          guard hasAtSomewhere            ?! "Oops, that doesn't look like an email address."
          guard (T.all (not.isSpace) str) ?! "Oops, no spaces in email addresses please."
          guard (T.all (not.isAngle) str) ?! "Please use just the email address, not \"name\" <person@example.com> style."
          where
            isAngle c = c == '<' || c == '>'
            hasAtSomewhere =
              let (before, after) = T.span (/= '@') str
               in T.length before >= 1
               && T.length after  >  1

        errBadUserName err = errBadRequest "Problem with login name" [MText err]
        errBadRealName err = errBadRequest "Problem with name"[MText err]
        errBadEmail    err = errBadRequest "Problem with email address" [MText err]


    handlerGetSignupRequestOutstanding :: DynamicPath -> ServerPartE Response
    handlerGetSignupRequestOutstanding dpath = do
        nonce <- nonceInPath dpath
        SignupInfo {..} <- lookupSignupInfo nonce
        template <- getTemplate templates "SignupConfirm.html"
        resp 202 $ toResponse $
          template
            [ "realname"  $= signupRealName
            , "username"  $= signupUserName
            , "useremail" $= signupContactEmail
            , "posturl"   $= renderResource signupRequestResource
                                [renderNonce nonce]
            ]

    handlerPostSignupRequestOutstanding :: DynamicPath -> ServerPartE Response
    handlerPostSignupRequestOutstanding dpath = do
        nonce <- nonceInPath dpath
        SignupInfo {..} <- lookupSignupInfo nonce
        (passwd, passwdRepeat) <- lookPasswd
        when (passwd /= passwdRepeat) errPasswdMismatch
        updateDeleteSignupResetInfo nonce
        timenow <- liftIO getCurrentTime
        let username    = UserName (T.unpack signupUserName)
            userauth    = newUserAuth username (PasswdPlain passwd)
            acctDetails = AccountDetails {
              accountName         = signupRealName,
              accountContactEmail = signupContactEmail,
              accountKind         = Just AccountKindRealUser,
              accountAdminNotes   = T.pack $ "Account created by "
                                          ++ "self-registration at "
                                          ++ show timenow
            }
        uid <- updateAddUser username userauth
           >>= either errNameClash return
        updateUserDetails uid acctDetails
        liftIO $ addUserToGroup uploadersGroup uid
        seeOther (userPageUri userResource "" username) (toResponse ())
      where
        lookPasswd = body $ (,) <$> look "password"
                                <*> look "repeat-password"
        errPasswdMismatch =
          errBadRequest "Password mismatch"
            [MText $ "The two copies of the password did not match. "
                  ++ "Check and try again."]
        errNameClash Users.ErrUserNameClash =
          errBadRequest "Account login name already taken"
            [MText $ "Sorry! In the time between requesting the account and "
                  ++ "now, the login username was registered by someone else. "
                  ++ "You can make a new account request at "
            ,MLink "/users/signup-request" "/users/signup-request"]

    -- Password reset handlers

    handlerGetResetRequestNew :: DynamicPath -> ServerPartE Response
    handlerGetResetRequestNew _ = do
        template <- getTemplate templates "ResetRequest.html"
        ok $ toResponse $ template []

    handlerPostResetRequestNew :: DynamicPath -> ServerPartE Response
    handlerPostResetRequestNew _ = do
        templateEmail        <- getTemplate templates "ResetConfirmation.email"
        templateConfirmation <- getTemplate templates "ResetEmailSent.html"

        (supplied_username, supplied_useremail) <- lookUserNameEmail
        (uid, uinfo) <- lookupUserNameFull supplied_username
        mudetails    <- queryUserDetails uid

        guardEmailMatches mudetails supplied_useremail
        AccountDetails{..} <- guardSuitableAccountType uinfo mudetails

        nonce     <- liftIO (newRandomNonce 10)
        timestamp <- liftIO getCurrentTime
        let resetInfo = ResetInfo {
              resetUserId    = uid,
              nonceTimestamp = timestamp
            }
        let mailFrom = Address (Just (T.pack "Hackage website"))
                               (T.pack ("noreply@" ++ uriRegName ourHost))
            mail     = (emptyMail mailFrom) {
              mailTo      = [Address (Just accountName) accountContactEmail],
              mailHeaders = [(BS.pack "Subject",
                              T.pack "Hackage password reset confirmation")],
              mailParts   = [[Part (T.pack "text/plain; charset=utf-8")
                                    None Nothing [] mailBody]]
            }
            mailBody = renderTemplate $ templateEmail
              [ "realname"    $= accountName
              , "confirmlink" $= serverBaseURI {
                                   uriPath = "/users/password-reset/"
                                          ++ renderNonce nonce
                                 }
              , "serverhost"  $= serverBaseURI
              ]
            Just ourHost = uriAuthority serverBaseURI

        updateAddSignupResetInfo nonce resetInfo

        liftIO $ renderSendMail mail --TODO: if we need any configuration of
                                     -- sendmail stuff, has to go here

        resp 202 $ toResponse $
          templateConfirmation
            [ "useremail" $= accountContactEmail ]
      where
        lookUserNameEmail :: ServerPartE (UserName, Text)
        lookUserNameEmail =
          msum [ body $ (,) <$> (UserName <$> look "username")
                            <*> lookText' "email"
               , errBadRequest "Missing form fields" [] ]

        guardEmailMatches (Just AccountDetails {accountContactEmail}) useremail
          | T.toCaseFold accountContactEmail == T.toCaseFold useremail = return ()
        guardEmailMatches _ _ =
          errForbidden "Wrong account details"
            [MText "Sorry, that does not match any account details we have on file."]
-}