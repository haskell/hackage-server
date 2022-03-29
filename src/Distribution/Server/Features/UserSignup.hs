{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup (
    initUserSignupFeature,
    UserSignupFeature(..),
    SignupResetInfo(..),

    accountSuitableForPasswordReset
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails

import Distribution.Server.Users.Group
import Distribution.Server.Users.Types
import Distribution.Server.Util.Nonce
import qualified Distribution.Server.Users.Users as Users

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS -- Only used for ASCII data
import Data.Char (isSpace, isPrint)

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)
import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Text (display)
import Data.Time (UTCTime(..), getCurrentTime, addDays)
import Text.CSV (CSV, Record)
import Network.Mail.Mime
import Network.URI (URI(..), URIAuth(..))


-- | A feature to allow open account signup, and password reset,
-- both with email confirmation.
--
data UserSignupFeature = UserSignupFeature {
    userSignupFeatureInterface :: HackageFeature,

    queryAllSignupResetInfo :: forall m. MonadIO m => m [SignupResetInfo]
}

instance IsHackageFeature UserSignupFeature where
  getFeatureInterface = userSignupFeatureInterface


-----------------
-- Signup flow:
--
-- GET  account request page
-- POST account request, including username and email
--      suitable fail if username taken
--      does not yet create or reserve the account
--      makes entry in signup table, with random nonce
--      send email to user with link to account confirm
-- GET  account confirm page (with nonce in url)
-- POST account confirm (with nonce in url)
--      finally create account.
--
--
-- Reset flow:
--
-- GET  password reset request page
-- POST username and email
--      makes entry in reset table, with random nonce
--      send email to user with link to reset
-- GET  password change page (with nonce in url)
-- POST password change (with nonce in url)
--      set new password
--

-------------------------
-- Types of stored data
--

data SignupResetInfo = SignupInfo {
                         signupUserName     :: !Text,
                         signupRealName     :: !Text,
                         signupContactEmail :: !Text,
                         nonceTimestamp     :: !UTCTime
                       }
                     | ResetInfo {
                         resetUserId        :: !UserId,
                         nonceTimestamp     :: !UTCTime
                     }
  deriving (Eq, Show, Typeable)

newtype SignupResetTable = SignupResetTable (Map Nonce SignupResetInfo)
  deriving (Eq, Show, Typeable, MemSize)

emptySignupResetTable :: SignupResetTable
emptySignupResetTable = SignupResetTable Map.empty

instance MemSize SignupResetInfo where
    memSize (SignupInfo a b c d) = memSize4 a b c d
    memSize (ResetInfo  a b)     = memSize2 a b

$(deriveSafeCopy 0 'base ''SignupResetInfo)
$(deriveSafeCopy 0 'base ''SignupResetTable)

------------------------------
-- State queries and updates
--

getSignupResetTable :: Query SignupResetTable SignupResetTable
getSignupResetTable = ask

replaceSignupResetTable :: SignupResetTable -> Update SignupResetTable ()
replaceSignupResetTable = put

lookupSignupResetInfo :: Nonce -> Query SignupResetTable (Maybe SignupResetInfo)
lookupSignupResetInfo nonce = do
    SignupResetTable tbl <- ask
    return $! Map.lookup nonce tbl

addSignupResetInfo :: Nonce -> SignupResetInfo -> Update SignupResetTable Bool
addSignupResetInfo nonce info = do
    SignupResetTable tbl <- get
    if not (Map.member nonce tbl)
      then do put $! SignupResetTable (Map.insert nonce info tbl)
              return True
      else return False

deleteSignupResetInfo :: Nonce -> Update SignupResetTable ()
deleteSignupResetInfo nonce = do
    SignupResetTable tbl <- get
    put $! SignupResetTable (Map.delete nonce tbl)

deleteAllExpired :: UTCTime -> Update SignupResetTable ()
deleteAllExpired expireTime =
    modify $ \(SignupResetTable tbl) ->
      SignupResetTable $
        Map.filter (\entry -> nonceTimestamp entry > expireTime) tbl

makeAcidic ''SignupResetTable [
    --queries
    'getSignupResetTable,
    'lookupSignupResetInfo,
    --updates
    'replaceSignupResetTable,
    'addSignupResetInfo,
    'deleteSignupResetInfo,
    'deleteAllExpired
  ]


---------------------
-- State components
--

signupResetStateComponent :: FilePath -> IO (StateComponent AcidState SignupResetTable)
signupResetStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserSignupReset") emptySignupResetTable
  return StateComponent {
      stateDesc    = "State to keep track of outstanding requests for user signup and password resets"
    , stateHandle  = st
    , getState     = query st GetSignupResetTable
    , putState     = update st . ReplaceSignupResetTable
    , backupState  = \backuptype tbl ->
        [csvToBackup ["signups.csv"] (signupInfoToCSV backuptype tbl)
        ,csvToBackup ["resets.csv"]  (resetInfoToCSV backuptype tbl)]
    , restoreState = signupResetBackup
    , resetState   = signupResetStateComponent
    }

----------------------------
-- Data backup and restore
--

signupResetBackup :: RestoreBackup SignupResetTable
signupResetBackup = go []
  where
   go :: [(Nonce, SignupResetInfo)] -> RestoreBackup SignupResetTable
   go st =
     RestoreBackup {
       restoreEntry = \entry -> case entry of
         BackupByteString ["signups.csv"] bs -> do
           csv <- importCSV "signups.csv" bs
           signups <- importSignupInfo csv
           return (go (signups ++ st))

         BackupByteString ["resets.csv"] bs -> do
           csv <- importCSV "resets.csv" bs
           resets <- importResetInfo csv
           return (go (resets ++ st))

         _ -> return (go st)

     , restoreFinalize =
        return (SignupResetTable (Map.fromList st))
     }

importSignupInfo :: CSV -> Restore [(Nonce, SignupResetInfo)]
importSignupInfo = sequence . map fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (Nonce, SignupResetInfo)
    fromRecord [nonceStr, usernameStr, realnameStr, emailStr, timestampStr] = do
        timestamp <- parseUTCTime "timestamp" timestampStr
        nonce <- parseNonceM nonceStr
        let signupinfo = SignupInfo {
              signupUserName     = T.pack usernameStr,
              signupRealName     = T.pack realnameStr,
              signupContactEmail = T.pack emailStr,
              nonceTimestamp     = timestamp
            }
        return (nonce, signupinfo)
    fromRecord x = fail $ "Error processing signup info record: " ++ show x

signupInfoToCSV :: BackupType -> SignupResetTable -> CSV
signupInfoToCSV backuptype (SignupResetTable tbl)
    = ["0.1"]
    : [ "token", "username", "realname", "email", "timestamp" ]
    : [ [ if backuptype == FullBackup
          then renderNonce nonce
          else ""
        , T.unpack signupUserName
        , T.unpack signupRealName
        , if backuptype == FullBackup
          then T.unpack signupContactEmail
          else "hidden-email@nowhere.org"
        , formatUTCTime nonceTimestamp
        ]
      | (nonce, SignupInfo{..}) <- Map.toList tbl ]

importResetInfo :: CSV -> Restore [(Nonce, SignupResetInfo)]
importResetInfo = sequence . map fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (Nonce, SignupResetInfo)
    fromRecord [nonceStr, useridStr, timestampStr] = do
        userid <- parseText "userid" useridStr
        timestamp <- parseUTCTime "timestamp" timestampStr
        nonce <- parseNonceM nonceStr
        let signupinfo = ResetInfo {
              resetUserId    = userid,
              nonceTimestamp = timestamp
            }
        return (nonce, signupinfo)
    fromRecord x = fail $ "Error processing signup info record: " ++ show x

resetInfoToCSV :: BackupType -> SignupResetTable -> CSV
resetInfoToCSV backuptype (SignupResetTable tbl)
    = ["0.1"]
    : [ "token", "userid", "timestamp" ]
    : [ [ if backuptype == FullBackup
          then renderNonce nonce
          else ""
        , display resetUserId
        , formatUTCTime nonceTimestamp
        ]
      | (nonce, ResetInfo{..}) <- Map.toList tbl ]


----------------------------------------
-- Feature definition & initialisation
--

initUserSignupFeature :: ServerEnv
                      -> IO (UserFeature
                          -> UserDetailsFeature
                          -> UploadFeature
                          -> IO UserSignupFeature)
initUserSignupFeature env@ServerEnv{ serverStateDir, serverTemplatesDir,
                                     serverTemplatesMode } = do
    -- Canonical state
    signupResetState <- signupResetStateComponent serverStateDir

    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "UserSignupReset"]
                   [ "SignupRequest.html", "SignupConfirmation.email"
                   , "SignupEmailSent.html", "SignupConfirm.html"
                   , "ResetRequest.html", "ResetConfirmation.email"
                   , "ResetEmailSent.html", "ResetConfirm.html" ]

    return $ \users userdetails upload -> do
      let feature = userSignupFeature env
                      users userdetails upload
                      signupResetState templates
      return feature


userSignupFeature :: ServerEnv
                  -> UserFeature
                  -> UserDetailsFeature
                  -> UploadFeature
                  -> StateComponent AcidState SignupResetTable
                  -> Templates
                  -> UserSignupFeature
userSignupFeature ServerEnv{serverBaseURI, serverCron}
                  UserFeature{..} UserDetailsFeature{..}
                  UploadFeature{uploadersGroup} signupResetState templates
  = UserSignupFeature {..}

  where
    userSignupFeatureInterface = (emptyHackageFeature "user-signup-reset") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [signupRequestsResource,
                            signupRequestResource,
                            resetRequestsResource,
                            resetRequestResource]
      , featureState     = [abstractAcidStateComponent signupResetState]
      , featureCaches    = []
      , featureReloadFiles = reloadTemplates templates
      , featurePostInit  = setupExpireCronJob
      }

    -- Resources
    --

    signupRequestsResource =
      (resourceAt "/users/register-request") {
        resourceDesc = [ (GET,  "Page to let you make a request for an account")
                       , (POST, "Create a new account signup request") ]
      , resourceGet  = [ ("", handlerGetSignupRequestNew) ]
      , resourcePost = [ ("", handlerPostSignupRequestNew) ]
      }

    signupRequestResource =
      (resourceAt "/users/register-request/:nonce") {
        resourceDesc = [ (GET,  "Page for confirming outstanding signup request")
                       , (POST, "Confirm signup request and create the new account") ]
      , resourceGet  = [ ("", handlerGetSignupRequestOutstanding) ]
      , resourcePost = [ ("", handlerPostSignupRequestOutstanding) ]
      }

    resetRequestsResource =
      (resourceAt "/users/password-reset") {
        resourceDesc = [ (GET,  "Page to let you make a request for a password reset")
                       , (POST, "Create a new password reset request") ]
      , resourceGet  = [ ("", handlerGetResetRequestNew) ]
      , resourcePost = [ ("", handlerPostResetRequestNew) ]
      }

    resetRequestResource =
      (resourceAt "/users/password-reset/:nonce") {
        resourceDesc = [ (GET,  "Page for confirming password reset request and entering new password")
                       , (POST, "Confirm password reset and set new password") ]
      , resourceGet  = [ ("", handlerGetResetRequestOutstanding) ]
      , resourcePost = [ ("", handlerPostResetRequestOutstanding) ]
      }


    -- Queries and updates
    --

    queryAllSignupResetInfo :: MonadIO m => m [SignupResetInfo]
    queryAllSignupResetInfo =
          queryState signupResetState GetSignupResetTable
      >>= \(SignupResetTable tbl) -> return (Map.elems tbl)

    querySignupInfo :: Nonce -> MonadIO m => m (Maybe SignupResetInfo)
    querySignupInfo nonce =
            queryState signupResetState (LookupSignupResetInfo nonce)
        >>= return . justSignupInfo
      where
        justSignupInfo (Just info@SignupInfo{}) = Just info
        justSignupInfo _                        = Nothing

    queryResetInfo :: Nonce -> MonadIO m => m (Maybe SignupResetInfo)
    queryResetInfo nonce =
            queryState signupResetState (LookupSignupResetInfo nonce)
        >>= return . justResetInfo
      where
        justResetInfo (Just info@ResetInfo{}) = Just info
        justResetInfo _                       = Nothing

    updateAddSignupResetInfo :: Nonce -> SignupResetInfo -> MonadIO m => m Bool
    updateAddSignupResetInfo nonce signupInfo =
        updateState signupResetState (AddSignupResetInfo nonce signupInfo)

    updateDeleteSignupResetInfo :: Nonce -> MonadIO m => m ()
    updateDeleteSignupResetInfo nonce =
        updateState signupResetState (DeleteSignupResetInfo nonce)

    -- Expiry
    --
    setupExpireCronJob =
      addCronJob serverCron CronJob {
        cronJobName      = "delete expired signup/reset requests",
        cronJobFrequency = DailyJobFrequency,
        cronJobOneShot   = False,
        cronJobAction    = do
          now <- getCurrentTime
          let expire = now { utctDay = addDays (-7) (utctDay now) }
          updateState signupResetState (DeleteAllExpired expire)
      }

    -- Request handlers
    --

    nonceInPath :: MonadPlus m => DynamicPath -> m Nonce
    nonceInPath dpath =
        maybe mzero return (lookup "nonce" dpath >>= parseNonceM)

    lookupSignupInfo :: Nonce -> ServerPartE SignupResetInfo
    lookupSignupInfo nonce = querySignupInfo nonce
                         >>= maybe (errNoNonce "account signup") return

    lookupResetInfo :: Nonce -> ServerPartE SignupResetInfo
    lookupResetInfo nonce = queryResetInfo nonce
                        >>= maybe (errNoNonce "password reset") return

    errNoNonce thing = errNotFound "Not found"
      [MText $ "The " ++ thing ++ " token does not exist. It could be that it "
            ++ "has been used already, or that it has expired."]

    handlerGetSignupRequestNew :: DynamicPath -> ServerPartE Response
    handlerGetSignupRequestNew _ = do
        template <- getTemplate templates "SignupRequest.html"
        ok $ toResponse $ template []

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
                                    None DefaultDisposition [] (PartContent mailBody)]]
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
        -- Temporarily disabled to prevent spam -- GB 2/22/2018
        -- liftIO $ addUserToGroup uploadersGroup uid
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
                                    None DefaultDisposition [] (PartContent mailBody)]]
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

    guardSuitableAccountType uinfo (Just udetails)
      | accountSuitableForPasswordReset uinfo udetails
                                 = return udetails
    guardSuitableAccountType _ _ =
      errForbidden "Cannot reset password for this account"
        [MText $ "Sorry, the self-service password reset system cannot be "
              ++ "used for this account at this time."]


    handlerGetResetRequestOutstanding :: DynamicPath -> ServerPartE Response
    handlerGetResetRequestOutstanding dpath = do
        nonce                    <- nonceInPath dpath
        ResetInfo{resetUserId}   <- lookupResetInfo nonce
        uinfo@UserInfo{userName} <- lookupUserInfo resetUserId
        mudetails                <- queryUserDetails resetUserId
        AccountDetails{..}       <- guardSuitableAccountType uinfo mudetails

        template <- getTemplate templates "ResetConfirm.html"
        resp 202 $ toResponse $
          template
            [ "realname"  $= accountName
            , "username"  $= display userName
            , "useremail" $= accountContactEmail
            , "posturl"   $= renderResource resetRequestResource
                                [renderNonce nonce]
            ]

    handlerPostResetRequestOutstanding :: DynamicPath -> ServerPartE Response
    handlerPostResetRequestOutstanding dpath = do
        nonce                    <- nonceInPath dpath
        ResetInfo{resetUserId}   <- lookupResetInfo nonce
        uinfo@UserInfo{userName} <- lookupUserInfo resetUserId
        mudetails                <- queryUserDetails resetUserId
        AccountDetails{..}       <- guardSuitableAccountType uinfo mudetails

        (passwd, passwdRepeat) <- lookPasswd
        when (passwd /= passwdRepeat) errPasswdMismatch

        let userauth = newUserAuth userName (PasswdPlain passwd)

        updateDeleteSignupResetInfo nonce
        updateSetUserAuth resetUserId userauth >>= maybe (return ()) errDeleted
        --TODO: confirmation page?
        seeOther (userPageUri userResource "" userName) (toResponse ())
      where
        lookPasswd = body $ (,) <$> look "password"
                                <*> look "repeat-password"
        errPasswdMismatch =
          errBadRequest "Password mismatch"
            [MText $ "The two copies of the password did not match. "
                  ++ "Check and try again."]

        errDeleted (Right Users.ErrDeletedUser) =
          errForbidden "Account deleted"
            [MText "Cannot set a new password as the user account has just been deleted."]
        errDeleted (Left Users.ErrNoSuchUserId) =
          errInternalError [MText "No such user id!"]

accountSuitableForPasswordReset :: UserInfo -> AccountDetails -> Bool
accountSuitableForPasswordReset
    (UserInfo { userStatus = AccountEnabled{} })
    (AccountDetails { accountKind = Just AccountKindRealUser })
                                    = True
accountSuitableForPasswordReset _ _ = False
