{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup (
    initUserSignupFeature,
    UserSignupFeature(..),

    accountSuitableForPasswordReset
  ) where

import qualified Distribution.Server.Features.UserSignup.Acid as Acid
import Distribution.Server.Features.UserSignup.Backup

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails

import Distribution.Server.Users.Group
import Distribution.Server.Users.Types
import Distribution.Server.Util.Nonce
import Distribution.Server.Util.Validators
import qualified Distribution.Server.Users.Users as Users

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS -- Only used for ASCII data
import qualified Data.ByteString.Lazy as BSL

import Distribution.Text (display)
import Data.Time
import Network.Mail.Mime
import Network.URI (URI(..), URIAuth(..))
import Graphics.Captcha
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.SHA256 as SHA256
import Data.String
import Data.Char
import Text.Read (readMaybe)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key


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

---------------------
-- State components
--

signupResetStateComponent :: FilePath -> IO (StateComponent AcidState Acid.SignupResetTable)
signupResetStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserSignupReset") Acid.emptySignupResetTable
  return StateComponent {
      stateDesc    = "State to keep track of outstanding requests for user signup and password resets"
    , stateHandle  = st
    , getState     = query st Acid.GetSignupResetTable
    , putState     = update st . Acid.ReplaceSignupResetTable
    , backupState  = \backuptype tbl ->
        [csvToBackup ["signups.csv"] (signupInfoToCSV backuptype tbl)
        ,csvToBackup ["resets.csv"]  (resetInfoToCSV backuptype tbl)]
    , restoreState = signupResetBackup
    , resetState   = signupResetStateComponent
    }


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
                  -> StateComponent AcidState Acid.SignupResetTable
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
                            captchaResource,
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

    captchaResource =
      (resourceAt "/users/register/captcha") {
        resourceDesc = [ (GET,  "Acid.Get a new captcha") ]
      , resourceGet  = [ ("json", handlerGetCaptcha) ]
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
          queryState signupResetState Acid.GetSignupResetTable
      >>= \(Acid.SignupResetTable tbl) -> return (Map.elems tbl)

    querySignupInfo :: Nonce -> MonadIO m => m (Maybe SignupResetInfo)
    querySignupInfo nonce =
        justSignupInfo <$> queryState signupResetState (Acid.LookupSignupResetInfo nonce)
      where
        justSignupInfo (Just info@SignupInfo{}) = Just info
        justSignupInfo _                        = Nothing

    queryResetInfo :: Nonce -> MonadIO m => m (Maybe SignupResetInfo)
    queryResetInfo nonce =
        justResetInfo <$> queryState signupResetState (Acid.LookupSignupResetInfo nonce)
      where
        justResetInfo (Just info@ResetInfo{}) = Just info
        justResetInfo _                       = Nothing

    updateAddSignupResetInfo :: Nonce -> SignupResetInfo -> MonadIO m => m Bool
    updateAddSignupResetInfo nonce signupInfo =
        updateState signupResetState (Acid.AddSignupResetInfo nonce signupInfo)

    updateDeleteSignupResetInfo :: Nonce -> MonadIO m => m ()
    updateDeleteSignupResetInfo nonce =
        updateState signupResetState (Acid.DeleteSignupResetInfo nonce)

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
          updateState signupResetState (Acid.DeleteAllExpired expire)
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

    hashTimeAndCaptcha :: UTCTime -> String -> BS.ByteString
    hashTimeAndCaptcha timestamp captcha = Base64.encode (SHA256.hash (fromString (show timestamp ++ map toUpper captcha)))

    makeCaptchaHash :: IO (UTCTime, BS.ByteString, BS.ByteString)
    makeCaptchaHash = do
        (code, image) <- makeCaptcha
        timestamp <- getCurrentTime
        pure (timestamp, hashTimeAndCaptcha timestamp code, fromString "data:image/png;base64," <> Base64.encode image)

    handlerGetSignupRequestNew :: DynamicPath -> ServerPartE Response
    handlerGetSignupRequestNew _ = do
        (timestamp, hash, base64image) <- liftIO makeCaptchaHash
        template <- getTemplate templates "SignupRequest.html"
        ok $ toResponse $ template
          [ "timestamp"   $= timestamp
          , "hash"        $= hash
          , "base64image" $= base64image
          ]

    handlerGetCaptcha :: DynamicPath -> ServerPartE Response
    handlerGetCaptcha _ = do
        (timestamp, hash, base64image) <- liftIO makeCaptchaHash
        ok $ toResponse $ Object $ KeyMap.fromList $
          [ (Key.fromString "timestamp"  , String (T.pack (show timestamp)))
          , (Key.fromString "hash"       , String (T.decodeUtf8 hash))
          , (Key.fromString "base64image", String (T.decodeUtf8 base64image))
          ]

    handlerPostSignupRequestNew :: DynamicPath -> ServerPartE Response
    handlerPostSignupRequestNew _ = do
        templateEmail        <- getTemplate templates "SignupConfirmation.email"
        templateConfirmation <- getTemplate templates "SignupEmailSent.html"

        timestamp <- liftIO getCurrentTime

        (username, realname, useremail) <- lookValidFields timestamp

        nonce     <- liftIO (newRandomNonce 10)
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
        lookValidFields now = do
          (username, realname, useremail, captcha, timestampStr, hash) <-
            msum [ body $ (,,,,,) <$> lookText' "username"
                                  <*> lookText' "realname"
                                  <*> lookText' "email"
                                  <*> look      "captcha"
                                  <*> look      "timestamp"
                                  <*> lookBS    "hash"
                 , errBadRequest "Missing form fields" [] ]

          guardValidLookingUserName username
          guardValidLookingName     realname
          guardValidLookingEmail    useremail

          timestamp <- maybe (errBadRequest "Invalid request" [MText "Seems something went wrong with your request."])
            pure (readMaybe timestampStr)

          when (diffUTCTime now timestamp > secondsToNominalDiffTime (10 * 60)) $
            errBadRequest "Problem with captcha" [MText "Oops, The captcha has expired. Please be quick next time!"]

          unless (hashTimeAndCaptcha timestamp captcha == BSL.toStrict hash) $
            errBadRequest "Problem with captcha" [MText "Sorry, the captcha is wrong. Please try sign up again."]

          return (username, realname, useremail)

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
