{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
   TypeFamilies, TemplateHaskell,
   RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup (
    initUserSignupFeature,
    UserSignupFeature(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails

import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users

import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Pages.Util (makeInput)
import Text.XHtml.Strict hiding (base, body)
import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Table (simpleTable)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Base16 as Base16

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Text (display)
import Data.Version
import Data.Time (UTCTime, getCurrentTime)
import Text.CSV (CSV, Record)
import System.IO
import Network.Mail.Mime
import Network.URI (URIAuth(..))


-- | A feature to allow open account signup, and password reset,
-- both with email confirmation.
--
data UserSignupFeature = UserSignupFeature {
    userSignupFeatureInterface :: HackageFeature
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

newtype Nonce = Nonce ByteString
  deriving (Eq, Ord, Show, Typeable, MemSize)

emptySignupResetTable :: SignupResetTable
emptySignupResetTable = SignupResetTable Map.empty

instance MemSize SignupResetInfo where
    memSize (SignupInfo a b c d) = memSize4 a b c d
    memSize (ResetInfo  a b)     = memSize2 a b

$(deriveSafeCopy 0 'base ''SignupResetInfo)
$(deriveSafeCopy 0 'base ''SignupResetTable)
$(deriveSafeCopy 0 'base ''Nonce)

------------------------------
-- Nonces
--

newRandomNonce :: IO Nonce
newRandomNonce = do
  raw <- withFile "/dev/urandom" ReadMode $ \h ->
           BS.hGet h 10
  return $! Nonce (Base16.encode raw)

renderNonce :: Nonce -> String
renderNonce (Nonce nonce) = BS.unpack nonce

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

makeAcidic ''SignupResetTable [
    --queries
    'getSignupResetTable,
    'lookupSignupResetInfo,
    --updates
    'replaceSignupResetTable,
    'addSignupResetInfo,
    'deleteSignupResetInfo
  ]


---------------------
-- State components
--

signupResetStateComponent :: FilePath -> IO (StateComponent SignupResetTable)
signupResetStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserSignupReset") emptySignupResetTable
  return StateComponent {
      stateDesc    = "State to keep track of outstanding requests for user signup and password resets"
    , acidState    = st
    , getState     = query st GetSignupResetTable
    , putState     = update st . ReplaceSignupResetTable
    , backupState  = \users -> [] -- [csvToBackup ["users.csv"] (userDetailsToCSV users)]
    , restoreState = restoreBackupUnimplemented -- userDetailsBackup
    , resetState   = signupResetStateComponent
    }

----------------------------
-- Data backup and restore
--
{-
userDetailsBackup :: RestoreBackup UserDetailsTable
userDetailsBackup = updateUserBackup emptyUserDetailsTable

updateUserBackup :: UserDetailsTable -> RestoreBackup UserDetailsTable
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importUserDetails csv users
        return (updateUserBackup users')
      _ ->
        return (updateUserBackup users)
  , restoreFinalize =
     return users
  }

importUserDetails :: CSV -> UserDetailsTable -> Restore UserDetailsTable
importUserDetails = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> UserDetailsTable -> Restore UserDetailsTable
    fromRecord [idStr, nameStr, emailStr, kindStr, notesStr] (UserDetailsTable tbl) = do
        UserId uid <- parseText "user id" idStr
        akind      <- parseKind kindStr
        let udetails = AccountDetails {
                        accountName         = T.pack nameStr,
                        accountContactEmail = T.pack emailStr,
                        accountKind         = akind,
                        accountAdminNotes   = T.pack notesStr
                      }
        return $! UserDetailsTable (IntMap.insert uid udetails tbl)

    fromRecord x _ = fail $ "Error processing user details record: " ++ show x

    parseKind ""        = return Nothing
    parseKind "real"    = return (Just AccountKindRealUser)
    parseKind "special" = return (Just AccountKindSpecial)
    parseKind sts       = fail $ "unable to parse account kind: " ++ sts

userDetailsToCSV :: UserDetailsTable -> CSV
userDetailsToCSV (UserDetailsTable tbl)
    = ([showVersion userCSVVer]:) $
      (userdetailsCSVKey:) $

      flip map (IntMap.toList tbl) $ \(uid, udetails) ->
      [ display (UserId uid)
      , T.unpack (accountName udetails)  --FIXME: apparently the csv lib doesn't do unicode properly
      , T.unpack (accountContactEmail udetails)
      , infoToAccountKind udetails
      , T.unpack (accountAdminNotes udetails)
      ]

 where
    userdetailsCSVKey =
       [ "uid"
       , "realname"
       , "email"
       , "kind"
       , "notes"
       ]
    userCSVVer = Version [0,2] []

    -- one of "enabled" "disabled" or "deleted"
    infoToAccountKind :: AccountDetails -> String
    infoToAccountKind udetails = case accountKind udetails of
      Nothing                  -> ""
      Just AccountKindRealUser -> "real"
      Just AccountKindSpecial  -> "special"
-}
----------------------------------------
-- Feature definition & initialisation
--

initUserSignupFeature :: ServerEnv
                      -> UserFeature
                      -> UserDetailsFeature
                      -> IO UserSignupFeature
initUserSignupFeature env@ServerEnv{serverStateDir} users userdetails = do

  -- Canonical state
  signupResetState <- signupResetStateComponent serverStateDir

  let feature = userSignupFeature env users userdetails
                                  signupResetState

  return feature


userSignupFeature :: ServerEnv
                  -> UserFeature
                  -> UserDetailsFeature
                  -> StateComponent SignupResetTable
                  -> UserSignupFeature
userSignupFeature env UserFeature{..} UserDetailsFeature{..} signupResetState
  = UserSignupFeature {..}

  where
    userSignupFeatureInterface = (emptyHackageFeature "user-signup-reset") {
        featureDesc      = "Extra information about user accounts, email addresses etc."
      , featureResources = [signupRequestsResource,
                            signupRequestResource,
                            resetRequestsResource,
                            resetRequestResource]
      , featureState     = [abstractStateComponent signupResetState]
      , featureCaches    = []
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

    -- Request handlers
    --

    nonceInPath :: MonadPlus m => DynamicPath -> m Nonce
    nonceInPath dpath = maybe mzero return (Nonce . BS.pack <$> lookup "nonce" dpath)

    lookupSignupInfo :: Nonce -> ServerPartE SignupResetInfo
    lookupSignupInfo nonce = querySignupInfo nonce
                         >>= maybe (errNoNonce "account signup") return

    lookupResetInfo :: Nonce -> ServerPartE SignupResetInfo
    lookupResetInfo nonce = queryResetInfo nonce
                        >>= maybe (errNoNonce "password reset") return

    errNoNonce thing = errNotFound "Not found"
      [MText $ "The " ++ thing ++ " token does not exist. It could be that it "
            ++ "has been used already, or that it has expired."]

    handlerGetSignupRequestNew :: DynamicPath -> ServerPart Response
    handlerGetSignupRequestNew _ =
      serveFile (asContentType "text/html")
                (serverStaticDir env </> "account-request.html")

    handlerPostSignupRequestNew :: DynamicPath -> ServerPart Response
    handlerPostSignupRequestNew _ = do
        (username, realname, useremail) <- lookUserNameEmail
        --TODO: basic sanity check on username, real name and email
        -- we probably want usernames to remain ascii
        -- and real names to be printable + space chars
        -- examples: JanNovak vs Jan Novák
        nonce     <- liftIO newRandomNonce
        timestamp <- liftIO getCurrentTime
        let signupInfo = SignupInfo {
              signupUserName     = username,
              signupRealName     = realname,
              signupContactEmail = useremail,
              nonceTimestamp     = timestamp
            }
        updateAddSignupResetInfo nonce signupInfo
        let mailFrom = Address (Just (T.pack "Hackage website"))
                               (T.pack ("noreply@" ++ uriRegName ourURI))
            mail     = (emptyMail mailFrom) {
              mailTo      = [Address (Just realname) useremail],
              mailHeaders = [(BS.pack "Subject",
                              T.pack "Hackage account confirmation")],
              mailParts   = [[Part (T.pack "text/plain; charset=utf-8")
                                    None Nothing [] mailBody]]
            }
            --TODO: this should be a template:
            mailBody = LBS.unlines $ map LBS.pack
              [ "Dear " ++ T.unpack realname ++ ","
              , ""
              , "We received a request to create a Hackage account"
              , "for you. To create a Hackage account, please follow"
              , "this link:"
              , ""
              , "http://" ++ ourURIStr ++ "/users/register-request/"
                                       ++ renderNonce nonce
              , ""
              , "If you were not expecting this email, our apologies,"
              , "please ignore it."
              , ""
              , "From,"
              , "  The Hackage website at " ++ ourURIStr
              , "  (and on behalf of the site administrators)"
              , "______________________________________________"
              , "Please do not reply to this email. This email"
              , "address is used only for sending email so you"
              , "will not receive a response."
              ]
            ourURI    = serverHostURI env
            ourURIStr = uriRegName ourURI ++ uriPort ourURI

        --TODO: if we need any configuration of sendmail stuff, has to go here
        liftIO $ renderSendMail mail
        resp 202 $ toResponse $ Resource.XHtml $ confirmPage useremail
      where
        lookUserNameEmail = body $ (,,) <$> lookText' "username"
                                        <*> lookText' "realname"
                                        <*> lookText' "email"
        confirmPage useremail =
          hackagePage "Register a new account"
          [ h2 << "Confirmation email sent"
          , paragraph << ("An email has been sent to "
                          +++ bold << T.unpack useremail)
          , paragraph << ("The email will contain a link to a page where you "
                     +++ "can set your password and activate your account. ")
          , paragraph << ("Note that these activation links do eventually "
                     +++ "expire, so don't leave it too long!")
          ]

    handlerGetSignupRequestOutstanding :: DynamicPath -> ServerPart Response
    handlerGetSignupRequestOutstanding dpath = runServerPartE $ do
        nonce <- nonceInPath dpath
        SignupInfo {..} <- lookupSignupInfo nonce
        ok $ toResponse $ Resource.XHtml $
             signupSetPasswordPage nonce
               signupRealName signupUserName signupContactEmail
      where
        signupSetPasswordPage nonce username realname useremail =
          hackagePage "Register a new account"
          [ h2 << "Register a new account"
          , paragraph << "Email confirmation done! "
          , paragraph << "Now you can set your password and create the account."
          , form !
              [theclass "box",
                    XHtml.method "post",
                    action $ renderResource signupRequestResource 
                                [renderNonce nonce]]
            << [ simpleTable [] []
                 [ [toHtml "Your name:",  toHtml $ T.unpack realname]
                 , [toHtml "Login username:",  toHtml $ T.unpack username]
                 , [toHtml "Contact email address:", toHtml $ T.unpack useremail]
                 , makeInput [thetype "password"] "password" "Password"
                 , makeInput [thetype "password"] "repeat-password" "Confirm password"
                 ]
               , paragraph << input ! [thetype "submit", value "Create account"]
               ]
          ]

    handlerPostSignupRequestOutstanding :: DynamicPath -> ServerPart Response
    handlerPostSignupRequestOutstanding dpath = runServerPartE $ do
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

    handlerGetResetRequestNew :: DynamicPath -> ServerPart Response
    handlerGetResetRequestNew _ = fail "TODO"

    handlerPostResetRequestNew :: DynamicPath -> ServerPart Response
    handlerPostResetRequestNew _ = fail "TODO"

    handlerGetResetRequestOutstanding :: DynamicPath -> ServerPart Response
    handlerGetResetRequestOutstanding _ = fail "TODO"

    handlerPostResetRequestOutstanding :: DynamicPath -> ServerPart Response
    handlerPostResetRequestOutstanding _ = fail "TODO"

