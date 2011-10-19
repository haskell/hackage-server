-- | Server methods to do user authentication.
--
-- We authenticate clients using HTTP Basic or Digest authentication and we
-- authorise users based on membership of particular user groups.
--
{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Framework.Auth (
    -- * Checking authorisation
    guardAuthorised,
    
    -- ** Realms
    RealmName,
    hackageRealm,
    adminRealm,

    -- ** Creating password hashes
    newPasswdHash,
    UserName,
    PasswdPlain,
    PasswdHash,

    -- ** Special cases
    guardAuthenticated,
    guardPriviledged,

    -- * deprecatged
    withHackageAuth
  ) where

import Distribution.Server.Users.Types (UserId, UserName(..), UserAuth(..), UserInfo(userName))
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import Distribution.Server.Framework.AuthCrypt
import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.Error
import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Pages.Util (makeInput)
import Distribution.Server.Util.Happstack (rqRealMethod)
import Distribution.Server.Util.ContentType

import Distribution.Text (display)

import Happstack.Server
import qualified Happstack.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS

import Text.XHtml.Table (simpleTable)
import Text.XHtml.Strict
import qualified Text.XHtml.Strict as XHtml

import Control.Monad (guard, join, liftM2, mzero)
import Control.Monad.Error.Class (Error, noMsg)
import Data.Char (intToDigit, isAsciiLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (listToMaybe)
import Data.List  (intercalate)


------------------------------------------------------------------------
-- The old deprecated interface
--

{-# DEPRECATED withHackageAuth "use guardAuthorised instead" #-}
withHackageAuth :: Users.Users -> Maybe Group.UserList
                -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
withHackageAuth users mgroup action = do
    (uid, uinfo) <- guardAuthenticated hackageRealm users
    maybe (return ()) (\group -> guardPriviledged group uid) mgroup
    action uid uinfo


------------------------------------------------------------------------
-- Main auth methods
--

hackageRealm, adminRealm :: RealmName
hackageRealm = RealmName "Hackage"
adminRealm   = RealmName "Hackage admin"


-- | Check that the client is authenticated and is authorised to perform some
-- priviledged action.
--
-- We check that:
--
--   * the client has supplied appropriate authentication credentials for a
--      known enabled user account;
--   * is a member of a given group of users who are permitted to perform
--     certain priviledged actions.
--
guardAuthorised :: RealmName -> Users.Users -> Group.UserList
                -> ServerPartE (UserId, UserInfo)
guardAuthorised realm users group = do
    (uid, uinfo) <- guardAuthenticated realm users
    guardPriviledged group uid
    return (uid, uinfo)


-- | Check that the client is authenticated. Returns the information about the
-- user account that the client authenticates as.
--
-- This checks the client has supplied appropriate authentication credentials
-- for a known enabled user account.
--
-- It only checks the user is known, it does not imply that the user is
-- authorised to do anything in particular, see 'guardAuthorised'.
--
guardAuthenticated :: RealmName -> Users.Users -> ServerPartE (UserId, UserInfo)
guardAuthenticated realm users = do
    req <- askRq
    either (authError realm) return $
      case getHeaderAuth req of
        Just (BasicAuth,  ahdr) -> checkBasicAuth  users realm ahdr
        Just (DigestAuth, ahdr) -> checkDigestAuth users       ahdr req
        Nothing                 -> Left NoAuthError
  where
    getHeaderAuth :: Request -> Maybe (AuthType, BS.ByteString)
    getHeaderAuth req =
        case getHeader "authorization" req of
          Just hdr
            |  BS.isPrefixOf (BS.pack "Digest ") hdr
            -> Just (DigestAuth, BS.drop 7 hdr)

            |  BS.isPrefixOf (BS.pack "Basic ") hdr
            -> Just (BasicAuth,  BS.drop 6 hdr)
          _ -> Nothing

data AuthType = BasicAuth | DigestAuth


-- | Check that a given user is permitted to perform certain priviledged
-- actions.
--
-- This is based on whether the user is a mamber of a particular group of
-- priviledged users.
--
-- It only checks if the user is in the priviledged user group, it does not
-- imply that the current client has been authenticated, see 'guardAuthorised'.
--
guardPriviledged :: Group.UserList -> UserId -> ServerPartE ()
guardPriviledged ugroup uid
  | Group.member uid ugroup = return ()
  | otherwise = errForbidden "Forbidden" [MText "No access for this page."]


------------------------------------------------------------------------
-- Basic auth method
--

-- | Use HTTP Basic auth to authenticate the client as an active enabled user.
--
checkBasicAuth :: Users.Users -> RealmName -> BS.ByteString
               -> Either AuthError (UserId, UserInfo)
checkBasicAuth users realm ahdr = do
    authInfo   <- getBasicAuthInfo realm ahdr       ?! UnrecognizedAuthError
    let uname  = basicUsername authInfo
    uid        <- Users.lookupName uname users      ?! NoSuchUserError
    uinfo      <- Users.lookupId   uid   users      ?! NoSuchUserError
    uauth      <- getUserAuth uinfo                 ?! NoSuchUserError
    passwdhash <- getPasswdHash uauth               ?! OldAuthError (userName uinfo)
    guard (checkBasicAuthInfo passwdhash authInfo)  ?! PasswordMismatchError
    return (uid, uinfo)

getBasicAuthInfo :: RealmName -> BS.ByteString -> Maybe BasicAuthInfo
getBasicAuthInfo realm authHeader
  | (name, ':':pass) <- splitHeader authHeader
  = Just BasicAuthInfo {
           basicRealm    = realm,
           basicUsername = UserName name,
           basicPasswd   = PasswdPlain pass
         }
  | otherwise = Nothing
  where
    splitHeader = break (':'==) . Base64.decode . BS.unpack

setBasicAuthChallenge :: RealmName -> ServerPartE ()
setBasicAuthChallenge (RealmName realmName) = do
    addHeaderM headerName headerValue
  where
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""


------------------------------------------------------------------------
-- Digest auth method
--

-- See RFC 2617 http://www.ietf.org/rfc/rfc2617

-- Digest auth TODO:
-- * support domain for the protection space (otherwise defaults to whole server)
-- * nonce generation is not ideal: consists just of a random number
-- * nonce is not checked
-- * opaque is not used

-- | Use HTTP Digest auth to authenticate the client as an active enabled user.
--
checkDigestAuth :: Users.Users -> BS.ByteString -> Request
                -> Either AuthError (UserId, UserInfo)
checkDigestAuth users ahdr req = do
    authInfo   <- getDigestAuthInfo ahdr req         ?! UnrecognizedAuthError
    let uname  = digestUsername authInfo
    uid        <- Users.lookupName uname users       ?! NoSuchUserError
    uinfo      <- Users.lookupId uid users           ?! NoSuchUserError
    uauth      <- getUserAuth uinfo                  ?! NoSuchUserError
    passwdhash <- getPasswdHash uauth                ?! OldAuthError (userName uinfo)
    guard (checkDigestAuthInfo passwdhash authInfo)  ?! PasswordMismatchError
    -- TODO: if we want to prevent replay attacks, then we must check the
    -- nonce and nonce count and issue stale=true replies.
    return (uid, uinfo)

-- | retrieve the Digest auth info from the headers
--
getDigestAuthInfo :: BS.ByteString -> Request -> Maybe DigestAuthInfo
getDigestAuthInfo authHeader req = do
    authMap    <- parseDigestHeader authHeader
    username   <- Map.lookup "username" authMap
    nonce      <- Map.lookup "nonce"    authMap
    response   <- Map.lookup "response" authMap
    uri        <- Map.lookup "uri"      authMap
    let mb_qop  = Map.lookup "qop"      authMap
    qopInfo    <- case mb_qop of
                    Just "auth" -> do
                      nc     <- Map.lookup "nc"     authMap
                      cnonce <- Map.lookup "cnonce" authMap
                      return (QopAuth nc cnonce)
                    Nothing -> return QopNone
                    _       -> mzero
    return DigestAuthInfo {
       digestUsername = UserName username,
       digestNonce    = nonce,
       digestResponse = response,
       digestURI      = uri,
       digestRqMethod = show (rqRealMethod req),
       digestQoP      = qopInfo
    }
  where
    -- Parser derived from RFCs 2616 and 2617
    parseDigestHeader :: BS.ByteString -> Maybe (Map String String)
    parseDigestHeader =
        fmap Map.fromList . parse . BS.unpack
      where
        parse :: String -> Maybe [(String, String)]
        parse s = listToMaybe [ x | (x, "") <- Parse.readP_to_S parser s ]

        parser :: Parse.ReadP [(String, String)]
        parser = Parse.skipSpaces
              >> Parse.sepBy1 nameValuePair
                       (Parse.skipSpaces >> Parse.char ',' >> Parse.skipSpaces)

        nameValuePair = do
          name <- Parse.munch1 isAsciiLower
          Parse.char '='
          value <- quotedString
          return (name, value)

        quotedString :: Parse.ReadP String
        quotedString =
          join Parse.between
               (Parse.char '"')
               (Parse.many $ (Parse.char '\\' >> Parse.get) Parse.<++ Parse.satisfy (/='"'))
              Parse.<++ (liftM2 (:) (Parse.satisfy (/='"')) (Parse.munch (/=',')))

setDigestAuthChallenge :: RealmName -> ServerPartE ()
setDigestAuthChallenge (RealmName realmName) = do
    nonce <- liftIO generateNonce
    addHeaderM headerName (headerValue nonce)
  where
    headerName = "WWW-Authenticate"
    -- Note that offering both qop=\"auth,auth-int\" can confuse some browsers
    -- e.g. see http://code.google.com/p/chromium/issues/detail?id=45194
    headerValue nonce =
      "Digest " ++
      intercalate ", "
        [ "realm="     ++ quote realmName
        , "qop="       ++ quote "auth"
        , "nonce="     ++ quote nonce
        , "opaque="    ++ quote ""
        ]
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen
    quote s = '"' : s ++ ['"']


------------------------------------------------------------------------
-- Common
--

getUserAuth :: UserInfo -> Maybe UserAuth
getUserAuth userInfo =
  case Users.userStatus userInfo of
    Users.Active _ auth -> Just auth
    _                   -> Nothing

getPasswdHash :: UserAuth -> Maybe PasswdHash
getPasswdHash (NewUserAuth hash) = Just hash
getPasswdHash (OldUserAuth _)    = Nothing

-- | The \"oh noes?!\" operator
--
(?!) :: Maybe a -> e -> Either e a
ma ?! e = maybe (Left e) Right ma


------------------------------------------------------------------------
-- Errors
--

authError :: RealmName -> AuthError -> ServerPartE a
authError realm err = do
  -- we want basic first, but addHeaderM makes them come out reversed
  setDigestAuthChallenge realm
  setBasicAuthChallenge  realm
  req <- askRq
  let accepts = maybe [] (parseContentAccept . BS.unpack) (getHeader "Accept" req)
      want_text = foldr (\ct rest_want_text -> case ct of ContentType { ctType = "text", ctSubtype = st }
                                                            | st `elem` ["html", "xhtml"] -> False
                                                            | st == "plain"               -> True
                                                          _ -> rest_want_text) True accepts
  finishWith $ (showAuthError want_text (rqPeer req) err) {
                     rsCode = case err of
                                UnrecognizedAuthError -> 400
                                OldAuthError _        -> 403 -- Fits in that authenticating will make no difference...
                                _                     -> 401
                   }

data AuthError = NoAuthError | UnrecognizedAuthError | NoSuchUserError
               | PasswordMismatchError | OldAuthError UserName
  deriving Show

instance Error AuthError where
    noMsg = NoAuthError

showAuthError :: Bool -> Host -> AuthError -> Response
showAuthError want_text (hostname, port) err = case err of
    NoAuthError           -> toResponse "No authorization provided."
    UnrecognizedAuthError -> toResponse "Authorization scheme not recognized."
    NoSuchUserError       -> toResponse "Username or password incorrect."
    PasswordMismatchError -> toResponse "Username or password incorrect."
    OldAuthError uname 
      | want_text -> toResponse $ "Hackage has been upgraded to use more secure passwords. You need login to Hackage and reenter your password at http://" ++ hostname ++ ":" ++ show port ++ rel_url
      | otherwise -> toResponse $ Resource.XHtml $ hackagePage "Change password"
          [ toHtml "You haven't logged in since Hackage was upgraded. Please reenter your password below to upgrade your account."
          , form ! [theclass "box", XHtml.method "POST", action rel_url] <<
                [ simpleTable [] []
                    [ makeInput [thetype "password"] "password" "Old password"
                    , makeInput [thetype "password"] "repeat-password" "Repeat old password"
                    ]
                , hidden "try-upgrade" "1"
                , hidden "_method" "PUT" --method override
                , paragraph << input ! [thetype "submit", value "Upgrade password"]
                ]
          ]
      where rel_url = "/user/" ++ display uname ++ "/password"
