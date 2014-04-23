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
    guardAuthenticated, checkAuthenticated,
    guardPriviledged,   checkPriviledged,
    PrivilegeCondition(..),

    -- ** Errors
    AuthError(..),
    authErrorResponse,
  ) where

import Distribution.Server.Users.Types (UserId, UserName(..), UserAuth(..), UserInfo)
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Framework.AuthCrypt
import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.Error
import Distribution.Server.Framework.HtmlFormWrapper (rqRealMethod)

import Happstack.Server

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS -- Only used for Digest headers

import Control.Monad
import qualified Data.ByteString.Base64 as Base64
import Data.Char (intToDigit, isAsciiLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (listToMaybe)
import Data.List  (intercalate)


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
guardAuthorised :: RealmName -> Users.Users -> [PrivilegeCondition]
                -> ServerPartE UserId
guardAuthorised realm users privconds = do
    (uid, _) <- guardAuthenticated realm users
    guardPriviledged users uid privconds
    return uid


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
    authres <- checkAuthenticated realm users
    case authres of
      Left  autherr -> throwError =<< authErrorResponse realm autherr
      Right info    -> return info

checkAuthenticated :: ServerMonad m => RealmName -> Users.Users -> m (Either AuthError (UserId, UserInfo))
checkAuthenticated realm users = do
    req <- askRq
    return $ case getHeaderAuth req of
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


data PrivilegeCondition = InGroup    Group.UserGroup
                        | IsUserId   UserId
                        | AnyKnownUser

-- | Check that a given user is permitted to perform certain priviledged
-- actions.
--
-- This is based on whether the user is a mamber of a particular group of
-- priviledged users.
--
-- It only checks if the user is in the priviledged user group, it does not
-- imply that the current client has been authenticated, see 'guardAuthorised'.
--
guardPriviledged :: Users.Users -> UserId -> [PrivilegeCondition] -> ServerPartE ()
guardPriviledged users uid privconds = do
  allok <- checkPriviledged users uid privconds
  when (not allok) $
    errForbidden "Forbidden" [MText "No access for this resource."]

checkPriviledged :: MonadIO m => Users.Users -> UserId -> [PrivilegeCondition] -> m Bool
checkPriviledged _users _uid [] = return False

checkPriviledged users uid (InGroup ugroup:others) = do
  uset <- liftIO $ Group.queryUserList ugroup
  if Group.member uid uset
    then return True
    else checkPriviledged users uid others

checkPriviledged users uid (IsUserId uid':others) =
  if uid == uid'
    then return True
    else checkPriviledged users uid others

checkPriviledged _ _ (AnyKnownUser:_) = return True


------------------------------------------------------------------------
-- Basic auth method
--

-- | Use HTTP Basic auth to authenticate the client as an active enabled user.
--
checkBasicAuth :: Users.Users -> RealmName -> BS.ByteString
               -> Either AuthError (UserId, UserInfo)
checkBasicAuth users realm ahdr = do
    authInfo     <- getBasicAuthInfo realm ahdr       ?! UnrecognizedAuthError
    let uname    = basicUsername authInfo
    (uid, uinfo) <- Users.lookupUserName uname users  ?! NoSuchUserError uname
    uauth        <- getUserAuth uinfo                 ?! UserStatusError uid uinfo
    let passwdhash = getPasswdHash uauth
    guard (checkBasicAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid uinfo
    return (uid, uinfo)

getBasicAuthInfo :: RealmName -> BS.ByteString -> Maybe BasicAuthInfo
getBasicAuthInfo realm authHeader
  | Just (username, pass) <- splitHeader authHeader
  = Just BasicAuthInfo {
           basicRealm    = realm,
           basicUsername = UserName username,
           basicPasswd   = PasswdPlain pass
         }
  | otherwise = Nothing
  where
    splitHeader h = case Base64.decode h of
                    Left _ -> Nothing
                    Right xs ->
                        case break (':' ==) $ BS.unpack xs of
                        (username, ':' : pass) -> Just (username, pass)
                        _ -> Nothing

{-
We don't actually want to offer basic auth. It's not something we want to
encourage and some browsers (like firefox) end up prompting the user for
failing auth once for each auth method that the server offers. So if we offer
both digest and auth then the user gets prompted twice when they try to cancel
the auth.

Note that we still accept basic auth if the client offers it pre-emptively.

headerBasicAuthChallenge :: RealmName -> (String, String)
headerBasicAuthChallenge (RealmName realmName) =
    (headerName, headerValue)
  where
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
-}

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
    authInfo     <- getDigestAuthInfo ahdr req         ?! UnrecognizedAuthError
    let uname    = digestUsername authInfo
    (uid, uinfo) <- Users.lookupUserName uname users   ?! NoSuchUserError uname
    uauth        <- getUserAuth uinfo                  ?! UserStatusError uid uinfo
    let passwdhash = getPasswdHash uauth
    guard (checkDigestAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid uinfo
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
                      `mplus`
                      return QopNone
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
          theName <- Parse.munch1 isAsciiLower
          void $ Parse.char '='
          theValue <- quotedString
          return (theName, theValue)

        quotedString :: Parse.ReadP String
        quotedString =
          join Parse.between
               (Parse.char '"')
               (Parse.many $ (Parse.char '\\' >> Parse.get) Parse.<++ Parse.satisfy (/='"'))
              Parse.<++ (liftM2 (:) (Parse.satisfy (/='"')) (Parse.munch (/=',')))

headerDigestAuthChallenge :: RealmName -> IO (String, String)
headerDigestAuthChallenge (RealmName realmName) = do
    nonce <- liftIO generateNonce
    return (headerName, headerValue nonce)
  where
    headerName = "WWW-Authenticate"
    -- Note that offering both qop=\"auth,auth-int\" can confuse some browsers
    -- e.g. see http://code.google.com/p/chromium/issues/detail?id=45194
    headerValue nonce =
      "Digest " ++
      intercalate ", "
        [ "realm="     ++ inQuotes realmName
        , "qop="       ++ inQuotes "auth"
        , "nonce="     ++ inQuotes nonce
        , "opaque="    ++ inQuotes ""
        ]
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen
    inQuotes s = '"' : s ++ ['"']


------------------------------------------------------------------------
-- Common
--

getUserAuth :: UserInfo -> Maybe UserAuth
getUserAuth userInfo =
  case Users.userStatus userInfo of
    Users.AccountEnabled auth -> Just auth
    _                         -> Nothing

getPasswdHash :: UserAuth -> PasswdHash
getPasswdHash (UserAuth hash) = hash


------------------------------------------------------------------------
-- Errors
--

data AuthError = NoAuthError
               | UnrecognizedAuthError
               | NoSuchUserError       UserName
               | UserStatusError       UserId UserInfo
               | PasswordMismatchError UserId UserInfo
  deriving Show

authErrorResponse :: MonadIO m => RealmName -> AuthError -> m ErrorResponse
authErrorResponse realm autherr = do
    digestHeader   <- liftIO (headerDigestAuthChallenge realm)
    return $! (toErrorResponse autherr) { errorHeaders = [digestHeader] }
  where
    toErrorResponse :: AuthError -> ErrorResponse
    toErrorResponse NoAuthError =
      ErrorResponse 401 [] "No authorization provided" []

    toErrorResponse UnrecognizedAuthError =
      ErrorResponse 400 [] "Authorization scheme not recognized" []

    -- we don't want to leak info for the other cases, so same message for them all:
    toErrorResponse _ =
      ErrorResponse 401 [] "Username or password incorrect" []

