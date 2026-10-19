-- | Server methods to do user authentication.
--
-- We authenticate clients using HTTP Basic or Digest authentication and we
-- authorise users based on membership of particular user groups.
--
{-# LANGUAGE LambdaCase, PatternGuards, NamedFieldPuns #-}
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
    guardInAnyGroup,
    PrivilegeCondition(..),

    -- ** Looking up users
    UserLookup(..),

    -- ** Errors
    AuthError(..),
    authErrorResponse,

    -- ** Internal details
    AuthMethod(..),
    probeAttemptedAuthMethod,
  ) where

import qualified Data.Text as T
import Distribution.Server.Users.Types (UserId, UserName(..), UserAuth(..), UserInfo, AuthToken)
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Framework.AuthCrypt
import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.Error
import Distribution.Server.Framework.HtmlFormWrapper (rqRealMethod)
import Distribution.Server.Framework.ServerEnv (ServerEnv(ServerEnv, serverRequiredBaseHostHeader), getHost)

import Happstack.Server

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS -- Only used for Digest headers

import Control.Monad
import qualified Data.ByteString.Base64 as Base64
import Data.Char (intToDigit, isAsciiLower, isAscii, isAlphaNum, toLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (listToMaybe)
import Data.List  (intercalate)
import qualified Data.Text.Encoding as T


------------------------------------------------------------------------
-- Main auth methods
--

hackageRealm, adminRealm :: RealmName
hackageRealm = RealmName "Hackage"
adminRealm   = RealmName "Hackage admin"

-- | How to find the user account that a client claims to be.
data UserLookup = UserLookup {
    lookupUserByName      :: UserName  -> IO (Maybe (UserId, UserInfo)),
    lookupUserByAuthToken :: AuthToken -> IO (Maybe (UserId, UserInfo))
  }


-- | Check that the client is authenticated and is authorised to perform some
-- privileged action.
--
-- We check that:
--
--   * the client has supplied appropriate authentication credentials for a
--      known enabled user account;
--   * is a member of a given group of users who are permitted to perform
--     certain privileged actions.
--
guardAuthorised :: RealmName -> UserLookup -> [PrivilegeCondition]
                -> ServerEnv
                -> ServerPartE UserId
guardAuthorised realm users privconds env = do
    uid <- guardAuthenticated realm users env
    guardPriviledged uid privconds
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
guardAuthenticated :: RealmName -> UserLookup -> ServerEnv -> ServerPartE UserId
guardAuthenticated realm users env = do
    authres <- checkAuthenticated realm users env
    case authres of
      Left  autherr -> throwError =<< authErrorResponse realm autherr
      Right info    -> return info

checkAuthenticated :: (ServerMonad m, MonadIO m) => RealmName -> UserLookup -> ServerEnv -> m (Either AuthError UserId)
checkAuthenticated realm users ServerEnv { serverRequiredBaseHostHeader } = do
    mbHost <- getHost
    case mbHost of
       Just hostHeaderValue ->
         if hostHeaderValue /= T.encodeUtf8 (T.pack serverRequiredBaseHostHeader)
            then pure $ Left BadHost
              { actualHost=Just hostHeaderValue
              , oughtToBeHost=serverRequiredBaseHostHeader
              }
            else goCheck
       Nothing -> goCheck
  where
    goCheck = do
         req <- askRq
         liftIO $ case getHeaderAuth req of
           Just (DigestAuth, ahdr) -> checkDigestAuth users       ahdr req
           Just _ | plainHttp req  -> return (Left InsecureAuthError)
           Just (BasicAuth,  ahdr) -> checkBasicAuth  users realm ahdr
           Just (AuthToken,  ahdr) -> checkTokenAuth  users       ahdr
           Nothing                 -> return (Left NoAuthError)

-- | Authentication methods supported by hackage-server.
data AuthMethod
  = -- | HTTP Basic authentication.
    BasicAuth
  | -- | HTTP Digest authentication.
    DigestAuth
  | -- | Authentication usinng an API token via the @X-ApiKey@ header.
    AuthToken

getHeaderAuth :: Request -> Maybe (AuthMethod, BS.ByteString)
getHeaderAuth req =
  case getHeader "authorization" req of
    Just hdr
      |  BS.isPrefixOf (BS.pack "Digest ") hdr
      -> Just (DigestAuth, BS.drop 7 hdr)
      |  BS.isPrefixOf (BS.pack "X-ApiKey ") hdr
      -> Just (AuthToken, BS.drop 9 hdr)
      |  BS.isPrefixOf (BS.pack "Basic ") hdr
      -> Just (BasicAuth,  BS.drop 6 hdr)
    _ -> Nothing

-- | Reads the request headers to determine which @AuthMethod@ the client has attempted to use, if
-- any. Note that this does not /validate/ the authentication credentials.
probeAttemptedAuthMethod :: Request -> Maybe AuthMethod
probeAttemptedAuthMethod = fmap fst . getHeaderAuth

data PrivilegeCondition = InGroup    Group.UserGroup
                        | IsUserId   UserId
                        | AnyKnownUser

guardInAnyGroup :: UserId -> [Group.UserGroup] -> ServerPartE ()
guardInAnyGroup _ [] =
  fail "Group list is empty, this is not meant to happen"
guardInAnyGroup uid groups = do
  allok <- checkPriviledged uid (map InGroup groups)
  let groupTitles = map (Group.groupTitle . Group.groupDesc) groups
      errMsg = "Access denied, you must be member of either of the following groups: "
               <> show groupTitles
  when (not allok) $
    errForbidden "Missing group membership" [MText errMsg]

-- | Check that a given user is permitted to perform certain privileged
-- actions.
--
-- This is based on whether the user is a member of a particular group of
-- privileged users.
--
-- It only checks if the user is in the privileged user group, it does not
-- imply that the current client has been authenticated, see 'guardAuthorised'.
--
guardPriviledged :: UserId -> [PrivilegeCondition] -> ServerPartE ()
guardPriviledged uid privconds = do
  allok <- checkPriviledged uid privconds
  when (not allok) $
    errForbidden "Forbidden" [MText "No access for this resource."]

checkPriviledged :: MonadIO m => UserId -> [PrivilegeCondition] -> m Bool
checkPriviledged _uid [] = return False

checkPriviledged uid (InGroup ugroup:others) = do
  uset <- liftIO $ Group.queryUserGroup ugroup
  if UserIdSet.member uid uset
    then return True
    else checkPriviledged uid others

checkPriviledged uid (IsUserId uid':others) =
  if uid == uid'
    then return True
    else checkPriviledged uid others

checkPriviledged _ (AnyKnownUser:_) = return True


------------------------------------------------------------------------
-- Are we using plain http?
--

-- | The idea here is if you're using https by putting the hackage-server
-- behind a reverse proxy then you can get the proxy to set this header
-- so that we can know if the request is coming in by https or plain http.
--
-- We only reject insecure connections in setups where the proxy passes
-- "Forwarded: proto=http" or "X-Forwarded-Proto: http" for the non-secure
-- rather than rejecting in all setups where no header is provided.
--
plainHttp :: Request -> Bool
plainHttp req
  | Just fwd      <- getHeader "Forwarded" req
  , Just fwdprops <- parseForwardedHeader fwd
  , Just "http"   <- Map.lookup "proto" fwdprops
  = True

  | Just xfwd <- getHeader "X-Forwarded-Proto" req
  , xfwd == BS.pack "http"
  = True

  | otherwise
  = False
  where
    -- "Forwarded" header parser derived from RFC 7239
    -- https://tools.ietf.org/html/rfc7239
    parseForwardedHeader :: BS.ByteString -> Maybe (Map String String)
    parseForwardedHeader =
        fmap Map.fromList . parse . BS.unpack
      where
        parse :: String -> Maybe [(String, String)]
        parse s = listToMaybe [ x | (x, "") <- Parse.readP_to_S parser s ]

        parser :: Parse.ReadP [(String, String)]
        parser = Parse.skipSpaces
              >> Parse.sepBy1 forwardedPair
                       (Parse.skipSpaces >> Parse.char ';' >> Parse.skipSpaces)

        forwardedPair :: Parse.ReadP (String, String)
        forwardedPair = do
          theName <- token
          void $ Parse.char '='
          theValue <- quotedString Parse.+++ token
          return (map toLower theName, theValue)

        token :: Parse.ReadP String
        token = Parse.munch1 (\c -> isAscii c
                                && (isAlphaNum c || c `elem` "!#$%&'*+-.^_`|~"))

        quotedString :: Parse.ReadP String
        quotedString =
          join Parse.between
               (Parse.char '"')
               (Parse.many $ (Parse.char '\\' >> Parse.get)
                    Parse.<++ Parse.satisfy (/='"'))


------------------------------------------------------------------------
-- Auth token method
--

-- | Handle a auth request using an access token
checkTokenAuth :: UserLookup -> BS.ByteString
               -> IO (Either AuthError UserId)
checkTokenAuth users ahdr =
    case Users.parseOriginalToken (T.decodeUtf8 ahdr) of
      Left _    -> return (Left BadApiKeyError)
      Right tok -> do
        muser <- lookupUserByAuthToken users (Users.convertToken tok)
        return $ do
          (uid, uinfo) <- muser ?! BadApiKeyError
          _ <- getUserAuth uinfo ?! UserStatusError uid uinfo
          return uid

------------------------------------------------------------------------
-- Basic auth method
--

-- | Use HTTP Basic auth to authenticate the client as an active enabled user.
--
checkBasicAuth :: UserLookup -> RealmName -> BS.ByteString
               -> IO (Either AuthError UserId)
checkBasicAuth users realm ahdr =
    case getBasicAuthInfo realm ahdr of
      Nothing       -> return (Left UnrecognizedAuthError)
      Just authInfo -> do
        let uname = basicUsername authInfo
        muser <- lookupUserByName users uname
        return $ do
          (uid, uinfo) <- muser                             ?! NoSuchUserError uname
          uauth        <- getUserAuth uinfo                 ?! UserStatusError uid uinfo
          let passwdhash = getPasswdHash uauth
          guard (checkBasicAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid uinfo
          return uid

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
checkDigestAuth :: UserLookup -> BS.ByteString -> Request
                -> IO (Either AuthError UserId)
checkDigestAuth users ahdr req =
    case getDigestAuthInfo ahdr req of
      Nothing       -> return (Left UnrecognizedAuthError)
      Just authInfo -> do
        let uname = digestUsername authInfo
        muser <- lookupUserByName users uname
        return $ do
          (uid, uinfo) <- muser                              ?! NoSuchUserError uname
          uauth        <- getUserAuth uinfo                  ?! UserStatusError uid uinfo
          let passwdhash = getPasswdHash uauth
          guard (checkDigestAuthInfo passwdhash authInfo)    ?! PasswordMismatchError uid uinfo
          -- TODO: if we want to prevent replay attacks, then we must check the
          -- nonce and nonce count and issue stale=true replies.
          return uid

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
               | InsecureAuthError
               | NoSuchUserError       UserName
               | UserStatusError       UserId UserInfo
               | PasswordMismatchError UserId UserInfo
               | BadApiKeyError
               | BadHost { actualHost :: Maybe BS.ByteString, oughtToBeHost :: String }
  deriving Show

authErrorResponse :: MonadIO m => RealmName -> AuthError -> m ErrorResponse
authErrorResponse realm autherr = do
    digestHeader <- liftIO (headerDigestAuthChallenge realm)

    let
      toErrorResponse :: AuthError -> ErrorResponse
      toErrorResponse = \case
        NoAuthError ->
          ErrorResponse 401 [digestHeader] "No authorization provided" []

        UnrecognizedAuthError ->
          ErrorResponse 400 [digestHeader] "Authorization scheme not recognized" []

        InsecureAuthError ->
          ErrorResponse 400 [digestHeader] "Authorization scheme not allowed over plain http"
            [ MText $ "HTTP Basic and X-ApiKey authorization methods leak "
                   ++ "information when used over plain HTTP. Either use HTTPS "
                   ++ "or if you must use plain HTTP for authorised requests then "
                   ++ "use HTTP Digest authentication." ]

        BadApiKeyError ->
          ErrorResponse 401 [digestHeader] "Bad auth token" []

        BadHost {} ->
          ErrorResponse 401 [digestHeader] "Bad host" []

        -- we don't want to leak info for the other cases, so same message for them all:
        _ ->
          ErrorResponse 401 [digestHeader] "Username or password incorrect" []

    return $! toErrorResponse autherr
