module Distribution.Server.Auth.Basic (
    getHackageAuth,
    withHackageAuth,
    requireHackageAuth,
    authorizationRealm
  ) where

import Distribution.Server.Users.Types (UserId, UserName(..), UserInfo)
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Auth.Crypt as Crypt
import Distribution.Server.Auth.Types
import Distribution.Server.Framework.Error

import Happstack.Server
import qualified Happstack.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

import Control.Monad (join, liftM2, mplus)
import Data.Char (intToDigit, isAsciiLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.List (find, intercalate)
import Data.Digest.Pure.MD5 (md5)

authorizationRealm :: String
authorizationRealm = "Hackage"
{-
getHackageAuth :: Users.Users -> ServerPart (Either AuthError (UserId, UserInfo))
getHackageAuth users = askRq >>= \req -> return $ case getAuthType req of
    Just BasicAuth  -> genericBasicAuth req authorizationRealm (getPasswdInfo users)
    Just DigestAuth -> genericDigestAuth req (getPasswdInfo users) --realm hashed in user database
    Nothing -> Left NoAuthError
-}

getHackageAuth :: Users.Users -> ServerPartE (Either AuthError (UserId, UserInfo))
getHackageAuth users = do
    req <- askRq
    case getAuthType req of
      Just BasicAuth  -> return $ genericBasicAuth req authorizationRealm (getPasswdInfo users)
      Just DigestAuth -> return $ genericDigestAuth req (getPasswdInfo users) --realm hashed in user database
      Nothing         -> return $ Left NoAuthError


getAuthType :: Request -> Maybe AuthType
getAuthType req = case getHeader "authorization" req of
      Just h | BS.isPrefixOf (BS.pack "Digest ") h -> Just DigestAuth
             | BS.isPrefixOf (BS.pack "Basic ")  h -> Just BasicAuth
      _ -> Nothing

-- semantic ambiguity: does Nothing mean allow everyone, or only allow enabled? Currently, the former.
-- disabled users might want to perform some authorized action, like login or change their password
withHackageAuth :: Users.Users -> Maybe Group.UserList -> Maybe AuthType ->
                   (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
withHackageAuth users authorizedGroup forceType func = getHackageAuth users >>= \res -> case res of
    Right (userId, info) -> do
        if isAuthorizedFor userId info authorizedGroup
            then func userId info
            else errForbidden "Forbidden" [MText "No access for this page."]
    Left authError -> do
        setHackageAuth forceType
        finishWith =<< unauthorized (toResponse $ showAuthError authError)

setHackageAuth :: Maybe AuthType -> ServerPartE ()
setHackageAuth forceType = do
    req <- askRq
    case forceType `mplus` getAuthType req of
        Just BasicAuth  -> askBasicAuth authorizationRealm
        Just DigestAuth -> askDigestAuth authorizationRealm
        -- the below means that, upon logging in, basic auth will be
        -- required by default. Changing this means migrating (which is
        -- just fine).
        Nothing -> askDigestAuth authorizationRealm
        

-- the UserInfo should belong to the UserId
isAuthorizedFor :: UserId -> UserInfo -> Maybe Group.UserList -> Bool
isAuthorizedFor userId userInfo authorizedGroup = case Users.userStatus userInfo of
    Users.Active Users.Enabled _ -> maybe True (Group.member userId) authorizedGroup
    _ -> False

requireHackageAuth :: Users.Users -> Maybe Group.UserList -> Maybe AuthType -> ServerPart (UserId, UserInfo)
requireHackageAuth users authorizedGroup forceType = do
    runServerPartE $
      withHackageAuth users authorizedGroup forceType $ \uid info -> return (uid, info)

-- Used by both basic and digest auth functions.
getPasswdInfo :: Users.Users -> UserName -> Maybe ((UserId, UserInfo), Users.UserAuth)
getPasswdInfo users userName = do
    userId   <- Users.lookupName userName users
    userInfo <- Users.lookupId userId users
    auth <- case Users.userStatus userInfo of
        Users.Active _ auth -> Just auth
        _                   -> Nothing
    return ((userId, userInfo), auth)

(<?) :: a -> Maybe b -> Either a b
e <? mb = maybe (Left e) Right mb

-- TODO: s/String/[ErrorMessage]/ for appropriate links
showAuthError :: AuthError -> String
showAuthError err = case err of
    NoAuthError -> "No authorization provided."
    UnrecognizedAuthError -> "Authorization scheme not recognized."
    NoSuchUserError -> "Username or password incorrect."
    PasswordMismatchError -> "Username or password incorrect."
    AuthTypeMismatchError -> "You can't use the more secure MD5 digest authentication because the server has already hashed your password in a different format. Try logging in using basic authentication and then submitting a password change request to let the server rehash your password in digest form (recommended)."

--------------------------------------------------------------------------------
genericBasicAuth :: Request -> String -> (UserName -> Maybe (a, Users.UserAuth)) -> Either AuthError a
genericBasicAuth req realmName userDetails = do
    authHeader <- NoAuthError <? getHeader "authorization" req
    (userName, pass) <- UnrecognizedAuthError <? parseHeader authHeader
    (var, Users.UserAuth hash atype) <- NoSuchUserError <? userDetails userName
    let matches = Crypt.checkPasswdBasicAuth realmName userName hash pass
    if matches then Right var else Left PasswordMismatchError
  where
    parseHeader h = case splitHeader h of
      (name, ':':pass) -> Just (UserName name, PasswdPlain pass)
      _                -> Nothing
    splitHeader = break (':'==) . Base64.decode . BS.unpack . BS.drop 6

askBasicAuth :: String -> ServerPartE ()
askBasicAuth realmName = setHeaderM headerName headerValue >> setResponseCode 401
  where
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""

--------------------------------------------------------------------------------
digestPasswdCheck :: Request -> Map String String -> PasswdHash -> Maybe Bool
digestPasswdCheck req authMap (PasswdHash hash1) = do
    nonce <- Map.lookup "nonce" authMap
    response <- Map.lookup "response" authMap
    uri <- Map.lookup "uri" authMap
    let qop = Map.lookup "qop" authMap
        nonceCount = Map.lookup "nc" authMap
        cnonce = Map.lookup "cnonce" authMap --insert (join traceShow) before intercalates to debug
        hash2 = show . md5 . BS.Lazy.pack $ intercalate ":" [show (rqMethod req), uri]
        responseString = show . md5 . BS.Lazy.pack . intercalate ":" $ case (qop, nonceCount, cnonce) of
            (Just qop', Just nonceCount', Just cnonce')
                | qop' == "auth" -> [hash1, nonce, nonceCount', cnonce', qop', hash2]
            _ -> [hash1, nonce, hash2]
    return (responseString == response)

-- This is the digest version of the above code, utilizing the UserInfo map as
-- though it contained MD5 hashes of the form username:hackage:password.
--
-- To use both systems at the same time, determine at the start of a client
-- "session" whether to request Basic or Digest authentication, and then
-- demultiplex when the client sends the Authorization header.
genericDigestAuth :: Request -> (UserName -> Maybe (a, Users.UserAuth)) -> Either AuthError a
genericDigestAuth req userDetails = do
    authHeader <- NoAuthError <? getHeader "authorization" req
    authMap <- UnrecognizedAuthError <? parseDigestResponse (BS.unpack authHeader)
    nameStr <- UnrecognizedAuthError <? Map.lookup "username" authMap
    (var, Users.UserAuth hash atype) <- NoSuchUserError <? userDetails (UserName nameStr)
    case atype of
      BasicAuth  -> Left AuthTypeMismatchError
      DigestAuth -> do
        matches <- UnrecognizedAuthError <? digestPasswdCheck req authMap hash
        if matches then Right var else Left PasswordMismatchError

-- Parser derived straight from RFCs 2616 and 2617
parseDigestResponse :: String -> Maybe (Map String String)
parseDigestResponse = fmap (Map.fromList . fst) . find (null . snd) .
                      -- giving ReadS [(String, String)] = [([(String, String)], String)]
                      Parse.readP_to_S (Parse.string "Digest " >> Parse.skipSpaces >> parser)
  where
    parser :: Parse.ReadP [(String, String)]
    parser = flip Parse.sepBy1 (Parse.char ',') $ do
        Parse.skipSpaces
        liftM2 (,) (Parse.munch1 isAsciiLower) (Parse.char '=' >> quotedString)
    quotedString :: Parse.ReadP String
    quotedString = join Parse.between (Parse.char '"') (Parse.many $ (Parse.char '\\' >> Parse.get) Parse.<++ Parse.satisfy (/='"'))
                      Parse.<++ (liftM2 (:) (Parse.satisfy (/='"')) (Parse.munch (/=',')))

askDigestAuth :: String -> ServerPartE ()
askDigestAuth realmName = do
    nonce <- liftIO generateNonce
    setHeaderM headerName (headerValue nonce)
    setResponseCode 401
  where
    headerName = "WWW-Authenticate"
    -- I would use qop=\"auth,auth-int\", but Google Chrome seems to have problems choosing one
    -- http://code.google.com/p/chromium/issues/detail?id=45194
    headerValue nonce = "Digest realm=\"" ++ realmName ++ "\", qop=\"auth\", nonce=\"" ++ nonce ++ "\", opaque=\"\""
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen

