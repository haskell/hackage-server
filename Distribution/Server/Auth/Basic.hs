module Distribution.Server.Auth.Basic (
   getHackageAuth,
   requireHackageAuth,
  ) where

import Distribution.Server.Users.Types
         ( UserId, UserName(..), PasswdPlain(..) )
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Auth.Crypt as Crypt
import Distribution.Server.Auth.Types

import Happstack.Server
import qualified Happstack.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

import Control.Monad (join, liftM2, mplus, unless)
import Data.Char (intToDigit, isAsciiLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (maybe)
import Data.List (find, intercalate)
import Data.Digest.Pure.MD5 (md5)

getHackageAuth :: Monad m => Users.Users -> ServerPartT m (Either AuthError UserId)
getHackageAuth users = askRq >>= \req -> return $ case getAuthType req of
    Just BasicAuth  -> genericBasicAuth req "hackage" (getPasswdInfo users)
    Just DigestAuth -> genericDigestAuth req (getPasswdInfo users) --realm hashed in user database
    Nothing -> Left NoAuthError

getAuthType :: Request -> Maybe AuthType
getAuthType req = case getHeader "authorization" req of
      Just h | BS.isPrefixOf (BS.pack "Digest ") h -> Just DigestAuth
             | BS.isPrefixOf (BS.pack "Basic ")  h -> Just BasicAuth
      _ -> Nothing

-- semantic ambiguity: does Nothing mean allow everyone, or only allow enabled? Currently, the former.
-- disabled users might want to perform some authorized action, like login or change their password
requireHackageAuth :: MonadIO m => Users.Users -> Maybe Group.UserList -> Maybe AuthType -> ServerPartT m UserId
requireHackageAuth users authorisedGroup forceType = getHackageAuth users >>= \res -> case res of
    Right userId -> do
        let forbid = escape $ forbidden $ toResponse "No access for this page."
        case Users.userStatus `fmap` Users.lookupId userId users of
            Just (Users.Active Users.Disabled _) -> forbid
            _ -> return ()
        unless (maybe True (Group.member userId) authorisedGroup) $ forbid
        return userId
    Left NoAuthError -> makeAuthPage "No authorization provided."
    Left UnrecognizedAuthError -> makeAuthPage "Authorization scheme not recognized."
    Left NoSuchUserError -> makeAuthPage "Username or password incorrect."
    Left PasswordMismatchError -> makeAuthPage "Username or password incorrect."
    -- the complicated migrating case
    Left AuthTypeMismatchError -> makeAuthPage "You can't use the more secure MD5 digest authentication because the server has already hashed your password in a different format. Try logging in using basic authentication and then submitting a password change request to let the server rehash your password in digest form (recommended)."
  where
    makeAuthPage str = do
        req <- askRq
        let response = toResponse $ "401 Unathorized: " ++ str -- todo: render pretty XHTML
            theAsk = case forceType `mplus` getAuthType req of
                Just BasicAuth  -> askBasicAuth
                Just DigestAuth -> askDigestAuth
                Nothing -> askBasicAuth -- for now?
        theAsk "hackage" response

getPasswdInfo :: Users.Users -> UserName -> Maybe (UserId, Users.UserAuth)
getPasswdInfo users userName = do
    userId   <- Users.lookupName userName users
    userInfo <- Users.lookupId userId users
    auth <- case Users.userStatus userInfo of
        Users.Active _ auth -> Just auth
        _                   -> Nothing
    return (userId, auth)

(<?) :: a -> Maybe b -> Either a b
e <? mb = maybe (Left e) Right mb

--------------------------------------------------------------------------------
genericBasicAuth :: Request -> String -> (UserName -> Maybe (a, Users.UserAuth)) -> Either AuthError a
genericBasicAuth req realmName userDetails = do
    authHeader <- NoAuthError <? getHeader "authorization" req
    (userName, pass) <- UnrecognizedAuthError <? parseHeader authHeader
    (var, Users.UserAuth hash atype) <- NoSuchUserError <? userDetails userName
    let matches = case atype of
            BasicAuth  -> Crypt.checkPasswd pass hash
            DigestAuth -> Crypt.newDigestPass userName pass realmName == hash
    if matches then Right var else Left PasswordMismatchError
  where
    parseHeader h = case splitHeader h of
      (name, ':':pass) -> Just (UserName name, PasswdPlain pass)
      _                -> Nothing
    splitHeader = break (':'==) . Base64.decode . BS.unpack . BS.drop 6

-- assumes user is not already authorized
askBasicAuth :: MonadIO m => String -> Response -> ServerPartT m a
askBasicAuth realmName response = escape $ unauthorized $
            addHeader headerName headerValue $ response
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
        hash2 = show . md5 . BS.Lazy.pack $ intercalate ":" [show (rqMethod req), uri] ++ case qop of
            Just "auth-int" -> (':':) . show . md5 $ case rqBody req of Body body -> body
            _ -> ""
        responseString = show . md5 . BS.Lazy.pack . intercalate ":" $ case (qop, nonceCount, cnonce) of
            (Just qop', Just nonceCount', Just cnonce')
                | qop' == "auth" || qop' == "auth-int" -> [hash1, nonce, nonceCount', cnonce', qop', hash2]
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

askDigestAuth :: MonadIO m => String -> Response -> ServerPartT m a
askDigestAuth realmName response = do
    nonce <- liftIO generateNonce
    escape $ unauthorized $ addHeader headerName (headerValue nonce) $ response
  where
    headerName = "WWW-Authenticate"
    -- I would use qop=\"auth,auth-int\", but Google Chrome seems to have problems choosing one
    -- http://code.google.com/p/chromium/issues/detail?id=45194
    headerValue nonce = "Digest realm=\"" ++ realmName ++ "\", qop=\"auth\", nonce=\"" ++ nonce ++ "\", opaque=\"\""
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen

