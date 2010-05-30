module Distribution.Server.Auth.Basic (
   hackageAuth,
   hackageDigestAuth
  ) where

import Distribution.Server.Users.Types
         ( UserId, UserName(..), PasswdPlain(..) )
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Auth.Crypt as Crypt

import Happstack.Server
--         ( ServerPartT(..), withRequest, getHeader, escape
--         , unauthorized, addHeader, toResponse )
import qualified Happstack.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

import Control.Monad (mzero, join, liftM2)
import Data.Char (intToDigit, isAsciiLower)
import System.Random (randomRs, newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe (maybe)
import Data.List (find, intercalate)
import Data.Digest.Pure.MD5 (md5)
import Debug.Trace

hackageAuth :: MonadIO m => Users.Users -> Maybe Group.UserGroup
            -> ServerPartT m UserId
hackageAuth users authorisedGroup = genericBasicAuth realm cryptPasswdCheck
  where
    realm = "hackage"
    cryptPasswdCheck userName passwd = do
      userId   <- Users.lookupName userName users
      userInfo <- Users.lookupId userId users
      guard $ case Users.userStatus userInfo of
        Users.Enabled hash -> Crypt.checkPasswd passwd hash
        _                  -> False
      guard (maybe True (Group.member userId) authorisedGroup)
      return userId

-- This is directly ripped out of Happstack-Server and generalised
--
genericBasicAuth :: Monad m => String -> (UserName -> PasswdPlain -> Maybe a)
                 -> ServerPartT m a
genericBasicAuth realmName validLogin = withRequest $ \req ->
  case getHeader "authorization" req of
    Just h -> case checkAuth h of
      Just res -> return res -- unServerPartT (multi (xs ok)) req
      Nothing  -> err
    _          -> err
  where
    checkAuth h = do
      (user, pass) <- parseHeader h
      validLogin user pass

    parseHeader h = case splitHeader h of
      (name, ':':pass) -> Just (UserName name, PasswdPlain pass)
      _                -> Nothing
    splitHeader = break (':'==) . Base64.decode . BS.unpack . BS.drop 6

    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err = escape $ unauthorized $
            addHeader headerName headerValue $ toResponse "Not authorized"

-- This is the digest version of the above code, utilizing the UserInfo map as
-- though it contained MD5 hashes of the form username:hackage:password.
--
-- To use both systems at the same time, determine at the start of a client
-- "session" whether to request Basic or Digest authentication, and then
-- demultiplex when the client sends the Authorization header.
hackageDigestAuth :: MonadIO m => Users.Users -> Maybe Group.UserGroup -> ServerPartT m UserId
hackageDigestAuth users authorizedGroup = genericDigestAuth realm digestPasswdCheck
  where
    realm = "hackage"
    digestPasswdCheck :: Request -> Map String String -> Maybe UserId
    digestPasswdCheck req authMap = do
        userName <- Map.lookup "username" authMap
        userId   <- Users.lookupName (UserName userName) users
        guard (maybe True (Group.member userId) authorizedGroup)
        userInfo <- Users.lookupId userId users
        --split this off into a separate module that hides the MD5 business
        case Users.userStatus userInfo of
          Users.Enabled (Users.PasswdHash hash1) -> do
            nonce <- Map.lookup "nonce" authMap
            response <- Map.lookup "response" authMap
            uri <- Map.lookup "uri" authMap
            let qop = Map.lookup "qop" authMap
                nonceCount = Map.lookup "nc" authMap
                cnonce = Map.lookup "cnonce" authMap
                hash2 = show . md5 . BS.Lazy.pack . join traceShow $ intercalate ":" [show (rqMethod req), uri] ++ case qop of
                    Just "auth-int" -> (':':) . show . md5 $ case rqBody req of Body body -> body
                    _ -> ""
                responseString = show . md5 . BS.Lazy.pack . join traceShow . intercalate ":" $ case (qop, nonceCount, cnonce) of
                    (Just qop', Just nonceCount', Just cnonce')
                        | qop' == "auth" || qop' == "auth-int" -> [hash1, nonce, nonceCount', cnonce', qop', hash2]
                    _ -> [hash1, nonce, hash2]
            guard (responseString == response)
            return userId
          _ -> mzero

genericDigestAuth :: MonadIO m => String -> (Request -> Map String String -> Maybe a) -> ServerPartT m a
genericDigestAuth realmName validLogin = askRq >>= \req -> do
    let authMap = case getHeader "authorization" req of
                      Nothing -> Map.empty
                      Just h -> parseDigestResponse . BS.unpack $ h
    case validLogin req authMap of
        Nothing -> setDigestChallenge realmName
        Just res -> return res

-- Parser derived straight from RFCs 2616 and 2617
parseDigestResponse :: String -> Map String String
parseDigestResponse = maybe Map.empty (Map.fromList . fst) . find (null . snd) .
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

setDigestChallenge :: MonadIO m => String -> ServerPartT m a
setDigestChallenge realmName = do
    nonce <- liftIO generateNonce
    escape $ unauthorized $ addHeader headerName (headerValue nonce) $ toResponse "Not authorized under digest"
  where
    headerName = "WWW-Authenticate"
    -- I would use qop=\"auth,auth-int\", but Google Chrome seems to have problems choosing one...
    headerValue nonce = "Digest realm=\"" ++ realmName ++ "\", qop=\"auth\", nonce=\"" ++ nonce ++ "\", opaque=\"\""
    generateNonce = fmap (take 32 . map intToDigit . randomRs (0, 15)) newStdGen

