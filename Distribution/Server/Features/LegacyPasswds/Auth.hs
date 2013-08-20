{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TemplateHaskell, ForeignFunctionInterface, PatternGuards #-}
module Distribution.Server.Features.LegacyPasswds.Auth (
   HtPasswdHash(..),
   guardAuthenticated,
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.Error
import Distribution.Server.Framework.MemSize
import Distribution.Server.Users.Types (UserId, UserName(..), UserInfo)
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Framework.AuthCrypt (BasicAuthInfo(..))

import Happstack.Server

import qualified Data.ByteString.Base64 as Base64
import Control.Monad
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)

import qualified Data.ByteString.Char8 as BS -- TODO: Verify that we don't need to worry about UTF8 here

---------------------------
-- Old-style crypt() auth
--

-- compatible with apache htpasswd files using CRYPT format:
--
-- http://httpd.apache.org/docs/2.2/misc/password_encryptions.html
--

-- | These are the *old* crypt format password hashes (salted DES: perl crypt).
-- Not the same as the new hashes we store in 'PasswdHash'.
--
newtype HtPasswdHash = HtPasswdHash String
  deriving (Eq, Show, Typeable, MemSize)

$(deriveSafeCopy 0 'base ''HtPasswdHash)

checkCryptAuthInfo :: HtPasswdHash -> BasicAuthInfo -> Bool
checkCryptAuthInfo (HtPasswdHash hash) (BasicAuthInfo _ _ (PasswdPlain passwd))
  = crypt passwd hash == hash

foreign import ccall unsafe "crypt" cCrypt :: CString-> CString -> CString

crypt :: String -- ^ Payload
      -> String -- ^ Salt
      -> String -- ^ Hash
crypt key seed = unsafePerformIO $ withMVar cryptMVar $ \_ -> do
    k <- newCAString key
    s <- newCAString seed
    peekCAString $ cCrypt k s

cryptMVar :: MVar ()
cryptMVar = unsafePerformIO $ newMVar ()
{-# NOINLINE cryptMVar #-}

--------------------
-- HTTP Basic auth
--

guardAuthenticated :: RealmName -> Users.Users
                   -> (UserId -> Maybe HtPasswdHash)
                   -> ServerPartE (UserId, UserInfo, PasswdPlain)
guardAuthenticated realm users getHtPasswdHash = do
    req <- askRq
    either (authError realm) return $
      case getHeaderAuth req of
        Just ahdr -> checkBasicAuth users getHtPasswdHash realm ahdr
        Nothing   -> Left NoAuthError
  where
    getHeaderAuth :: Request -> Maybe BS.ByteString
    getHeaderAuth req
      | Just hdr <- getHeader "authorization" req
      , BS.isPrefixOf (BS.pack "Basic ") hdr
      = Just (BS.drop 6 hdr)

      | otherwise
      = Nothing

checkBasicAuth :: Users.Users -> (UserId -> Maybe HtPasswdHash) -> RealmName -> BS.ByteString
               -> Either AuthError (UserId, UserInfo, PasswdPlain)
checkBasicAuth users getHtPasswdHash realm ahdr = do
    authInfo     <- getBasicAuthInfo realm ahdr       ?! UnrecognizedAuthError
    let uname    = basicUsername authInfo
    (uid, uinfo) <- Users.lookupUserName uname users  ?! NoSuchUserError
    passwdhash   <- getHtPasswdHash uid               ?! NoSuchUserError
    guard (checkCryptAuthInfo passwdhash authInfo)    ?! PasswordMismatchError
    return (uid, uinfo, basicPasswd authInfo)

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
                          _                      -> Nothing

setBasicAuthChallenge :: RealmName -> ServerPartE ()
setBasicAuthChallenge (RealmName realmName) = do
    addHeaderM headerName headerValue
  where
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""

authError :: RealmName -> AuthError -> ServerPartE a
authError realm err = do
  setBasicAuthChallenge  realm
  finishWith (showAuthError err)

data AuthError = NoAuthError | UnrecognizedAuthError | NoSuchUserError
               | PasswordMismatchError
  deriving Show

showAuthError :: AuthError -> Response
showAuthError err = case err of
    NoAuthError           -> withRsCode 401 $ toResponse "No authorization provided."
    UnrecognizedAuthError -> withRsCode 400 $ toResponse "Authorization scheme not recognized."
    NoSuchUserError       -> withRsCode 401 $ toResponse "Username or password incorrect."
    PasswordMismatchError -> withRsCode 401 $ toResponse "Username or password incorrect."
  where
    withRsCode c r = r { rsCode = c }

