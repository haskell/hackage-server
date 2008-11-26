module Distribution.Server.Auth.Basic (
   hackageAuth,
  ) where

import Distribution.Server.Users.Types
         ( UserId, UserName(..), PasswdPlain(..) )
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Auth.Crypt as Crypt

import HAppS.Server
         ( ServerPartT(..), withRequest, getHeader, escape
         , unauthorized, addHeader, toResponse )
import qualified HAppS.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as BS

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

-- This is directly ripped out of HAppS-Server and generalised
--
genericBasicAuth :: Monad m => String -> (UserName -> PasswdPlain -> Maybe a)
                 -> ServerPartT m a
genericBasicAuth realmName validLogin = withRequest $ \req ->
  case getHeader "authorization" req of
    Just h -> case checkAuth h of
      Just ok -> return ok -- unServerPartT (multi (xs ok)) req
      Nothing -> err
    _         -> err
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
