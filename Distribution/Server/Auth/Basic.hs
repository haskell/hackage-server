module Distribution.Server.Auth.Basic (
   hackageAuth,
  ) where

import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types
import qualified Distribution.Server.Auth.Crypt as Crypt
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group

import HAppS.Server
         ( ServerPartT(..), multi, withRequest, getHeader, noHandle, escape
         , unauthorized, addHeader, toResponse )
import qualified HAppS.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO)
import Control.Monad (guard)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

hackageAuth :: MonadIO m => Users.Users -> Group.UserGroup
            -> (UserId -> [ServerPartT m a])
            -> ServerPartT m a
hackageAuth users authorisedGroup = genericBasicAuth realm cryptPasswdCheck
  where
    realm = "hackage"
    cryptPasswdCheck user passwd = do
      userId <- Users.lookupName user users
      user   <- Users.lookupId userId users
      guard $ case userStatus user of
        Enabled hash -> Crypt.checkPasswd passwd hash
        _            -> False
      guard (Group.member userId authorisedGroup)
      return userId

-- This is directly ripped out of HAppS-Server and generalised
--
genericBasicAuth :: MonadIO m => String -> (UserName -> PasswdPlain -> Maybe a)
                 -> (a -> [ServerPartT m b]) -> ServerPartT m b
genericBasicAuth realmName validLogin xs = withRequest $ \req ->
  case getHeader "authorization" req of
    Just h -> case checkAuth h of
      Just ok -> unServerPartT (multi (xs ok)) req
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
