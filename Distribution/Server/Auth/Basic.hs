module Distribution.Server.Auth.Basic (
   basicAuth,
   plainPasswdCheck,
  ) where

import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types
import qualified Distribution.Server.Auth.Crypt as Crypt

import HAppS.Server
         ( ServerPartT, multi, withRequest, getHeader, noHandle, escape
         , unauthorized, addHeader, toResponse )
import qualified HAppS.Crypto.Base64 as Base64

import Control.Monad.Trans (MonadIO)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

-- This is directly ripped out of HAppS-Server and generalised
--
basicAuth :: MonadIO m => String -> (UserName -> PasswdPlain -> Bool)
          -> [ServerPartT m a] -> ServerPartT m a
basicAuth realmName validLogin xs = multi $ basicAuthImpl:xs
  where
    basicAuthImpl = withRequest $ \rq ->
      case getHeader "authorization" rq of
        Just h | checkAuth h -> noHandle
        _                    -> err
    checkAuth h = case parseHeader h of
      (name, ':':pass) -> validLogin (UserName name) (PasswdPlain pass)
      _                -> False
    parseHeader = break (':'==) . Base64.decode . BS.unpack . BS.drop 6
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err = escape $ unauthorized $
            addHeader headerName headerValue $ toResponse "Not authorized"

plainPasswdCheck :: Map.Map UserName PasswdPlain
                 -> UserName -> PasswdPlain -> Bool
plainPasswdCheck authMap name (PasswdPlain pass) =
  Map.lookup name authMap == Just (PasswdPlain pass)
