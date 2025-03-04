{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Framework.ServerEnv where

import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Framework.Logging (Verbosity)
import Distribution.Server.Framework.Cron (Cron)
import Distribution.Server.Framework.Templating (TemplatesMode)
import Distribution.Server.Framework.Error (ServerPartE)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.URI as URI
import Data.List (find)
import Data.Text.Encoding (decodeASCII', encodeUtf8)
import Happstack.Server (ServerMonad(askRq))
import Happstack.Server.Response (seeOther, toResponse)
import Happstack.Server.Types (HeaderPair(..), Response, rqHeaders, rqQuery, rqUri)

import qualified Hackage.Security.Util.Path as Sec

-- | The internal server environment as used by 'HackageFeature's.
--
-- It contains various bits of static information (and handles of
-- server-global objects) that are needed by the implementations of
-- some 'HackageFeature's.
--
data ServerEnv = ServerEnv {

    -- | The location of the server's static files
    serverStaticDir :: FilePath,

    -- | The location of the server's template files
    serverTemplatesDir :: FilePath,

    -- | The location of TUF data (signed root info, private keys)
    serverTUFDir :: Sec.Path Sec.Absolute,

    -- | Default templates mode
    serverTemplatesMode :: TemplatesMode,

    -- | The location of the server's state directory. This is where the
    -- server's persistent state is kept, e.g. using ACID state.
    serverStateDir  :: FilePath,

    -- | The blob store is a specialised provider of persistent state for
    -- larger relatively-static blobs of data (e.g. uploaded tarballs).
    serverBlobStore :: BlobStorage,

    -- | A cron job service
    serverCron :: Cron,

    -- | The temporary directory the server has been configured to use.
    -- Use it for temp files such as when validating uploads.
    serverTmpDir    :: FilePath,

    -- | The base URI of the server, just the hostname (and perhaps port).
    -- Use this if you need to construct absolute URIs pointing to the
    -- current server (e.g. as required in RSS feeds).
    serverBaseURI   :: URI.URI,

    serverUserContentHost :: String,

    -- | A tunable parameter for cache policy. Setting this parameter high
    -- during bulk imports can very significantly improve performance. During
    -- normal operation it probably doesn't help much.

    -- By delaying cache updates we can sometimes save some effort: caches are
    -- based on a bit of changing state and if that state is updated more
    -- frequently than the time taken to update the cache, then we don't have
    -- to do as many cache updates as we do state updates. By artificially
    -- increasing the time taken to update the cache we can push this further.
    serverCacheDelay :: Int,

    serverVerbosity  :: Verbosity
}

getHostAndBaseAuth :: ServerMonad m => ServerEnv -> m (Maybe (BS.ByteString, URI.URIAuth))
getHostAndBaseAuth ServerEnv {serverBaseURI} = do
  rq <- askRq
  let
    mbHost :: Maybe BS.ByteString
    mbHost =
      case find ((== encodeUtf8 (T.pack "host")) . hName) $ rqHeaders rq of
        Just hostHeaderPair | [oneValue] <- hValue hostHeaderPair ->
          -- If there is a colon in the host header, remove it.
          -- We require the regName of user-content and base to differ.
          -- 58 is ASCII for colon
          let (beforeColon, _colonAndAfter) = BS.break (== 58) oneValue
           in Just beforeColon
        _ -> Nothing
    mbBaseAuth = URI.uriAuthority serverBaseURI
  case (,) <$> mbHost <*> mbBaseAuth of
    Nothing -> pure Nothing
    Just (hostHeaderValue, baseAuth) -> pure $ Just (hostHeaderValue, baseAuth)

requireUserContent :: ServerEnv -> Response -> ServerPartE Response
requireUserContent env@ServerEnv {serverBaseURI, serverUserContentHost} action = do
  Just (hostHeaderValue, baseAuth) <- getHostAndBaseAuth env
  rq <- askRq
  if hostHeaderValue == encodeUtf8 (T.pack $ URI.uriRegName baseAuth)
     then
      let
        uri = URI.URI
          { URI.uriScheme = URI.uriScheme serverBaseURI
          , URI.uriAuthority = Just URI.URIAuth { URI.uriUserInfo = "", URI.uriRegName = serverUserContentHost, URI.uriPort = "" }
          , URI.uriPath = rqUri rq
          , URI.uriQuery = rqQuery rq
          , URI.uriFragment = ""
          }
      in
       seeOther (show uri) (toResponse ())
     else
       if hostHeaderValue /= encodeUtf8 (T.pack serverUserContentHost)
          then
            fail $
              "Host name (" <> maybe "N/A" T.unpack (decodeASCII' hostHeaderValue) <> ")" <>
              " matched neither user content host (" <> serverUserContentHost <> ")" <>
              " nor base host (" <> URI.uriRegName baseAuth <> ")"
          else pure action

-- | Returns Just when the host isn't matching the regular host
isRegularHost :: ServerMonad m => ServerEnv -> m (Maybe (BS.ByteString, String))
isRegularHost env = do
  mbPair <- getHostAndBaseAuth env
  case mbPair of
    Just (hostHeaderValue, baseAuth) | hostHeaderValue /= encodeUtf8 (T.pack $ URI.uriRegName baseAuth) ->
      pure $ Just (hostHeaderValue, URI.uriRegName baseAuth)
    _ -> pure Nothing
