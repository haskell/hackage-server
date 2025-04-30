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
import Data.Text.Encoding (encodeUtf8)
import Happstack.Server (ServerMonad(askRq))
import Happstack.Server.Response (movedPermanently, toResponse)
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

    serverUserContentBaseURI :: URI.URI,
    -- | This might be an internal host name, used internally behind a load balancer
    serverRequiredBaseHostHeader :: String,

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

getHost :: ServerMonad m => m (Maybe BS.ByteString)
getHost = do
  rq <- askRq
  pure $
    case find ((== encodeUtf8 (T.pack "host")) . hName) $ rqHeaders rq of
      Just hostHeaderPair | [oneValue] <- hValue hostHeaderPair -> Just oneValue
      _ -> Nothing

requireUserContent :: ServerEnv -> Response -> ServerPartE Response
requireUserContent ServerEnv {serverUserContentBaseURI, serverRequiredBaseHostHeader} action = do
  Just hostHeaderValue <- getHost
  rq <- askRq
  if hostHeaderValue == encodeUtf8 (T.pack serverRequiredBaseHostHeader)
     then
      let
        uri = serverUserContentBaseURI
          { URI.uriPath = rqUri rq
          , URI.uriQuery = rqQuery rq
          }
      in
       movedPermanently (show uri) (toResponse ())
     else
       pure action
