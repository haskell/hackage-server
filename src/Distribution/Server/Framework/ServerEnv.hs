module Distribution.Server.Framework.ServerEnv where

import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Framework.Logging (Verbosity)
import Distribution.Server.Framework.Cron (Cron)
import Distribution.Server.Framework.Templating (TemplatesMode)

import qualified Network.URI as URI

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
