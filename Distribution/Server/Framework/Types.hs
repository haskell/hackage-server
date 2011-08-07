module Distribution.Server.Framework.Types where

import Distribution.Server.Framework.BlobStorage (BlobStorage)

import Happstack.Server
import qualified Network.URI as URI

-- | The internal server environment as used by 'HackageFeature's.
--
-- It contains various bits of static information (and handles of
-- server-global objects) that are needed by the implementations of
-- some 'HackageFeature's.
--
data ServerEnv = ServerEnv {

    -- | The location of the server's static files
    serverStaticDir :: FilePath,

    -- | The location of the server's state directory. This is where the
    -- server's persistent state is kept, e.g. using ACID state.
    serverStateDir  :: FilePath,

    -- | The blob store is a specialised provider of persistent state for
    -- larger relatively-static blobs of data (e.g. uploaded tarballs).
    serverBlobStore :: BlobStorage,

    -- | The temporary directory the server has been configured to use.
    -- Use it for temp files such as when validating uploads.
    serverTmpDir    :: FilePath,

    -- | The base URI of the server, just the hostname (and perhaps port).
    -- Use this if you need to construct absolute URIs pointing to the
    -- current server (e.g. as required in RSS feeds).
    serverHostURI   :: URI.URIAuth
}

type DynamicPath = [(String, String)]

type ServerResponse = DynamicPath -> ServerPart Response

