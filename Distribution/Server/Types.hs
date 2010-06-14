module Distribution.Server.Types where

import Distribution.Server.Util.BlobStorage (BlobStorage)
import qualified Distribution.Server.Cache as Cache

import Happstack.Server
import qualified Network.URI as URI
import qualified Distribution.Server.Cache as Cache

data Config = Config {
    serverStore     :: BlobStorage,
    serverStaticDir :: FilePath,
    serverURI       :: URI.URIAuth,
    serverCache     :: Cache.Cache
}

type DynamicPath = [(String, String)]

type ServerResponse = Config -> DynamicPath -> ServerPart Response
