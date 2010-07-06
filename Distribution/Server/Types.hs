module Distribution.Server.Types where

import Distribution.Server.Util.BlobStorage (BlobStorage)

import Happstack.Server
import qualified Network.URI as URI

data Config = Config {
    serverStore     :: BlobStorage,
    serverStaticDir :: FilePath,
    serverURI       :: URI.URIAuth
}

type DynamicPath = [(String, String)]

type ServerResponse = DynamicPath -> ServerPart Response
