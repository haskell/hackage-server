module Distribution.Server.Feature where

import Distribution.Server.Util.BlobStorage (BlobStorage)
import Happstack.Server
import qualified Network.URI as URI

-- This module defines a plugin interface for hackage features.
--
-- We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.

data HackageFeature = HackageFeature {
    featureName    :: String,

    serverPart     :: Config -> ServerPart Response,

    dumpBackup     :: IO [BackupEntry],
    restoreBackup  :: [BackupEntry] -> IO ()
}

-- TODO: move this to a backup dump/restore module
-- filesystem name + human readable content
type BackupEntry = ([FilePath], {-Byte-}String)

data Config = Config {
  serverStore      :: BlobStorage,
  serverStaticDir  :: FilePath,
  serverURI        :: URI.URIAuth
}
