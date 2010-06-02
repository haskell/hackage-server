module Distribution.Server.Feature where

import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Resource
import Happstack.Server
import qualified Network.URI as URI

-- This module defines a plugin interface for hackage features.
--
-- We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.

data HackageFeature = HackageFeature {
    featureName    :: String,
    resources      :: [Resource],
    serverParts    :: [(BranchPath, ServerPart Response)],
    dumpBackup     :: IO [BackupEntry],
    restoreBackup  :: [BackupEntry] -> IO ()
}
addFeatureResource :: Resource -> HackageFeature -> HackageFeature
addFeatureResource resource feature = feature { resources = resource:(resources feature) }

addStaticURIPart :: [String] -> ServerPart Response -> HackageFeature -> HackageFeature
addStaticURIPart = addDynamicURIPart . map StaticBranch

addDynamicURIPart :: BranchPath -> ServerPart Response -> HackageFeature -> HackageFeature
addDynamicURIPart bpath response feature = feature { serverParts = (bpath, response):(serverParts feature) }

-- TODO: move this to a backup dump/restore module
-- filesystem name + human readable content
type BackupEntry = ([FilePath], {-Byte-}String)

data Config = Config {
  serverStore      :: BlobStorage,
  serverStaticDir  :: FilePath,
  serverURI        :: URI.URIAuth
}
--instance Eq SomeResource where (==) (SomeResource r1) (SomeResource r2) = typeRep r1 == typeRep r2
