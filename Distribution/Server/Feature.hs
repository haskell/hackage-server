module Distribution.Server.Feature where

import Distribution.Server.Backup.Import (RestoreBackup, BackupEntry)
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Resource

-- This module defines a plugin interface for hackage features.
--
-- We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.
data HackageModule = HackageModule {
    featureName   :: String,
    resources     :: [Resource],
    dumpBackup    :: Maybe (BlobStorage -> IO [BackupEntry]),
    restoreBackup :: Maybe (BlobStorage -> RestoreBackup)
}

-- A type belonging to the HackageFeature class is a data structure from which
-- a HackageModule can be created. These data structures are initialized manually
-- in Features.hs, possibly depending on each other to register for hooks, construct
-- URIs, and (maybe in the future) retrieve or encapsulate TxControl handles.
--
-- With this class, they are combined into homogenous list of HackageModules
-- used by the Server data structure.
class HackageFeature a where
    getFeature :: a -> HackageModule
    initHooks  :: a -> [IO ()]
    initHooks _ = []

-- degenerate
instance HackageFeature HackageModule where
    getFeature = id

