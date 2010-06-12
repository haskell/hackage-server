module Distribution.Server.Core where

--import Distribution.Server.Packages.PackageBackup
import Distribution.Server.Users.Resource (makeGroupResources)
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Cache as Cache
import Distribution.Server.Users.UserBackup
import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Hook (HookList, newHookList)
import Distribution.Server.Import (BackupEntry)
import Text.CSV (printCSV, CSV)

coreFeature :: HackageFeature 
coreFeature = HackageFeature {
    featureName = "core",
    locations   = [],
    dumpBackup    = return [],
    restoreBackup = Just userBackup
}

