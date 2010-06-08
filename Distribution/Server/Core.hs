module Distribution.Server.Core where

import Distribution.Server.Users.UserBackup
import Distribution.Server.Feature

coreFeature :: HackageFeature 
coreFeature = HackageFeature {
    featureName = "core",
    locations   = [],
    dumpBackup    = return [],
    restoreBackup = Just userBackup
}

