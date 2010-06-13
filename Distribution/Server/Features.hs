{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, TypeOperators #-}
module Distribution.Server.Features where

--TODO: decrease the code/comments ratio
import Distribution.Server.Feature
import Distribution.Server.Features.Core (initCoreFeature)
--import Distribution.Server.Features.Json (initJsonFeature)
--import Distribution.Server.Features.Html (initHtmlFeature)
--import Distribution.Server.Features.Check (initCheckFeature)
--import Distribution.Server.Features.Upload (initUploadFeature)
import Distribution.Server.Features.Packages (initPackagesFeature)
import Distribution.Server.Features.Users (initUsersFeature)
--import Distribution.Server.Features.Mirror (initMirrorFeature)
import Distribution.Server.Features.LegacyRedirects (legacyRedirectsFeature)
--import Distribution.Server.Users.State (UsersStore)

--import Happstack.State
--import Data.Typeable

-- This module ties together all the hackage features that we will use.

hackageFeatures :: IO [HackageModule]
hackageFeatures = do
    -- > these can get along by themselves
    coreFeature <- initCoreFeature
    --mirrorFeature <- initMirrorFeature coreFeature

    -- > and for richer content...
    usersFeature <- initUsersFeature coreFeature
    --uploadFeature <- initUploadFeature coreFeature
    packagesFeature <- initPackagesFeature coreFeature
    --checkFeature <- initCheckFeature coreFeature uploadFeature
    --htmlFeature <- initHtmlFeature packagesFeature usersFeature uploadFeature checkFeature
    --jsonFeature <- initJsonFeature packagesFeature usersFeature uploadFeature checkFeature
    return [ getFeature coreFeature, getFeature usersFeature, legacyRedirectsFeature ]

-- Still using Distribution.Server.State.HackageEntryPoint for the moment, it seems
{-
-- For the sake of the happstack state system we need to give the list
-- of all data components used by each hackage feature.
data HackageOverallState = HackageOverallState
  deriving Typeable

instance Component HackageOverallState where
    type Dependencies HackageOverallState = End -- :+:
    initialValue = HackageOverallState

instance Version HackageOverallState
instance Serialize HackageOverallState where
    putCopy HackageOverallState = contain (return ())
    getCopy = contain (return HackageOverallState)

$(mkMethods ''HackageOverallState [])
-}
