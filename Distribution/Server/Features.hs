{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, TypeOperators, ExistentialQuantification #-}
module Distribution.Server.Features where

--TODO: decrease the code/comments ratio
import Distribution.Server.Feature
import Distribution.Server.Features.Core (initCoreFeature)
--import Distribution.Server.Features.Json (initJsonFeature)
import Distribution.Server.Features.Html (initHtmlFeature)
--import Distribution.Server.Features.Check (initCheckFeature)
import Distribution.Server.Features.Upload (initUploadFeature)
import Distribution.Server.Features.Packages (initPackagesFeature)
import Distribution.Server.Features.Users (initUsersFeature)
import Distribution.Server.Features.Distro (initDistroFeature)
import Distribution.Server.Features.Mirror (initMirrorFeature)
import Distribution.Server.Features.LegacyRedirects (legacyRedirectsFeature)

-- This module ties together all the hackage features that we will use.

hackageFeatures :: IO [HackageModule]
hackageFeatures = do
    -- > these can get along by themselves
    coreFeature <- initCoreFeature
--    mirrorFeature <- initMirrorFeature undefined --coreFeature

    -- > and for richer content...
    usersFeature <- initUsersFeature coreFeature
    uploadFeature <- initUploadFeature coreFeature
    packagesFeature <- initPackagesFeature coreFeature
    distroFeature <- initDistroFeature coreFeature packagesFeature
    --checkFeature <- initCheckFeature coreFeature uploadFeature
    htmlFeature <- initHtmlFeature coreFeature packagesFeature --usersFeature uploadFeature checkFeature
    --jsonFeature <- initJsonFeature
    let allFeatures = [HF coreFeature, HF usersFeature, HF packagesFeature, HF uploadFeature, HF distroFeature, HF legacyRedirectsFeature, HF htmlFeature]
--    let allFeatures = [HF mirrorFeature]
    -- Run all initial hooks, now that everyone's gotten a chance to register for them
    -- This solution does not work too well for special initial hook arguments
    sequence . concat $ map initHooks allFeatures
    return (map getFeature allFeatures)

data HF = forall a. HackageFeature a => HF a
instance HackageFeature HF where
    getFeature (HF a) = getFeature a
    initHooks  (HF a) = initHooks a

-- Still using Distribution.Server.State.HackageEntryPoint for the moment, it seems
-- otherwise, a HackageOverallState could be defined that works more closely with HackageFeature

