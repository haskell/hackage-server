{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, TypeOperators, ExistentialQuantification #-}
module Distribution.Server.Features where

import Distribution.Server.Feature
import Distribution.Server.Types (Config)
import Distribution.Server.Features.Core (initCoreFeature)
--import Distribution.Server.Features.Json (initJsonFeature)
import Distribution.Server.Features.Html (initHtmlFeature)
import Distribution.Server.Features.Check (initCheckFeature)
import Distribution.Server.Features.Upload (initUploadFeature)
import Distribution.Server.Features.Packages (initPackagesFeature)
import Distribution.Server.Features.Users (initUsersFeature)
import Distribution.Server.Features.Distro (initDistroFeature)
import Distribution.Server.Features.Documentation (initDocumentationFeature)
import Distribution.Server.Features.Reports (initReportsFeature)
import Distribution.Server.Features.LegacyRedirects (legacyRedirectsFeature)
--for a mirror, import Distribution.Server.Features.Mirror (initMirrorFeature)

-- This module ties together all the hackage features that we will use.

-- TODO: DownloadCount, PreferredVersions, ReverseDependencies
hackageFeatures :: Config -> IO [HackageModule]
hackageFeatures config = do
    -- > these can get along by themselves
    coreFeature <- initCoreFeature config
--    mirrorFeature <- initMirrorFeature coreFeature

    -- > and additional content...
    -- arguments denote data dependencies: even if the feature objects are themselves unused
    -- what follows is a topological sort along those lines
    usersFeature <- initUsersFeature config coreFeature
    uploadFeature <- initUploadFeature config coreFeature
    packagesFeature <- initPackagesFeature config coreFeature
    distroFeature <- initDistroFeature config coreFeature packagesFeature
    checkFeature <- initCheckFeature config coreFeature packagesFeature uploadFeature
    reportsFeature <- initReportsFeature config coreFeature
    documentationFeature <- initDocumentationFeature config coreFeature uploadFeature
    htmlFeature <- initHtmlFeature config coreFeature packagesFeature uploadFeature
                                   checkFeature usersFeature
    --jsonFeature <- initJsonFeature
    let allFeatures =
         [ HF coreFeature, HF usersFeature, HF packagesFeature, HF uploadFeature
         , HF distroFeature, HF checkFeature, HF reportsFeature
         , HF legacyRedirectsFeature, HF documentationFeature
         , HF htmlFeature
         ]
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

