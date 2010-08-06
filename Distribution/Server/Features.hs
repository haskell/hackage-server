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
import Distribution.Server.Features.PreferredVersions (initVersionsFeature)
import Distribution.Server.Features.ReverseDependencies (initReverseFeature)
import Distribution.Server.Features.DownloadCount (initDownloadFeature)
import Distribution.Server.Features.Tags (initTagsFeature)
import Distribution.Server.Features.NameSearch (initNamesFeature)
import Distribution.Server.Features.PackageList (initListFeature)
--for a mirror, import Distribution.Server.Features.Mirror (initMirrorFeature)

-- This module ties together all the hackage features that we will use.
-- To add a feature:
-- 1. Import its initialization function
-- 2. Call its initialization function with all of its required arguments
-- 3. Add it to the allFeatures list

-- TODO:
-- * PackageServe: serving from tarballs (most of the work is setting it up on import)
-- * Snippet: code samples, pastebin for 'getting started' code
-- * LibraryRank: http://hackage.haskell.org/trac/hackage/ticket/183
-- * HaskellPlatform: mark off packages in the haskell platform.
-- * Anonymous build reports should work, as well as candidate build reports
-- * alter Users to be more in line with the current way registering is handled,
--     with email addresses available to maintainers, etc.
-- * UserNotify: email users and let them email each other
hackageFeatures :: Config -> IO [HackageModule]
hackageFeatures config = do
    coreFeature <- initCoreFeature config

    -- Arguments denote data dependencies, even if the feature objects are themselves unused,
    --   functions from their modules are.
    -- What follows is a topological sort along those lines
    usersFeature <- initUsersFeature config coreFeature
    uploadFeature <- initUploadFeature config coreFeature usersFeature
    packagesFeature <- initPackagesFeature config coreFeature
    distroFeature <- initDistroFeature config coreFeature usersFeature packagesFeature
    checkFeature <- initCheckFeature config coreFeature usersFeature packagesFeature uploadFeature
    reportsFeature <- initReportsFeature config coreFeature
    documentationFeature <- initDocumentationFeature config coreFeature uploadFeature
    downloadFeature <- initDownloadFeature config coreFeature
    tagsFeature <- initTagsFeature config coreFeature
    versionsFeature <- initVersionsFeature config coreFeature uploadFeature tagsFeature
    reverseFeature <- initReverseFeature config coreFeature versionsFeature
    namesFeature <- initNamesFeature config coreFeature
    listFeature <- initListFeature config coreFeature reverseFeature
                        downloadFeature tagsFeature versionsFeature
    --jsonFeature <- initJsonFeature
    htmlFeature <- initHtmlFeature config coreFeature packagesFeature
                        uploadFeature checkFeature usersFeature versionsFeature
                        reverseFeature tagsFeature downloadFeature listFeature
                        namesFeature
    -- The order of initialization above should be the same as
    -- the order of this list.
    let allFeatures =
         [ HF coreFeature
         , HF usersFeature
         , HF packagesFeature
         , HF uploadFeature
         , HF distroFeature
         , HF checkFeature
         , HF reportsFeature
         , HF documentationFeature
         , HF downloadFeature
         , HF tagsFeature
         , HF versionsFeature
         , HF reverseFeature
         , HF namesFeature
         , HF listFeature
         , HF htmlFeature
         , HF legacyRedirectsFeature
         ]
    -- Run all initial hooks, now that everyone's gotten a chance to register for them
    -- This solution does not work too well for special initial hook arguments
    sequence_ . concat $ map initHooks allFeatures
    return (map getFeature allFeatures)

data HF = forall a. HackageFeature a => HF a
instance HackageFeature HF where
    getFeature (HF a) = getFeature a
    initHooks  (HF a) = initHooks a

-- Still using Distribution.Server.State.HackageEntryPoint for the moment, it seems
-- otherwise, a HackageOverallState could be defined that works more closely with HackageFeature

