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
--for a mirror, import Distribution.Server.Features.Mirror (initMirrorFeature)

-- This module ties together all the hackage features that we will use.

-- TODO, soon or later.
-- * PackageServe: serving from tarballs (most of the work is setting it up on import)
-- * Snippet: code samples, pastebin for 'getting started' code
-- * NameSearch: searching just by name (possibly sorted by hotness), useful
--     for suggest in textfields and in-browser OpenSearch
-- * LibraryRank: http://hackage.haskell.org/trac/hackage/ticket/183
-- * HaskellPlatform: mark off packages in the haskell platform.
-- * ActionLog: or some way to log actions by user, date, and target(s), which
--     is easily searchable. If this is not built directly into the features
--     themselves, there could be some kind of way to hook onto them and expose
--     useful activity information. This would be a good home for recent.rss.
-- * Anonymous build reports should work, as well as candidate build reports
-- * fix ReverseDependencies to deal with uploads
-- * alter Users to be more in line with the current way registering is handled,
--     with email addresses available to maintainers, etc.
-- * find a better interface for UserGroups (again), that effectively combine
--     such as aspects as IO functions for modification, resources which list
--     group members, serving information about groups, and keeping track of
--     which groups a given user belongs to
-- * backups, backups, backups
-- * use more <$> and less fmap. (admittedly minor)

hackageFeatures :: Config -> IO [HackageModule]
hackageFeatures config = do
    -- > these can get along by themselves
    coreFeature <- initCoreFeature config

    -- > and additional content...
    -- Arguments denote data dependencies, even if the feature objects are themselves unused,
    --   functions from their modules are.
    -- What follows is a topological sort along those lines
    usersFeature <- initUsersFeature config coreFeature
    uploadFeature <- initUploadFeature config coreFeature usersFeature
    packagesFeature <- initPackagesFeature config coreFeature
    distroFeature <- initDistroFeature config coreFeature usersFeature packagesFeature
    checkFeature <- initCheckFeature config coreFeature packagesFeature uploadFeature
    reportsFeature <- initReportsFeature config coreFeature
    versionsFeature <- initVersionsFeature config coreFeature uploadFeature
    reverseFeature <- initReverseFeature config coreFeature versionsFeature
    documentationFeature <- initDocumentationFeature config coreFeature uploadFeature
    downloadFeature <- initDownloadFeature config coreFeature
    tagsFeature <- initTagsFeature config coreFeature
    --jsonFeature <- initJsonFeature
    htmlFeature <- initHtmlFeature config coreFeature packagesFeature
                        uploadFeature checkFeature usersFeature versionsFeature
                        reverseFeature
    let allFeatures =
         [ HF coreFeature
         , HF usersFeature
         , HF packagesFeature
         , HF uploadFeature
         , HF distroFeature
         , HF checkFeature
         , HF reportsFeature
         , HF versionsFeature
         , HF reverseFeature
         , HF documentationFeature
         , HF downloadFeature
         , HF tagsFeature
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

