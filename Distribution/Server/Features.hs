-- | This module ties together all the hackage features that we will use.
--
-- To add a feature:
--
-- * Import its initialization function
-- * Call its initialization function with all of its required arguments
-- * Add it to the allFeatures list
--
{-# LANGUAGE CPP #-}
module Distribution.Server.Features where

import Distribution.Server.Framework.Feature
import Distribution.Server.Framework.Types   (ServerEnv)
import Distribution.Server.Features.Core     (initCoreFeature)
import Distribution.Server.Features.Users    (initUsersFeature)
import Distribution.Server.Features.Upload   (initUploadFeature)
import Distribution.Server.Features.Mirror   (initMirrorFeature)

#ifndef MINIMAL
import Distribution.Server.Features.Html     (initHtmlFeature)
import Distribution.Server.Features.Check    (initCheckFeature)
import Distribution.Server.Features.Packages (initPackagesFeature)
import Distribution.Server.Features.Distro   (initDistroFeature)
import Distribution.Server.Features.PackageContents     (initPackageContentsFeature)
import Distribution.Server.Features.Documentation       (initDocumentationFeature)
import Distribution.Server.Features.BuildReports        (initBuildReportsFeature)
import Distribution.Server.Features.LegacyRedirects     (legacyRedirectsFeature)
import Distribution.Server.Features.PreferredVersions   (initVersionsFeature)
import Distribution.Server.Features.ReverseDependencies (initReverseFeature)
import Distribution.Server.Features.DownloadCount       (initDownloadFeature)
import Distribution.Server.Features.Tags            (initTagsFeature)
import Distribution.Server.Features.NameSearch      (initNamesFeature)
import Distribution.Server.Features.PackageList     (initListFeature)
import Distribution.Server.Features.HaskellPlatform (initPlatformFeature)
#endif

-- TODO:
-- * PackageServe: serving from tarballs (most of the work is setting it up on import)
-- * Snippet: code samples, pastebin for 'getting started' code
-- * LibraryRank: http://hackage.haskell.org/trac/hackage/ticket/183
-- * HaskellPlatform: mark off packages in the haskell platform.
-- * Anonymous build reports should work, as well as candidate build reports
-- * alter Users to be more in line with the current way registering is handled,
--     with email addresses available to maintainers, etc.
-- * UserNotify: email users and let them email each other
-- * Backup: would need a [HackageFeature] to backup, though a HackageFeature itself.
--     best approach is probably to write backup tarball to disk and transfer
--     it away through non-HTTP means (somewhat more secure)

initHackageFeatures :: ServerEnv -> IO [HackageFeature]
initHackageFeatures env = do

    -- Arguments denote data dependencies, even if the feature objects are
    -- themselves unused, functions from their modules are.
    -- What follows is a topological sort along those lines

    coreFeature     <- initCoreFeature env

    usersFeature    <- initUsersFeature env
                         coreFeature

    mirrorFeature   <- initMirrorFeature env
                         coreFeature
                         usersFeature

    uploadFeature   <- initUploadFeature env
                         coreFeature
                         usersFeature

#ifndef MINIMAL
    packagesFeature <- initPackagesFeature env
                         coreFeature

    distroFeature   <- initDistroFeature env
                         coreFeature
                         usersFeature
                         packagesFeature

    checkFeature    <- initCheckFeature env
                         coreFeature
                         usersFeature
                         packagesFeature
                         uploadFeature

    reportsFeature  <- initBuildReportsFeature env
                         coreFeature

    packageContentsFeature <- initPackageContentsFeature env
                                coreFeature
                                checkFeature

    documentationFeature <- initDocumentationFeature env
                         coreFeature
                         uploadFeature

    downloadFeature <- initDownloadFeature env
                         coreFeature

    tagsFeature     <- initTagsFeature env
                         coreFeature

    versionsFeature <- initVersionsFeature env
                         coreFeature
                         uploadFeature
                         tagsFeature

    reverseFeature  <- initReverseFeature env
                         coreFeature
                         versionsFeature

    namesFeature    <- initNamesFeature env
                         coreFeature

    listFeature     <- initListFeature env
                         coreFeature
                         reverseFeature
                         downloadFeature
                         tagsFeature
                         versionsFeature

    platformFeature <- initPlatformFeature env
                         coreFeature

    --jsonFeature   <- initJsonFeature

    htmlFeature     <- initHtmlFeature env
                         coreFeature
                         packagesFeature
                         uploadFeature
                         checkFeature
                         usersFeature
                         versionsFeature
                         reverseFeature
                         tagsFeature
                         downloadFeature
                         listFeature
                         namesFeature
                         mirrorFeature
#endif

    -- The order of initialization above should be the same as
    -- the order of this list.
    let allFeatures :: [HackageFeature]
        allFeatures =
         [ getFeatureInterface coreFeature
         , getFeatureInterface usersFeature
         , getFeatureInterface mirrorFeature
         , getFeatureInterface uploadFeature
#ifndef MINIMAL
         , getFeatureInterface packagesFeature
         , getFeatureInterface distroFeature
         , getFeatureInterface checkFeature
         , getFeatureInterface reportsFeature
         , getFeatureInterface packageContentsFeature
         , getFeatureInterface documentationFeature
         , getFeatureInterface downloadFeature
         , getFeatureInterface tagsFeature
         , getFeatureInterface versionsFeature
         , getFeatureInterface reverseFeature
         , getFeatureInterface namesFeature
         , getFeatureInterface listFeature
         , getFeatureInterface platformFeature
         , getFeatureInterface htmlFeature
         , legacyRedirectsFeature uploadFeature
#endif
         ]

    -- Run all post init hooks, now that everyone's gotten a chance to register
    -- for them. This solution is iffy for initial feature hooks that rely on
    -- other features It also happens even in the backup/restore modes.
    mapM_ featurePostInit allFeatures

    return allFeatures
