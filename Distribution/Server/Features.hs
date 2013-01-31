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
import Distribution.Server.Framework.Types   (ServerEnv(..))
import Distribution.Server.Framework.Logging

import Distribution.Server.Features.Users    (initUserFeature, UserFeature)
import Distribution.Server.Features.Core     (initCoreFeature, coreResource)
import Distribution.Server.Features.Upload   (initUploadFeature)
import Distribution.Server.Features.Mirror   (initMirrorFeature)

#ifndef MINIMAL
import Distribution.Server.Features.TarIndexCache       (initTarIndexCacheFeature)
import Distribution.Server.Features.Html                (initHtmlFeature)
import Distribution.Server.Features.PackageCandidates   (initPackageCandidatesFeature)
import Distribution.Server.Features.RecentPackages      (initRecentPackagesFeature)
import Distribution.Server.Features.Distro              (initDistroFeature)
import Distribution.Server.Features.PackageContents     (initPackageContentsFeature)
import Distribution.Server.Features.Documentation       (initDocumentationFeature)
import Distribution.Server.Features.BuildReports        (initBuildReportsFeature)
import Distribution.Server.Features.LegacyRedirects     (legacyRedirectsFeature)
import Distribution.Server.Features.PreferredVersions   (initVersionsFeature)
-- [reverse index disabled] import Distribution.Server.Features.ReverseDependencies (initReverseFeature)
import Distribution.Server.Features.DownloadCount       (initDownloadFeature)
import Distribution.Server.Features.Tags                (initTagsFeature)
import Distribution.Server.Features.NameSearch          (initNamesFeature)
import Distribution.Server.Features.PackageList         (initListFeature)
import Distribution.Server.Features.HaskellPlatform     (initPlatformFeature)
#endif
import Distribution.Server.Features.ServerIntrospect (serverIntrospectFeature)

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

initHackageFeatures :: ServerEnv -> IO ([HackageFeature], UserFeature)
initHackageFeatures env@ServerEnv{serverVerbosity = verbosity} = do

    loginfo verbosity "Initialising features"

    -- Arguments denote feature dependencies.
    -- What follows is a topological sort along those lines

    usersFeature    <- initUserFeature env

    coreFeature     <- initCoreFeature env
                         usersFeature

    mirrorFeature   <- initMirrorFeature env
                         coreFeature
                         usersFeature

    uploadFeature   <- initUploadFeature env
                         coreFeature
                         usersFeature

#ifndef MINIMAL
    tarIndexCacheFeature <- initTarIndexCacheFeature env

    packageContentsFeature <- initPackageContentsFeature env
                                coreFeature
                                tarIndexCacheFeature

    packagesFeature <- initRecentPackagesFeature env
                         usersFeature
                         coreFeature
                         packageContentsFeature

    distroFeature   <- initDistroFeature env
                         usersFeature
                         coreFeature

    candidatesFeature <- initPackageCandidatesFeature env
                           usersFeature
                           coreFeature
                           uploadFeature
                           tarIndexCacheFeature

    reportsFeature  <- initBuildReportsFeature env
                         usersFeature
                         (coreResource coreFeature)

    documentationFeature <- initDocumentationFeature env
                         coreFeature
                         uploadFeature
                         tarIndexCacheFeature

    downloadFeature <- initDownloadFeature env
                         coreFeature

    tagsFeature     <- initTagsFeature env
                         coreFeature

    versionsFeature <- initVersionsFeature env
                         coreFeature
                         uploadFeature
                         tagsFeature

    {- [reverse index disabled]
    reverseFeature  <- initReverseFeature env
                         coreFeature
                         versionsFeature
                         -}

    namesFeature    <- initNamesFeature env
                         coreFeature

    listFeature     <- initListFeature env
                         coreFeature
                         -- [reverse index disabled] reverseFeature
                         downloadFeature
                         tagsFeature
                         versionsFeature

    platformFeature <- initPlatformFeature env

    htmlFeature     <- initHtmlFeature env
                         usersFeature
                         coreFeature
                         packagesFeature
                         uploadFeature
                         candidatesFeature
                         versionsFeature
                         -- [reverse index disabled] reverseFeature
                         tagsFeature
                         downloadFeature
                         listFeature
                         namesFeature
                         mirrorFeature
                         distroFeature
                         documentationFeature
#endif

    -- The order of initialization above should be the same as
    -- the order of this list.
    let allFeatures :: [HackageFeature]
        allFeatures =
         [ getFeatureInterface usersFeature
         , getFeatureInterface coreFeature
         , getFeatureInterface mirrorFeature
         , getFeatureInterface uploadFeature
#ifndef MINIMAL
         , getFeatureInterface tarIndexCacheFeature
         , getFeatureInterface packageContentsFeature
         , getFeatureInterface packagesFeature
         , getFeatureInterface distroFeature
         , getFeatureInterface candidatesFeature
         , getFeatureInterface reportsFeature
         , getFeatureInterface documentationFeature
         , getFeatureInterface downloadFeature
         , getFeatureInterface tagsFeature
         , getFeatureInterface versionsFeature
         -- [reverse index disabled] , getFeatureInterface reverseFeature
         , getFeatureInterface namesFeature
         , getFeatureInterface listFeature
         , getFeatureInterface platformFeature
         , getFeatureInterface htmlFeature
         , legacyRedirectsFeature uploadFeature
#endif
         , serverIntrospectFeature allFeatures
         ]

    -- Run all post init hooks, now that everyone's gotten a chance to register
    -- for them. This solution is iffy for initial feature hooks that rely on
    -- other features It also happens even in the backup/restore modes.
    loginfo verbosity "Running feature post-init hooks"
    mapM_ featurePostInit allFeatures
    loginfo verbosity "Initialising features done"

    return (allFeatures, usersFeature)

featureCheckpoint :: HackageFeature -> IO ()
featureCheckpoint = mapM_ abstractStateCheckpoint . featureState

checkpointAllFeatures :: [HackageFeature] -> IO ()
checkpointAllFeatures = mapM_ featureCheckpoint

featureShutdown :: HackageFeature -> IO ()
featureShutdown = mapM_ abstractStateClose . featureState

shutdownAllFeatures :: [HackageFeature] -> IO ()
shutdownAllFeatures   = mapM_ featureShutdown . reverse

