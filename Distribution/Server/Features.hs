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
import Distribution.Server.Framework.ServerEnv (ServerEnv(..))
import Distribution.Server.Framework.Logging

import Distribution.Server.Features.StaticFiles (initStaticFilesFeature)
import Distribution.Server.Features.Users    (initUserFeature, UserFeature)
import Distribution.Server.Features.Core     (initCoreFeature, coreResource, queryGetPackageIndex)
import Distribution.Server.Features.Upload   (initUploadFeature)
import Distribution.Server.Features.Mirror   (initMirrorFeature)

#ifndef MINIMAL
import Distribution.Server.Features.TarIndexCache       (initTarIndexCacheFeature)
import Distribution.Server.Features.Html                (initHtmlFeature)
import Distribution.Server.Features.PackageCandidates   (initPackageCandidatesFeature, candidatesCoreResource, queryGetCandidateIndex)
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
import Distribution.Server.Features.Search              (initSearchFeature)
import Distribution.Server.Features.PackageList         (initListFeature)
import Distribution.Server.Features.HaskellPlatform     (initPlatformFeature)
import Distribution.Server.Features.UserDetails         (initUserDetailsFeature)
import Distribution.Server.Features.UserSignup          (initUserSignupFeature)
import Distribution.Server.Features.LegacyPasswds       (initLegacyPasswdsFeature)
import Distribution.Server.Features.EditCabalFiles      (initEditCabalFilesFeature)
import Distribution.Server.Features.HoogleData          (initHoogleDataFeature)
#endif
import Distribution.Server.Features.ServerIntrospect (serverIntrospectFeature)

#ifdef DEBUG
import Distribution.Server.Features.Crash
#endif

import Control.Applicative ((<$>))
import Distribution.Server.Packages.PackageIndex (allPackages)
import Distribution.Package (packageId)

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

-- | Initialize all features and run post-initialization hooks.
initHackageFeatures :: ServerEnv -> IO ([HackageFeature], UserFeature)
initHackageFeatures env@ServerEnv{serverVerbosity = verbosity} = do

    loginfo verbosity "Initialising features"

    -- Arguments denote feature dependencies.
    -- What follows is a topological sort along those lines
    staticFilesFeature <- initStaticFilesFeature env

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
    tarIndexCacheFeature <- initTarIndexCacheFeature env usersFeature

    packageContentsFeature <- initPackageContentsFeature env
                                coreFeature
                                tarIndexCacheFeature

    packagesFeature <- initRecentPackagesFeature env
                         usersFeature
                         coreFeature
                         packageContentsFeature

    userDetailsFeature <- initUserDetailsFeature env
                            usersFeature
                            coreFeature

    userSignupFeature <- initUserSignupFeature env
                           usersFeature
                           userDetailsFeature
                           uploadFeature

    legacyPasswdsFeature <- initLegacyPasswdsFeature env
                              usersFeature

    distroFeature   <- initDistroFeature env
                         usersFeature
                         coreFeature

    candidatesFeature <- initPackageCandidatesFeature env
                           usersFeature
                           coreFeature
                           uploadFeature
                           tarIndexCacheFeature

    reportsCoreFeature <- initBuildReportsFeature "reports-core" env
                         usersFeature
                         uploadFeature
                         (coreResource coreFeature)

    reportsCandidatesFeature <- initBuildReportsFeature "reports-candidates" env
                         usersFeature
                         uploadFeature
                         (candidatesCoreResource candidatesFeature)

    documentationCoreFeature <- initDocumentationFeature "documentation-core" env
                         (coreResource coreFeature)
                         (map packageId . allPackages <$> queryGetPackageIndex coreFeature)
                         uploadFeature
                         tarIndexCacheFeature

    documentationCandidatesFeature <- initDocumentationFeature "documentation-candidates" env
                         (candidatesCoreResource candidatesFeature)
                         (map packageId . allPackages <$> queryGetCandidateIndex candidatesFeature)
                         uploadFeature
                         tarIndexCacheFeature

    downloadFeature <- initDownloadFeature env
                         coreFeature
                         usersFeature

    tagsFeature     <- initTagsFeature env
                         coreFeature
                         uploadFeature

    versionsFeature <- initVersionsFeature env
                         coreFeature
                         uploadFeature
                         tagsFeature

    {- [reverse index disabled]
    reverseFeature  <- initReverseFeature env
                         coreFeature
                         versionsFeature
                         -}

    listFeature     <- initListFeature env
                         coreFeature
                         -- [reverse index disabled] reverseFeature
                         downloadFeature
                         tagsFeature
                         versionsFeature

    searchFeature   <- initSearchFeature env
                         coreFeature
                         listFeature

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
                         searchFeature
                         mirrorFeature
                         distroFeature
                         documentationCoreFeature
                         documentationCandidatesFeature
                         userDetailsFeature

    editCabalFeature <- initEditCabalFilesFeature env
                          usersFeature
                          coreFeature
                          uploadFeature

    hoogleDataFeature <- initHoogleDataFeature env
                           coreFeature
                           documentationCoreFeature
                           tarIndexCacheFeature
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
         , getFeatureInterface userDetailsFeature
         , getFeatureInterface userSignupFeature
         , getFeatureInterface legacyPasswdsFeature
         , getFeatureInterface distroFeature
         , getFeatureInterface candidatesFeature
         , getFeatureInterface reportsCoreFeature
         , getFeatureInterface reportsCandidatesFeature
         , getFeatureInterface documentationCoreFeature
         , getFeatureInterface documentationCandidatesFeature
         , getFeatureInterface downloadFeature
         , getFeatureInterface tagsFeature
         , getFeatureInterface versionsFeature
         -- [reverse index disabled] , getFeatureInterface reverseFeature
         , getFeatureInterface searchFeature
         , getFeatureInterface listFeature
         , getFeatureInterface platformFeature
         , getFeatureInterface htmlFeature
         , legacyRedirectsFeature uploadFeature
         , editCabalFeature
         , getFeatureInterface hoogleDataFeature
#endif
         , staticFilesFeature
         , serverIntrospectFeature allFeatures
#ifdef DEBUG
         , serverCrashFeature
#endif
         ]

    -- Run all post init hooks, now that everyone's gotten a chance to register
    -- for them. This solution is iffy for initial feature hooks that rely on
    -- other features It also happens even in the backup/restore modes.
    loginfo verbosity "Running feature post-init hooks"
    mapM_ featurePostInit allFeatures
    loginfo verbosity "Initialising features done"

    return (allFeatures, usersFeature)

-- | Checkpoint a feature's persistent state to disk.
featureCheckpoint :: HackageFeature -> IO ()
featureCheckpoint = mapM_ abstractStateCheckpoint . featureState

-- | Checkpoint all features' persistent state.
checkpointAllFeatures :: [HackageFeature] -> IO ()
checkpointAllFeatures = mapM_ featureCheckpoint

-- | Cleanly shut down a feature's state components.
featureShutdown :: HackageFeature -> IO ()
featureShutdown = mapM_ abstractStateClose . featureState

-- | Cleanly shut down all features' state components.
shutdownAllFeatures :: [HackageFeature] -> IO ()
shutdownAllFeatures   = mapM_ featureShutdown . reverse

