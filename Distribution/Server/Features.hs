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
import Distribution.Server.Features.Security (initSecurityFeature)
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
import Distribution.Server.Features.PackageInfoJSON     (initPackageInfoJSONFeature)
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
import Distribution.Server.Features.AdminFrontend       (initAdminFrontendFeature)
import Distribution.Server.Features.AdminLog            (initAdminLogFeature)
import Distribution.Server.Features.HoogleData          (initHoogleDataFeature)
import Distribution.Server.Features.Votes               (initVotesFeature)
import Distribution.Server.Features.Sitemap             (initSitemapFeature)
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

    -- We have a three phase initialisation procedure...
    -- 1. in phase 1 all features can start independently (could be parallel)
    --    they load the data they need, but before having access to the other
    --    features they depend on
    -- 2. in phase 2 they have access to the other features they depend on
    --    this is serialised according to the dependencies of the features
    -- 3. in phase 3 we run all post-init actions. This could also be parallel.

    loginfo verbosity "Initialising features, part 1"

    mkStaticFilesFeature <- logStartup "static files" $
                            initStaticFilesFeature env
    mkUserFeature        <- logStartup "user" $
                            initUserFeature env
    mkCoreFeature        <- logStartup "core" $
                            initCoreFeature env
    mkSecurityFeature    <- logStartup "security" $
                            initSecurityFeature env
    mkMirrorFeature      <- logStartup "mirror" $
                            initMirrorFeature env
    mkUploadFeature      <- logStartup "upload" $
                            initUploadFeature env
#ifndef MINIMAL
    mkTarIndexCacheFeature   <- logStartup "tar index" $
                                initTarIndexCacheFeature env
    mkPackageContentsFeature <- logStartup "package contents" $
                                initPackageContentsFeature env
    mkRecentPackagesFeature  <- logStartup "recent packages" $
                                initRecentPackagesFeature env
    mkUserDetailsFeature   <- logStartup "user details" $
                              initUserDetailsFeature env
    mkUserSignupFeature    <- logStartup "user signup" $
                              initUserSignupFeature env
    mkLegacyPasswdsFeature <- logStartup "legacy passwords" $
                              initLegacyPasswdsFeature env
    mkDistroFeature        <- logStartup "distro" $
                              initDistroFeature env
    mkPackageCandidatesFeature       <- logStartup "package candidates" $
                                        initPackageCandidatesFeature env
    mkBuildReportsCoreFeature        <- logStartup "reports (core)" $
                                        initBuildReportsFeature "reports-core" env
    mkBuildReportsCandidatesFeature  <- logStartup "reports (candidates)" $
                                        initBuildReportsFeature "reports-candidates" env
    mkDocumentationCoreFeature       <- logStartup "documentation (core)" $
                                        initDocumentationFeature "documentation-core" env
    mkDocumentationCandidatesFeature <- logStartup "documentation (candidates)" $
                                        initDocumentationFeature "documentation-candidates" env
    mkDownloadFeature       <- logStartup "download counts" $
                               initDownloadFeature env
    mkTagsFeature           <- logStartup "tags" $
                               initTagsFeature env
    mkVersionsFeature       <- logStartup "versions" $
                               initVersionsFeature env
    -- mkReverseFeature     <- logStartup "reverse deps" $
    --                         initReverseFeature env
    mkListFeature           <- logStartup "list" $
                               initListFeature env
    mkSearchFeature         <- logStartup "search" $
                               initSearchFeature env
    mkPlatformFeature       <- logStartup "platform" $
                               initPlatformFeature env
    mkHtmlFeature           <- logStartup "html" $
                               initHtmlFeature env
    mkEditCabalFilesFeature <- logStartup "edit cabal files" $
                               initEditCabalFilesFeature env
    mkAdminFrontendFeature  <- logStartup "admn frontend" $
                               initAdminFrontendFeature env
    mkHoogleDataFeature     <- logStartup "hoogle" $
                               initHoogleDataFeature env
    mkVotesFeature          <- logStartup "votes" $
                               initVotesFeature env
    mkAdminLogFeature       <- logStartup "admin log" $
                               initAdminLogFeature env
    mkSitemapFeature        <- logStartup "sitemap" $
                               initSitemapFeature env
    mkPackageJSONFeature    <- logStartup "package info JSON" $
                               initPackageInfoJSONFeature env
#endif

    loginfo verbosity "Initialising features, part 2"

    -- Arguments denote feature dependencies.
    -- What follows is a topological sort along those lines
    staticFilesFeature <- mkStaticFilesFeature

    usersFeature    <- mkUserFeature

    coreFeature     <- mkCoreFeature
                         usersFeature

    securityFeature <- mkSecurityFeature
                         coreFeature

    mirrorFeature   <- mkMirrorFeature
                         coreFeature
                         usersFeature

    uploadFeature   <- mkUploadFeature
                         usersFeature
                         coreFeature

#ifndef MINIMAL
    tarIndexCacheFeature <- mkTarIndexCacheFeature
                              usersFeature

    packageContentsFeature <- mkPackageContentsFeature
                                coreFeature
                                tarIndexCacheFeature
                                usersFeature

    packagesFeature <- mkRecentPackagesFeature
                         usersFeature
                         coreFeature

    userDetailsFeature <- mkUserDetailsFeature
                            usersFeature
                            coreFeature

    userSignupFeature <- mkUserSignupFeature
                           usersFeature
                           userDetailsFeature
                           uploadFeature

    legacyPasswdsFeature <- mkLegacyPasswdsFeature
                              usersFeature

    distroFeature   <- mkDistroFeature
                         usersFeature
                         coreFeature

    candidatesFeature <- mkPackageCandidatesFeature
                           usersFeature
                           coreFeature
                           uploadFeature
                           tarIndexCacheFeature

    reportsCoreFeature <- mkBuildReportsCoreFeature
                         usersFeature
                         uploadFeature
                         (coreResource coreFeature)

    reportsCandidatesFeature <- mkBuildReportsCandidatesFeature
                         usersFeature
                         uploadFeature
                         (candidatesCoreResource candidatesFeature)

    documentationCoreFeature <- mkDocumentationCoreFeature
                         (coreResource coreFeature)
                         (map packageId . allPackages <$> queryGetPackageIndex coreFeature)
                         uploadFeature
                         tarIndexCacheFeature

    documentationCandidatesFeature <- mkDocumentationCandidatesFeature
                         (candidatesCoreResource candidatesFeature)
                         (map packageId . allPackages <$> queryGetCandidateIndex candidatesFeature)
                         uploadFeature
                         tarIndexCacheFeature

    downloadFeature <- mkDownloadFeature
                         coreFeature
                         usersFeature

    votesFeature    <- mkVotesFeature
                           coreFeature
                           usersFeature

    tagsFeature     <- mkTagsFeature
                         coreFeature
                         uploadFeature
                         usersFeature

    versionsFeature <- mkVersionsFeature
                         coreFeature
                         uploadFeature
                         tagsFeature

    {- [reverse index disabled]
    reverseFeature  <- mkReverseFeature
                         coreFeature
                         versionsFeature
                         -}

    listFeature     <- mkListFeature
                         coreFeature
                         -- [reverse index disabled] reverseFeature
                         downloadFeature
                         votesFeature
                         tagsFeature
                         versionsFeature
                         usersFeature
                         uploadFeature

    searchFeature   <- mkSearchFeature
                         coreFeature
                         listFeature

    platformFeature <- mkPlatformFeature

    htmlFeature     <- mkHtmlFeature
                         usersFeature
                         coreFeature
                         packageContentsFeature
                         uploadFeature
                         candidatesFeature
                         versionsFeature
                         -- [reverse index disabled] reverseFeature
                         tagsFeature
                         downloadFeature
                         votesFeature
                         listFeature
                         searchFeature
                         mirrorFeature
                         distroFeature
                         documentationCoreFeature
                         documentationCandidatesFeature
                         tarIndexCacheFeature
                         reportsCoreFeature
                         userDetailsFeature

    editCabalFeature <- mkEditCabalFilesFeature
                          usersFeature
                          coreFeature
                          uploadFeature

    adminFrontendFeature <- mkAdminFrontendFeature
                              usersFeature
                              userDetailsFeature
                              userSignupFeature
                              legacyPasswdsFeature

    hoogleDataFeature <- mkHoogleDataFeature
                           coreFeature
                           documentationCoreFeature
                           tarIndexCacheFeature

    adminLogFeature <- mkAdminLogFeature
                         usersFeature

    siteMapFeature <- mkSitemapFeature
                        coreFeature
                        documentationCoreFeature
                        tagsFeature

    packageInfoJSONFeature <- mkPackageJSONFeature
                                coreFeature
                                versionsFeature

#endif

    -- The order of initialization above should be the same as
    -- the order of this list.
    let allFeatures :: [HackageFeature]
        allFeatures =
         [ getFeatureInterface usersFeature
         , getFeatureInterface coreFeature
         , getFeatureInterface securityFeature
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
         , adminFrontendFeature
         , getFeatureInterface hoogleDataFeature
         , getFeatureInterface votesFeature
         , getFeatureInterface adminLogFeature
         , getFeatureInterface siteMapFeature
         , getFeatureInterface packageInfoJSONFeature
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
    sequence_
      [ logStartup ("post-init for " ++ name) $
        featurePostInit feature
      | feature@HackageFeature { featureName = name } <- allFeatures ]
    loginfo verbosity "Initialising features done"

    return (allFeatures, usersFeature)

  where
    logStartup feature action = do
      loginfo verbosity ("Initialising " ++ feature ++ " feature")
      logTiming verbosity ("Initialising " ++ feature ++ " feature done") action

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
