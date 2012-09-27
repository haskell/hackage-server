{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Acid where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import Data.Acid           hiding (update, query)
import Data.Acid.Advanced  (MethodState, query', update')
import Data.Acid.Local     (createCheckpointAndClose)
import System.FilePath     ((</>))
import System.IO.Unsafe    (unsafePerformIO)

import Distribution.Server.Features.BuildReports.BuildReports (BuildReports)
import Distribution.Server.Features.BuildReports.State        (initialBuildReports)
import Distribution.Server.Features.Distro.State      (Distros, initialDistros)
import Distribution.Server.Packages.Downloads       (DownloadCounts, initialDownloadCounts)
import Distribution.Server.Packages.Platform        (PlatformPackages, initialPlatformPackages)
import Distribution.Server.Packages.Preferred       (PreferredVersions, initialPreferredVersions)
-- [reverse index disabled] import Distribution.Server.Packages.Reverse         (ReverseIndex, initialReverseIndex)
import Distribution.Server.Packages.State           (CandidatePackages, Documentation, HackageTrustees, HackageUploaders, PackageMaintainers, PackagesState,
                                                     initialCandidatePackages, initialDocumentation, 
                                                     initialHackageTrustees, initialHackageUploaders,
                                                     initialPackageMaintainers, initialPackagesState)
import Distribution.Server.Packages.Tag             (PackageTags, initialPackageTags)
import Distribution.Server.Users.State              (MirrorClients, initialMirrorClients)

-- WARNING: if you add fields here, you must update checkpointAcid and stopAcid. Failure to do so will *not* result in a compiler error.
data Acid = Acid 
    { acidBuildReports       :: AcidState BuildReports
    , acidCandidatePackages  :: AcidState CandidatePackages
    , acidDistros            :: AcidState Distros
    , acidDocumentation      :: AcidState Documentation
    , acidDownloadCounts     :: AcidState DownloadCounts
    , acidHackageTrustees    :: AcidState HackageTrustees
    , acidHackageUploaders   :: AcidState HackageUploaders
    , acidMirrorClients      :: AcidState MirrorClients
    , acidPackageMaintainers :: AcidState PackageMaintainers
    , acidPackagesState      :: AcidState PackagesState
    , acidPackageTags        :: AcidState PackageTags
    , acidPlatformPackages   :: AcidState PlatformPackages
    , acidPreferredVersions  :: AcidState PreferredVersions
    -- [reverse index disabled] , acidReverseIndex       :: AcidState ReverseIndex
    }

class AcidComponent c where
    acidComponent :: Acid -> AcidState c

instance AcidComponent BuildReports where
    acidComponent = acidBuildReports

instance AcidComponent CandidatePackages where
    acidComponent = acidCandidatePackages

instance AcidComponent Distros where
    acidComponent = acidDistros

instance AcidComponent Documentation where
    acidComponent = acidDocumentation

instance AcidComponent DownloadCounts where
    acidComponent = acidDownloadCounts

instance AcidComponent HackageTrustees where
    acidComponent = acidHackageTrustees

instance AcidComponent HackageUploaders where
    acidComponent = acidHackageUploaders

instance AcidComponent MirrorClients where
    acidComponent = acidMirrorClients

instance AcidComponent PackageMaintainers where
    acidComponent = acidPackageMaintainers

instance AcidComponent PackagesState where
    acidComponent = acidPackagesState

instance AcidComponent PackageTags where
    acidComponent = acidPackageTags

instance AcidComponent PlatformPackages where
    acidComponent = acidPlatformPackages

instance AcidComponent PreferredVersions where
    acidComponent = acidPreferredVersions

-- [reverse index disabled] instance AcidComponent ReverseIndex where
-- [reverse index disabled]     acidComponent = acidReverseIndex


acidRef :: IORef Acid
acidRef = unsafePerformIO $ newIORef (error "acid not initialized.")
{-# NOINLINE acidRef #-}

-- FIXME: a strictly temporary measure, while we're converting to encapsulated state use
unsafeGetAcid :: AcidComponent c => AcidState c
unsafeGetAcid = unsafePerformIO $ do
  acid <- liftIO $ readIORef acidRef
  return (acidComponent acid)

setAcid :: Acid -> IO ()
setAcid acid =
    writeIORef acidRef acid

startAcid :: FilePath -> IO Acid
startAcid stateDir =
    startAcid' stateDir 
           initialBuildReports
           initialCandidatePackages
           initialDistros
           initialDocumentation
           initialDownloadCounts
           initialHackageTrustees
           initialHackageUploaders
           initialMirrorClients
           initialPackageMaintainers
           initialPackagesState
           initialPackageTags
           initialPlatformPackages
           initialPreferredVersions
           -- [reverse index disabled] initialReverseIndex

startAcid' :: FilePath 
           -> BuildReports
           -> CandidatePackages
           -> Distros
           -> Documentation
           -> DownloadCounts
           -> HackageTrustees
           -> HackageUploaders
           -> MirrorClients
           -> PackageMaintainers
           -> PackagesState
           -> PackageTags
           -> PlatformPackages
           -> PreferredVersions
           -- [reverse index disabled] -> ReverseIndex
           -> IO Acid
startAcid' stateDir buildReports candidatePackages distros documentation downloadCounts hackageTrustees hackageUploaders mirrorClients packageMaintainers packagesState packageTags platformPackages preferredVersions
    -- [reverse index disabled] reverseIndex
    =
    do buildReports'       <- openLocalStateFrom (stateDir </> "BuildReports")       buildReports
       candidatePackages'  <- openLocalStateFrom (stateDir </> "CandidatePackages")  candidatePackages
       distros'            <- openLocalStateFrom (stateDir </> "Distros")            distros
       documentation'      <- openLocalStateFrom (stateDir </> "Documentation")      documentation
       downloadCounts'     <- openLocalStateFrom (stateDir </> "DownloadCounts")     downloadCounts
       hackageTrustees'    <- openLocalStateFrom (stateDir </> "HackageTrustees")    hackageTrustees
       hackageUploaders'   <- openLocalStateFrom (stateDir </> "HackageUploaders")   hackageUploaders
       mirrorClients'      <- openLocalStateFrom (stateDir </> "MirrorClients")      mirrorClients
       packageMaintainers' <- openLocalStateFrom (stateDir </> "PackageMaintainers") packageMaintainers
       packagesState'      <- openLocalStateFrom (stateDir </> "PackagesState")      packagesState
       packageTags'        <- openLocalStateFrom (stateDir </> "PackageTags")        packageTags
       platformPackages'   <- openLocalStateFrom (stateDir </> "PlatformPackages")   platformPackages
       preferredVersions'  <- openLocalStateFrom (stateDir </> "PreferredVersions")  preferredVersions
       -- [reverse index disabled] reverseIndex'       <- openLocalStateFrom (stateDir </> "ReverseIndex")       reverseIndex
       let acid = Acid { acidBuildReports       = buildReports' 
                       , acidCandidatePackages  = candidatePackages'
                       , acidDistros            = distros'
                       , acidDocumentation      = documentation'
                       , acidDownloadCounts     = downloadCounts'
                       , acidHackageTrustees    = hackageTrustees'
                       , acidHackageUploaders   = hackageUploaders'
                       , acidMirrorClients      = mirrorClients'
                       , acidPackageMaintainers = packageMaintainers'
                       , acidPackagesState      = packagesState'
                       , acidPackageTags        = packageTags'
                       , acidPlatformPackages   = platformPackages'
                       , acidPreferredVersions  = preferredVersions'
                       -- [reverse index disabled] , acidReverseIndex       = reverseIndex'
                       }
       setAcid acid
       return acid

stopAcid :: Acid -> IO ()
stopAcid acid = 
    do setAcid (error "acid-state has been shutdown already.")
       createCheckpointAndClose (acidBuildReports acid)
       createCheckpointAndClose (acidCandidatePackages acid)
       createCheckpointAndClose (acidDistros acid)
       createCheckpointAndClose (acidDocumentation acid)
       createCheckpointAndClose (acidDownloadCounts acid)
       createCheckpointAndClose (acidHackageTrustees acid)
       createCheckpointAndClose (acidHackageUploaders acid)
       createCheckpointAndClose (acidMirrorClients acid)
       createCheckpointAndClose (acidPackageMaintainers acid)
       createCheckpointAndClose (acidPackagesState acid)
       createCheckpointAndClose (acidPackageTags acid)
       createCheckpointAndClose (acidPlatformPackages acid)
       createCheckpointAndClose (acidPreferredVersions acid)
       -- [reverse index disabled] createCheckpointAndClose (acidReverseIndex acid)

checkpointAcid :: Acid -> IO ()
checkpointAcid acid =
    do createCheckpoint (acidBuildReports acid)
       createCheckpoint (acidCandidatePackages acid)
       createCheckpoint (acidDistros acid)
       createCheckpoint (acidDocumentation acid)
       createCheckpoint (acidDownloadCounts acid)
       createCheckpoint (acidHackageTrustees acid)
       createCheckpoint (acidHackageUploaders acid)
       createCheckpoint (acidMirrorClients acid)
       createCheckpoint (acidPackageMaintainers acid)
       createCheckpoint (acidPackagesState acid)
       createCheckpoint (acidPackageTags acid)
       createCheckpoint (acidPlatformPackages acid)
       createCheckpoint (acidPreferredVersions acid)
       -- [reverse index disabled] createCheckpoint (acidReverseIndex acid)

update :: ( AcidComponent (MethodState event)
          , UpdateEvent event
          , MonadIO m) => event -> m (EventResult event)
update ev = 
    do acid <- liftIO $ readIORef acidRef
       update' (acidComponent acid) ev

query :: ( AcidComponent (MethodState event)
          , QueryEvent event
          , MonadIO m) => event -> m (EventResult event)
query ev = 
    do acid <- liftIO $ readIORef acidRef
       query' (acidComponent acid) ev

