{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Acid where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import Data.Acid           hiding (update, query)
import Data.Acid.Advanced  (MethodState, query', update')
import Data.Acid.Local     (createCheckpointAndClose)
import System.FilePath     ((</>))
import System.IO.Unsafe    (unsafePerformIO)

import Distribution.Server.Packages.Platform        (PlatformPackages, initialPlatformPackages)
-- [reverse index disabled] import Distribution.Server.Packages.Reverse         (ReverseIndex, initialReverseIndex)
import Distribution.Server.Packages.State           (CandidatePackages,
                                                     initialCandidatePackages)

-- WARNING: if you add fields here, you must update checkpointAcid and stopAcid. Failure to do so will *not* result in a compiler error.
data Acid = Acid 
    { acidCandidatePackages  :: AcidState CandidatePackages
    , acidPlatformPackages   :: AcidState PlatformPackages
    -- [reverse index disabled] , acidReverseIndex       :: AcidState ReverseIndex
    }

class AcidComponent c where
    acidComponent :: Acid -> AcidState c

instance AcidComponent CandidatePackages where
    acidComponent = acidCandidatePackages

instance AcidComponent PlatformPackages where
    acidComponent = acidPlatformPackages

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
           initialCandidatePackages
           initialPlatformPackages
           -- [reverse index disabled] initialReverseIndex

startAcid' :: FilePath 
           -> CandidatePackages
           -> PlatformPackages
           -- [reverse index disabled] -> ReverseIndex
           -> IO Acid
startAcid' stateDir candidatePackages platformPackages
    -- [reverse index disabled] reverseIndex
    =
    do candidatePackages'  <- openLocalStateFrom (stateDir </> "CandidatePackages")  candidatePackages
       platformPackages'   <- openLocalStateFrom (stateDir </> "PlatformPackages")   platformPackages
       -- [reverse index disabled] reverseIndex'       <- openLocalStateFrom (stateDir </> "ReverseIndex")       reverseIndex
       let acid = Acid { acidCandidatePackages  = candidatePackages'
                       , acidPlatformPackages   = platformPackages'
                       -- [reverse index disabled] , acidReverseIndex       = reverseIndex'
                       }
       setAcid acid
       return acid

stopAcid :: Acid -> IO ()
stopAcid acid = 
    do setAcid (error "acid-state has been shutdown already.")
       createCheckpointAndClose (acidCandidatePackages acid)
       createCheckpointAndClose (acidPlatformPackages acid)
       -- [reverse index disabled] createCheckpointAndClose (acidReverseIndex acid)

checkpointAcid :: Acid -> IO ()
checkpointAcid acid =
    do createCheckpoint (acidCandidatePackages acid)
       createCheckpoint (acidPlatformPackages acid)
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

