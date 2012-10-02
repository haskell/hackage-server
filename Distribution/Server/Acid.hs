{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Acid where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import Data.Acid           hiding (update, query)
import Data.Acid.Advanced  (MethodState, query', update')
import Data.Acid.Local     (createCheckpointAndClose)
import System.FilePath     ((</>))
import System.IO.Unsafe    (unsafePerformIO)

-- [reverse index disabled] import Distribution.Server.Packages.Reverse         (ReverseIndex, initialReverseIndex)

-- WARNING: if you add fields here, you must update checkpointAcid and stopAcid. Failure to do so will *not* result in a compiler error.
data Acid = Acid 
    {
    -- [reverse index disabled] , acidReverseIndex       :: AcidState ReverseIndex
    }

class AcidComponent c where
    acidComponent :: Acid -> AcidState c

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
           -- [reverse index disabled] initialReverseIndex

startAcid' :: FilePath 
           -- [reverse index disabled] -> ReverseIndex
           -> IO Acid
startAcid' stateDir
    -- [reverse index disabled] reverseIndex
    =
    do -- [reverse index disabled] reverseIndex'       <- openLocalStateFrom (stateDir </> "ReverseIndex")       reverseIndex
       let acid = Acid {
                       -- [reverse index disabled] , acidReverseIndex       = reverseIndex'
                       }
       setAcid acid
       return acid

stopAcid :: Acid -> IO ()
stopAcid acid = 
    do setAcid (error "acid-state has been shutdown already.")
       -- [reverse index disabled] createCheckpointAndClose (acidReverseIndex acid)

checkpointAcid :: Acid -> IO ()
checkpointAcid acid = return ()
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

