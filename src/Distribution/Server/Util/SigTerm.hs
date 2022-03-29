{-# LANGUAGE CPP #-}

module Distribution.Server.Util.SigTerm (onSigTermCleanShutdown) where

import System.Posix.Signals
         ( installHandler
         , Handler(Catch)
         , softwareTermination
         )
import Control.Exception
         ( AsyncException(UserInterrupt), throwTo )
import Control.Concurrent
         ( myThreadId )
import Control.Concurrent
         ( ThreadId, mkWeakThreadId )
import System.Mem.Weak
         ( Weak )
import System.Mem.Weak
         ( deRefWeak )

-- | On SIGTERM, throw 'UserInterrupt' to the calling thread.
--
onSigTermCleanShutdown :: IO ()
onSigTermCleanShutdown = do
    wtid <- mkWeakThreadId =<< myThreadId
    _ <- installHandler softwareTermination
                        (Catch (cleanShutdownHandler wtid))
                        Nothing
    return ()
  where
    cleanShutdownHandler :: Weak ThreadId -> IO ()
    cleanShutdownHandler wtid = do
      mtid <- deRefWeak wtid
      case mtid of
        Nothing  -> return ()
        Just tid -> throwTo tid UserInterrupt
