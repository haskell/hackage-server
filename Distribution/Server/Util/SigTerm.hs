{-# LANGUAGE CPP #-}
#if !(MIN_VERSION_base(4,6,0))
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif

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
#if MIN_VERSION_base(4,6,0)
import Control.Concurrent
         ( ThreadId, mkWeakThreadId )
import System.Mem.Weak
         ( Weak )
#else
import GHC.Conc.Sync
         ( ThreadId(..) )
import GHC.Weak
         ( Weak(..) )
import GHC.IO
         ( IO(IO) )
import GHC.Exts
         ( mkWeak#, unsafeCoerce# )
#endif
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

#if !(MIN_VERSION_base(4,6,0))
mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
mkWeakThreadId t@(ThreadId t#) = IO $ \s ->
   case mkWeak# t# t (unsafeCoerce# 0#) s of
      (# s1, w #) -> (# s1, Weak w #)
#endif

