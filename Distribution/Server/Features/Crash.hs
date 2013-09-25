-- | This is strictly for debugging only. Throws various kinds of exceptions.
module Distribution.Server.Features.Crash (serverCrashFeature) where

import Distribution.Server.Framework

import Data.Maybe
import Control.Exception
import Control.Concurrent

serverCrashFeature :: HackageFeature
serverCrashFeature = (emptyHackageFeature "crash") {
    featureDesc = "Throw various kinds of exceptions (for debugging purposes)"
  , featureResources = [
        (resourceAt "/crash/throw/:userError/:delay") {
          resourceDesc = [ (GET, "Throw a user error") ]
        , resourceGet  = [ ("", throwUserError) ]
        }
      ]
  , featureState = []
  }

throwUserError :: DynamicPath -> ServerPartE Response
throwUserError dpath = liftIO $ do
  let ex :: IOError
      ex = userError $ fromJust (lookup "userError" dpath)

      delay :: Int
      delay = read $ fromJust (lookup "delay" dpath)

  if delay == 0
    then throwIO ex
    else do tid <- myThreadId
            void . forkIO $ do threadDelay delay
                               putStrLn "Throwing exception.."
                               throwTo tid ex
            return . toResponse $ "Throwing exception in " ++ show delay ++ " microseconds"
