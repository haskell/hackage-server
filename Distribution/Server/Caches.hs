{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.Caches
    ( fetchPackagesPage
    , fetchIndexTarball
    , cacheThread
    ) where

import Data.ByteString.Lazy as Lazy
import Data.Dynamic
import Data.IORef
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

import HAppS.State
import HAppS.Data
import HAppS.Server

import Distribution.Server.PackagesState
import Distribution.Server.Pages.Index

data Cache = Cache {
       cachePackagesPage :: Response,
       cacheIndexTarball :: ByteString
     } deriving (Typeable)

{-# NOINLINE cacheEnv #-}
cacheEnv :: IORef Cache
cacheEnv = unsafePerformIO $ newIORef $ error "Distribution.Server.Caches: cache not initialized."

fetchPackagesPage :: IO Response
fetchPackagesPage = fmap cachePackagesPage (readIORef cacheEnv)

fetchIndexTarball :: IO ByteString
fetchIndexTarball = fmap cacheIndexTarball (readIORef cacheEnv)

cacheThread :: IO ()
cacheThread = do updateCache
                 getEvent <- getEventStream
                 let loop = do item <- getEvent
                               case fromDynamic (eventData item) of
                                 Just BulkImport{}
                                     -> do takeMVar (eventFinished item)
                                           updateCache
                                 _   -> return ()
                               loop
                 forkIO $ loop
                 return ()


updateCache = do state <- query $ GetPackagesState
                 let pkgIndex = generatePackageIndex (packageList state)
                     packagePage = toResponse (packageIndex (packageList state))
                 -- FIXME: compute in parallel
                 evaluate $ Lazy.length pkgIndex
                 evaluate $ Lazy.length (rsBody packagePage)
                 atomicModifyIORef cacheEnv $ \_ -> (Cache { cacheIndexTarball = pkgIndex
                                                           , cachePackagesPage = packagePage }
                                                    ,())


