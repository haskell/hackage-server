{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable, NamedFieldPuns #-}
module Distribution.Server.Features.TarIndexCache.State (
    TarIndexCache(..)
  , initialTarIndexCache
  , GetTarIndexCache(GetTarIndexCache)
  , ReplaceTarIndexCache(ReplaceTarIndexCache)
  , FindTarIndex(FindTarIndex)
  , SetTarIndex(SetTarIndex)
  ) where

-- TODO: use strict map? (Can we rely on containers >= 0.5?)

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (put, modify)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<$>))

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Framework.MemSize

data TarIndexCache = TarIndexCache {
    tarIndexCacheMap :: Map BlobId BlobId
  }
  deriving (Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''TarIndexCache)

instance MemSize TarIndexCache where
  memSize st = 2 + memSize (tarIndexCacheMap st)

initialTarIndexCache :: TarIndexCache
initialTarIndexCache = TarIndexCache (Map.empty)

getTarIndexCache :: Query TarIndexCache TarIndexCache
getTarIndexCache = ask

replaceTarIndexCache :: TarIndexCache -> Update TarIndexCache ()
replaceTarIndexCache = put

getTarIndexCacheMap :: Query TarIndexCache (Map BlobId BlobId)
getTarIndexCacheMap = asks tarIndexCacheMap

modifyTarIndexCacheMap :: (Map BlobId BlobId -> Map BlobId BlobId)
                       -> Update TarIndexCache ()
modifyTarIndexCacheMap f = modify $ \st@TarIndexCache{tarIndexCacheMap} ->
                             st { tarIndexCacheMap = f tarIndexCacheMap }

findTarIndex :: BlobId -> Query TarIndexCache (Maybe BlobId)
findTarIndex blobId = Map.lookup blobId <$> getTarIndexCacheMap

setTarIndex :: BlobId -> BlobId -> Update TarIndexCache ()
setTarIndex tar index = modifyTarIndexCacheMap (Map.insert tar index)

makeAcidic ''TarIndexCache [
    'getTarIndexCache
  , 'replaceTarIndexCache
  , 'findTarIndex
  , 'setTarIndex
  ]
