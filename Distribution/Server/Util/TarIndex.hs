{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, TemplateHaskell,
      MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- This is presently unused: features provide their own BlobId-to-TarIndex
-- mappings.

module Distribution.Server.Util.TarIndex
    where

import Control.Applicative ((<$>))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (put, modify)
import qualified Data.Map as Map

import Data.Acid     (makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.TarIndex (TarIndex)

import Distribution.Server.Framework.BlobStorage (BlobId)

data TarIndexMap = M {indexMap :: Map.Map BlobId TarIndex}
 deriving (Typeable, Show)

addIndex :: BlobId -> TarIndex -> Update TarIndexMap ()
addIndex blob index = modify $ insertTarIndex blob index

insertTarIndex :: BlobId -> TarIndex -> TarIndexMap -> TarIndexMap
insertTarIndex blob index (M state) = M (Map.insert blob index state)

dropIndex :: BlobId -> Update TarIndexMap ()
dropIndex blob = modify $ \(M state) -> M (Map.delete blob state)

lookupIndex :: BlobId -> Query TarIndexMap (Maybe TarIndex)
lookupIndex blob =  Map.lookup blob <$> asks indexMap

replaceTarIndexMap :: TarIndexMap -> Update TarIndexMap ()
replaceTarIndexMap = put

$(deriveSafeCopy 0 'base ''TarIndexMap)

initialTarIndexMap :: TarIndexMap
initialTarIndexMap = emptyTarIndex

emptyTarIndex :: TarIndexMap
emptyTarIndex = M Map.empty


$(makeAcidic ''TarIndexMap
                [ 'addIndex
                , 'dropIndex
                , 'lookupIndex
                , 'replaceTarIndexMap
                ]
 )
