{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, TemplateHaskell,
      MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Distribution.Server.TarIndex.State
    where

import Control.Applicative ((<$>))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (put, modify)
import qualified Data.Map as Map

import Happstack.Data
import Happstack.State

import Data.TarIndex (TarIndex)

import Distribution.Server.Packages.State()
import Distribution.Server.Util.BlobStorage (BlobId)

data TarIndexMap = M {indexMap :: Map.Map BlobId TarIndex}
 deriving Typeable

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

instance Version TarIndexMap where

$(deriveSerialize ''TarIndexMap)

instance Component TarIndexMap where
    type Dependencies TarIndexMap = End
    initialValue = emptyTarIndex

emptyTarIndex :: TarIndexMap
emptyTarIndex = M Map.empty


$(mkMethods ''TarIndexMap
                [ 'addIndex
                , 'dropIndex
                , 'lookupIndex
                , 'replaceTarIndexMap
                ]
 )
