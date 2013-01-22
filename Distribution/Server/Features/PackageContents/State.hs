{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
module Distribution.Server.Features.PackageContents.State
  ( TarIndicesState(..)
  , initialTarIndices
  , GetTarIndices(GetTarIndices)
  , ReplaceTarIndices(ReplaceTarIndices)
  ) where

import Data.Typeable (Typeable)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Server.Framework.MemSize

data TarIndicesState = TarIndicesState
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''TarIndicesState)

instance MemSize TarIndicesState where
  memSize _ = 0 -- TODO

initialTarIndices :: TarIndicesState
initialTarIndices = TarIndicesState

getTarIndices :: Query TarIndicesState TarIndicesState
getTarIndices = ask

replaceTarIndices :: TarIndicesState -> Update TarIndicesState ()
replaceTarIndices = put

makeAcidic ''TarIndicesState [
    'getTarIndices
  , 'replaceTarIndices
  ]
