{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.State where

import Distribution.Server.Instances ()

import Distribution.Server.Packages.State
import Distribution.Server.Users.Permissions (Permissions)
import Distribution.Server.Distributions.State (Distros)
import Distribution.Server.TarIndex.State (TarIndexMap)

import Happstack.State

import Data.Typeable

data HackageEntryPoint = HackageEntryPoint deriving Typeable

instance Version HackageEntryPoint
instance Serialize HackageEntryPoint where
    putCopy HackageEntryPoint = contain $ return ()
    getCopy = contain $ return HackageEntryPoint

instance Component HackageEntryPoint where
    type Dependencies HackageEntryPoint
        = PackagesState :+: Documentation :+: Permissions :+:
          Distros :+: TarIndexMap :+: End
    initialValue = HackageEntryPoint


$(mkMethods ''HackageEntryPoint [])

