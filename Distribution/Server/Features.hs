{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, TypeOperators #-}
module Distribution.Server.Features where

import Distribution.Server.Feature

import Distribution.Server.Core (initCoreFeature)
--import Distribution.Server.Users.ServerParts (usersFeature)
--import Distribution.Server.Packages.ServerParts (packagePagesFeature)
--import Distribution.Server.Features.StaticFiles (staticFilesFeature)
import Distribution.Server.Features.LegacyRedirects (legacyRedirectsFeature)
--import Distribution.Server.Users.State (UsersStore)

import Happstack.State

import Data.Typeable

-- This module ties together all the hackage features that we will use.

hackageFeatures :: [HackageFeature]
hackageFeatures =
  [ coreFeature
  , legacyRedirectsFeature
  , usersFeature
  , packagePagesFeature
--, staticFilesFeature
  ]

-- For the sake of the happstack state system we need to give the list
-- of all data components used by each hackage feature.
data HackageOverallState = HackageOverallState
  deriving Typeable

instance Component HackageOverallState where
    type Dependencies HackageOverallState = End -- :+:
    initialValue = HackageOverallState

instance Version HackageOverallState
instance Serialize HackageOverallState where
    putCopy HackageOverallState = contain (return ())
    getCopy = contain (return HackageOverallState)

$(mkMethods ''HackageOverallState [])
