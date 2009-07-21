{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.State where

import Distribution.Server.Instances ()

import Distribution.Package (PackageIdentifier,Package(packageId),PackageName)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import Distribution.Server.Users.Types (UserId,UserName,UserAuth)
import Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Util.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import Distribution.Server.BuildReport.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Distribution.Server.Packages.State
import Distribution.Server.Users.State

import Happstack.State
import Happstack.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time.Clock (UTCTime(..))

data HackageEntryPoint = HackageEntryPoint deriving Typeable

instance Version HackageEntryPoint
instance Serialize HackageEntryPoint where
    putCopy HackageEntryPoint = contain $ return ()
    getCopy = contain $ return HackageEntryPoint

instance Component HackageEntryPoint where
    type Dependencies HackageEntryPoint = PackagesState :+: Documentation :+: Permissions :+: End
    initialValue = HackageEntryPoint


$(mkMethods ''HackageEntryPoint [])

