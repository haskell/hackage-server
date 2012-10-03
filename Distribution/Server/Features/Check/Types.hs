{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Features.Check.Types
-- Copyright   :  (c) Matthew Gruen 2010
-- License     :  BSD-like
--
-- Data types for the candidate feature
-----------------------------------------------------------------------------
module Distribution.Server.Features.Check.Types where

import Distribution.Server.Packages.Types (PkgInfo)
import Distribution.Server.Framework.Instances ()

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Typeable (Typeable)
import Data.SafeCopy


------------------------------------------------------
-- | The information we keep about a candidate package.
-- 
-- It's currently possible to have candidates for packages which don't exist yet.
--
data CandPkgInfo = CandPkgInfo {
    candInfoId  :: !PackageIdentifier,
    -- there should be one ByteString and one BlobId per candidate.
    -- this was enforced in the types.. but it's easier to just
    -- reuse PkgInfo for the task.
    candPkgInfo :: !PkgInfo,
    -- | Warnings to display at the top of the package page.
    candWarnings   :: ![String],
    -- | Whether to allow non-maintainers to view the page or not.
    candPublic :: !Bool
} deriving (Show, Typeable)

deriveSafeCopy 0 'base ''CandPkgInfo

instance Package CandPkgInfo where packageId = candInfoId

instance Serialize CandPkgInfo where
  put pkgInfo = do
    Serialize.put (candInfoId pkgInfo)
    Serialize.put (candPkgInfo pkgInfo)
    Serialize.put (candWarnings pkgInfo)
    Serialize.put (candPublic pkgInfo)

  get = do
    infoId  <- Serialize.get
    pkgInfo <- Serialize.get
    warning <- Serialize.get
    public  <- Serialize.get
    return CandPkgInfo {
        candInfoId  = infoId,
        candPkgInfo = pkgInfo,
        candWarnings = warning,
        candPublic = public
    }

