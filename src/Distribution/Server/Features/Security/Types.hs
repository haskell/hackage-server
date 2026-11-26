{-# LANGUAGE TemplateHaskell #-}
module Distribution.Server.Features.Security.Types (
    SecurityStateFiles(..)
  ) where

import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Framework.MemSize

import Hackage.Security.Util.Some
import qualified Hackage.Security.Server as Sec

data SecurityStateFiles =
     SecurityStateFiles {
       securityRoot         :: !Root       -- ^ root.json content and hashes
     , securityMirrors      :: !Mirrors    -- ^ mirrors.json content and hashes
     , securitySnapshotKey  :: !(Some Sec.Key) -- ^ key to sign snapshot.json
     , securityTimestampKey :: !(Some Sec.Key) -- ^ key to sign timestamp.json

       -- These two are a cache, they can be calculated from the info above
     , securitySnapshot     ::  Snapshot       -- ^ content of snapshot.json
     , securityTimestamp    ::  Timestamp      -- ^ content of timestamp.json
     }
  deriving (Show, Eq)

deriveSafeCopy 0 'base      ''SecurityStateFiles

instance MemSize SecurityStateFiles where
  memSize (SecurityStateFiles a b c d e f) = memSize6 a b c d e f
