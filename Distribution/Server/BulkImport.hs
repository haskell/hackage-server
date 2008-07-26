-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.BulkImport
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for importing data from the old hackage server.
-----------------------------------------------------------------------------
module Distribution.Server.BulkImport (
  read
  ) where

import qualified Distribution.Server.IndexUtils as PackageIndex (read)
import Distribution.Server.Types (PkgInfo(..))

import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)

import Prelude hiding (read)

read :: ByteString -> ByteString -> Either String [PkgInfo]
read indexFile _logFile = PackageIndex.read indexFile
