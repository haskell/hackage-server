-----------------------------------------------------------------------------
-- |
-- Module      :  ImportClient.PkgIndex
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for importing cabal files from a 00-index.tar.gz file
-----------------------------------------------------------------------------
module ImportClient.PkgIndex (
  readPkgIndex
  ) where

import qualified Distribution.Server.Util.Index as PackageIndex (read)
import qualified Codec.Archive.Tar.Entry as Tar (Entry(..), EntryContent(..))

import Distribution.Package

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Codec.Compression.GZip as GZip

import Prelude hiding (read)


readPkgIndex :: ByteString -> Either String [(PackageIdentifier, ByteString)]
readPkgIndex = fmap extractCabalFiles
             . PackageIndex.read (,)
             . GZip.decompress
  where
    extractCabalFiles entries =
      [ (pkgid, cabalFile)
      | (pkgid, Tar.Entry {
                          Tar.entryContent = Tar.NormalFile cabalFile _
                }) <- entries ]

