{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Util.Index
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Client.Index (
    read,
  ) where

import qualified Codec.Archive.Tar       as Tar
         ( read, Entries(..) )
import qualified Codec.Archive.Tar.Entry as Tar
         ( Entry(..), entryPath )

import Distribution.Package
import Distribution.Text
         ( simpleParse )

import Data.ByteString.Lazy (ByteString)
import System.FilePath.Posix
         ( splitDirectories, normalise )
import Prelude hiding (read)

-- | Parse an uncompressed tar repository index file from a 'ByteString'.
--
-- Takes a function to turn a tar entry into a package
--
-- This fails only if the tar is corrupted. Any entries not recognized as
-- belonging to a package are ignored.
--
read :: (PackageIdentifier -> Tar.Entry -> pkg)
     -> (FilePath -> Bool) -- ^ Should this file be included?
     -> ByteString
     -> Either String [pkg]
read mkPackage includeFile indexFileContent = collect [] entries
  where
    entries = Tar.read indexFileContent
    collect es' Tar.Done        = Right es'
    collect es' (Tar.Next e es) = case entry e of
                       Just e' -> collect (e':es') es
                       Nothing -> collect     es'  es
    collect _   (Tar.Fail err)  = Left (show err)

    entry e
      | [pkgname,versionStr,_] <- splitDirectories (normalise (Tar.entryPath e))
      , Just version <- simpleParse versionStr
      , True <- includeFile (Tar.entryPath e)
      = let pkgid = PackageIdentifier (mkPackageName pkgname) version
         in Just (mkPackage pkgid e)
    entry _ = Nothing
