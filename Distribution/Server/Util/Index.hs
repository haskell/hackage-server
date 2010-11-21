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
module Distribution.Server.Util.Index (
    read,
    write,
  ) where

import qualified Codec.Archive.Tar       as Tar
         ( read, write, Entries(..) )
import qualified Codec.Archive.Tar.Entry as Tar
         ( Entry(..), entryPath, fileEntry, toTarPath )

import Distribution.Package
import Distribution.Version
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Text
         ( display, simpleParse )

import Data.ByteString.Lazy (ByteString)
import System.FilePath.Posix
         ( (</>), (<.>), splitDirectories, normalise )
import Prelude hiding (read)

-- | Parse an uncompressed tar repository index file from a 'ByteString'.
--
-- Takes a function to turn a tar entry into a package
--
-- This fails only if the tar is corrupted. Any entries not recognized as
-- belonging to a package are ignored.
--
read :: (PackageIdentifier -> Tar.Entry -> pkg)
     -> ByteString
     -> Either String [pkg]
read mkPackage indexFileContent = collect [] entries
  where
    entries = Tar.read indexFileContent
    collect es' Tar.Done        = Right es'
    collect es' (Tar.Next e es) = case entry e of
                       Just e' -> collect (e':es') es
                       Nothing -> collect     es'  es
    collect _   (Tar.Fail err)  = Left err

    entry e
      | [pkgname,versionStr,_] <- splitDirectories (normalise (Tar.entryPath e))
      , Just version <- simpleParse versionStr
      , [] <- versionTags version
      = let pkgid = PackageIdentifier (PackageName pkgname) version
         in Just (mkPackage pkgid e)
    entry _ = Nothing

-- | Create an uncompressed tar repository index file as a 'ByteString'.
--
-- Takes a couple functions to turn a package into a tar entry. Extra
-- entries are also accepted.
--
write :: Package pkg
      => (pkg -> ByteString)
      -> (pkg -> Tar.Entry -> Tar.Entry)
      -> [Tar.Entry]
      -> PackageIndex pkg
      -> ByteString
write externalPackageRep updateEntry extras =
  Tar.write . (extras++) . map entry . PackageIndex.allPackages
  where
    entry pkg = updateEntry pkg
              . Tar.fileEntry tarPath
              $ externalPackageRep pkg
      where
        Right tarPath = Tar.toTarPath False fileName
        PackageName name = packageName pkg
        fileName = name </> display (packageVersion pkg)
                        </> name <.> "cabal"

