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

import qualified Distribution.Server.Util.Tar as Tar
         ( Entry(..), Entries(..), fileName, FileType(..)
         , read, write, simpleFileEntry, toTarPath )

import Distribution.Package
         ( PackageIdentifier(..), Package(..), packageName, packageVersion
         , PackageName(..))
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Text
         ( display, simpleParse )

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import System.FilePath
         ( (</>), (<.>), splitDirectories, normalise )
import Prelude hiding (read)

-- | Parse an uncompressed tar repository index file from a 'ByteString'.
--
-- Takes a function to turn a tar entry into a package
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
      | [pkgname,versionStr,_] <- splitDirectories (normalise (Tar.fileName e))
      , Just version <- simpleParse versionStr
      = let pkgid  = PackageIdentifier (PackageName pkgname) version
         in Just (mkPackage pkgid e)
    entry _ = Nothing

-- | Create an uncompressed tar repository index file as a 'ByteString'.
--
-- Takes a couple functions to turn a package into a tar entry
--
write :: Package pkg
      => (pkg -> ByteString)
      -> (pkg -> Tar.Entry -> Tar.Entry)
      -> PackageIndex pkg
      -> ByteString
write externalPackageRep updateEntry =
  Tar.write . map entry . PackageIndex.allPackages
  where
    entry pkg = updateEntry pkg
              . Tar.simpleFileEntry tarPath
              $ externalPackageRep pkg
      where
        Right tarPath = Tar.toTarPath Tar.NormalFile fileName
        PackageName name = packageName pkg
        fileName = name </> display (packageVersion pkg)
                        </> name <.> "cabal"
