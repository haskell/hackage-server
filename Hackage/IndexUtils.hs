{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Hackage.IndexUtils (
  read, write,
  ) where

import qualified Hackage.Tar as Tar
         ( Entry(..), Entries(..), read, write, simpleFileEntry )

import Distribution.Package
         ( PackageIdentifier(..), Package(..), packageName, packageVersion )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
         ( GenericPackageDescription
         , parsePackageDescription, ParseResult(..) )
import Distribution.Text
         ( simpleParse )
import Distribution.Simple.Utils
         ( fromUTF8 )
import Distribution.Text
         ( display )

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import System.FilePath
         ( (</>), (<.>), takeExtension, splitDirectories, normalise )
import Prelude hiding (read)

-- | Parse an uncompressed tar repository index file from a 'ByteString'.
--
read :: Package pkg
     => (PackageIdentifier -> GenericPackageDescription -> ByteString -> pkg)
     -> ByteString
     -> Either String (PackageIndex pkg)
read mkPackage indexFileContent = collect [] entries
  where
    entries = Tar.read indexFileContent
    collect es' Tar.Done        = Right (PackageIndex.fromList es')
    collect es' (Tar.Next e es) = case entry e of
                       Just e' -> collect (e':es') es
                       Nothing -> collect     es'  es
    collect _   (Tar.Fail err)  = Left err

    entry Tar.Entry { Tar.fileName = fileName
                    , Tar.fileContent = content }
      | takeExtension fileName == ".cabal"
      , [pkgname,versionStr,_] <- splitDirectories (normalise fileName)
      , Just version <- simpleParse versionStr
      = let pkgid  = PackageIdentifier pkgname version
            pkgstr = fromUTF8 (BS.Char8.unpack content)
            pkg    = case parsePackageDescription pkgstr of
              ParseOk _ desc -> desc
              _              -> error $ "Couldn't read cabal file "
                                    ++ show fileName
         in Just (mkPackage pkgid pkg content)
    entry _ = Nothing

write :: Package pkg
      => (pkg -> ByteString)
      -> PackageIndex pkg
      -> ByteString
write externalPackageRep =
  Tar.write . map entry . PackageIndex.allPackages
  where
    entry pkg = Tar.simpleFileEntry fileName (externalPackageRep pkg)
      where fileName = packageName pkg </> display (packageVersion pkg)
                   </> packageName pkg <.> "cabal"
