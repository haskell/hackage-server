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

import Hackage.Tar (readTarArchive, TarHeader(..))
import Hackage.Types (PkgInfo(..))

import Distribution.Package
        ( PackageIdentifier(..), Package(..) )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
         ( GenericPackageDescription
         , parsePackageDescription, ParseResult(..) )
import Distribution.Text
         ( simpleParse )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die, warn, intercalate, fromUTF8)

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
     -> PackageIndex pkg
read mkPackage indexFileContent =
  PackageIndex.fromList $ do
  (hdr, content) <- Tar.read indexFileContent
  if takeExtension (tarFileName hdr) == ".cabal"
    then case splitDirectories (normalise (tarFileName hdr)) of
           [pkgname,vers,_] ->
             let parsed = parsePackageDescription
                            (fromUTF8 . BS.Char8.unpack $ content)
                 descr  = case parsed of
                   ParseOk _ d -> d
                   _           -> error $ "Couldn't read cabal file "
                                       ++ show (tarFileName hdr)
              in case simpleParse vers of
                   Just ver -> return $ mkPackage pkgid content descr
                     where pkgid = PackageIdentifier pkgname ver
                   _ -> []
           _ -> []
    else []


write :: Package pkg
      => (pkg -> ByteString)
      -> PackageIndex pkg
      -> ByteString
write externalPackageRep =
  Tar.write . map entry . PackageIndex.allPackages
  where
    entry pkg =
      let content = externalPackageRep pkg
       in TarEntry {
            entryHdr = TarHeader {
              tarFileName   = packageName pkg </> packageVersion pkg
                          </> packageName pkg <.> "cabal",
              tarFileMode   = 0,
              tarFileType   = TarNormalFile,
              tarLinkTarget = ""
            },
            entrySize       = BS.length content,
            entryModTime    = 0,
            entryCnt        = content
          }
