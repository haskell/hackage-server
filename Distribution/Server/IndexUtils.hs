{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Server.IndexUtils (
  read,
  write,
  writeGeneric,
  ) where

import qualified Distribution.Server.Util.Tar as Tar
         ( Entry(..), Entries(..), fileName, ExtendedHeader(..), FileType(..)
         , read, write, simpleFileEntry, toTarPath )
import Distribution.Server.Types
         ( PkgInfo(..) )

import Distribution.Package
         ( PackageIdentifier(..), Package(..), packageName, packageVersion )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Text
         ( simpleParse )
import Distribution.Text
         ( display )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import System.FilePath
         ( (</>), (<.>), splitDirectories, normalise )
import Prelude hiding (read)

write :: PackageIndex PkgInfo -> ByteString
write = writeGeneric pkgData setModTime
  where
    setModTime pkgInfo entry = entry {
      Tar.modTime   = utcToUnixTime (pkgUploadTime pkgInfo),
      Tar.headerExt = (Tar.headerExt entry) {
        Tar.ownerName = pkgUploadUser pkgInfo,
        Tar.groupName = "HackageDB"
      }
    }
    utcToUnixTime :: UTCTime -> Int
    utcToUnixTime = truncate . utcTimeToPOSIXSeconds

-- | Parse an uncompressed tar repository index file from a 'ByteString'.
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
      = let pkgid  = PackageIdentifier pkgname version
         in Just (mkPackage pkgid e)
    entry _ = Nothing

writeGeneric :: Package pkg
             => (pkg -> ByteString)
             -> (pkg -> Tar.Entry -> Tar.Entry)
             -> PackageIndex pkg
             -> ByteString
writeGeneric externalPackageRep updateEntry =
  Tar.write . map entry . PackageIndex.allPackages
  where
    entry pkg = updateEntry pkg
              . Tar.simpleFileEntry tarPath
              $ externalPackageRep pkg
      where
        Right tarPath = Tar.toTarPath Tar.NormalFile fileName
        fileName = packageName pkg </> display (packageVersion pkg)
                                   </> packageName pkg <.> "cabal"
