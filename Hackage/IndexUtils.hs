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
  readPackageIndex,
  ) where

import Hackage.Tar
import Hackage.Types (PkgInfo(..))

import Distribution.Package (PackageIdentifier(..), Package(..), Dependency(Dependency))
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))
import Distribution.Text
         ( simpleParse )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die, warn, intercalate, fromUTF8)

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.IO.Error (isDoesNotExistError)

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
readPackageIndex :: Verbosity -> FilePath -> IO (PackageIndex PkgInfo)
readPackageIndex verbosity indexFile =
   fmap parseRepoIndex (BS.readFile indexFile)
          `catch` (\e -> do case e of
                              IOException ioe | isDoesNotExistError ioe ->
                                warn verbosity "The package list does not exist. Run 'cabal update' to download it."
                              _ -> warn verbosity (show e)
                            return (PackageIndex.fromList []))

  where
    -- | Parse a repository index file from a 'ByteString'.
    --
    -- All the 'PkgInfo's are marked as having come from the given 'Repo'.
    --
    parseRepoIndex :: ByteString -> PackageIndex PkgInfo
    parseRepoIndex s = PackageIndex.fromList $ do
      (hdr, content) <- readTarArchive s
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
                       Just ver -> return PkgInfo {
                           pkgInfoId = PackageIdentifier pkgname ver,
                           pkgDesc = descr,
                           pkgData = content
                         }
                       _ -> []
               _ -> []
        else []
