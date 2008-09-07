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
  importPkgInfo,
  importTarballs,
  ) where

import qualified Distribution.Server.IndexUtils as PackageIndex (read)
import qualified Distribution.Server.Util.Tar as Tar
         ( Entry(..), fileName )
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog
         ( Entry(..), read )
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Types (PkgInfo(..))

import Distribution.Package
         ( PackageIdentifier, Package(packageId) )
import Distribution.PackageDescription
         ( parsePackageDescription )
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg )
import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( fromUTF8 )

import System.FilePath
         ( takeExtension )
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
         (  unpack )
import qualified Codec.Compression.GZip as GZip
import Control.Monad.Error () --intance Monad (Either String)
import Data.List
         ( sortBy )
import Data.Ord
         ( comparing )

import Prelude hiding (read)

newPkgInfo :: PackageIdentifier
           -> Tar.Entry
           -> UploadLog.Entry -> [UploadLog.Entry]
           -> Either String PkgInfo
newPkgInfo pkgid entry (UploadLog.Entry time user _) others =
  case parse (Tar.fileContent entry) of
      ParseFailed err -> fail $ Tar.fileName entry
                             ++ maybe "" (\n -> ":" ++ show n) lineno
                             ++ ": " ++ message
        where (lineno, message) = locatedErrorMsg err

      ParseOk _ pkg   -> return PkgInfo {
        pkgInfoId     = pkgid,
        pkgDesc       = pkg,
        pkgData       = Tar.fileContent entry,
        pkgTarball    = Nothing,
        pkgUploadTime = time,
        pkgUploadUser = user,
        pkgUploadOld  = [ (time', user')
                        | UploadLog.Entry time' user' _ <- others]
      }
  where parse = parsePackageDescription . fromUTF8 . BS.unpack

-- | Actually write the tarballs to disk and return an association of
-- 'PackageIdentifier' to the 'BlobStorage.BlobId' of the tarball added to the
-- 'BlobStorage' area.
--
importTarballs :: BlobStorage
               -> ByteString
               -> IO [(PackageIdentifier, BlobStorage.BlobId)]
importTarballs store archiveFile =
  case PackageIndex.read (,) archiveFile of
    Left  problem  -> fail problem
    Right tarballs -> sequence
      [ do blobid <- BlobStorage.add store (Tar.fileContent entry)
           return (pkgid, blobid)
      | (pkgid, entry) <- tarballs
      , takeExtension (Tar.fileName entry) == ".gz" ] --FIXME: .tar.gz

-- | Merge all the package info together
importPkgInfo :: ByteString
              -> String
              -> [(PackageIdentifier, BlobStorage.BlobId)]
              -> Either String ([PkgInfo], [UploadLog.Entry])
importPkgInfo indexFile logFile tarballInfo = do
  pkgDescs    <- PackageIndex.read (,) (GZip.decompress indexFile)
  logEntries  <-    UploadLog.read logFile
  
  (pkgs, extraLogEntries) <- mergeIndexWithUploadLog pkgDescs logEntries
  pkgs' <- mergeTarballs tarballInfo pkgs
  return (pkgs', extraLogEntries)

-- | Merge the package index meta data with the upload log to make initial
--   'PkgInfo' records, but without tarballs.
--
-- Also returns any upload log entries with no corresponding package info.
-- This happens for packages which got uploaded but subsequently deleted.
--
mergeIndexWithUploadLog :: [(PackageIdentifier, Tar.Entry)]
                        -> [(UploadLog.Entry, [UploadLog.Entry])]
                        -> Either String ([PkgInfo], [UploadLog.Entry])
mergeIndexWithUploadLog pkgs entries =
  mergePkgs [] [] $
    mergeBy comparingPackageId
      (sortBy (comparing fst)
              [ pkg | pkg@(_, entry) <- pkgs
                    , takeExtension (Tar.fileName entry) == ".cabal" ])
      (sortBy (comparing (\(UploadLog.Entry _ _ pkgid, _) -> pkgid)) entries)
  where
    comparingPackageId (pkgid, _) (UploadLog.Entry _ _ pkgid', _) =
      compare pkgid pkgid'

    mergePkgs merged nonexistant []  = Right (reverse merged, nonexistant)
    mergePkgs merged nonexistant (next:remaining) = case next of
      InBoth (pkgid, tarEntry) (logEntry, logEntries) ->
        case newPkgInfo pkgid tarEntry logEntry logEntries of
          Left problem  -> Left problem
          Right ok      -> mergePkgs (ok:merged) nonexistant remaining
      OnlyInLeft (pkgid, _) ->
        Left $ "Package with no upload log " ++ display pkgid
      OnlyInRight (entry,_) -> mergePkgs merged (entry:nonexistant) remaining

-- | The tarball info with the existing 'PkgInfo' records.
--
-- It's an errror to find additional tarballs, but we don't mind at the moment
-- if not all packages have a tarball, as that makes testing easier.
--
mergeTarballs :: [(PackageIdentifier, BlobStorage.BlobId)]
              -> [PkgInfo]
              -> Either String [PkgInfo]
mergeTarballs tarballInfo pkgs =
  mergePkgs [] $
    mergeBy comparingPackageId
      (sortBy (comparing fst) tarballInfo) pkgs

  where
    comparingPackageId (pkgid, _) pkginfo =
      compare pkgid (packageId pkginfo)

    mergePkgs merged []  = Right merged
    mergePkgs merged (next:remaining) = case next of
      InBoth (_, blobid) pkginfo -> mergePkgs (pkginfo':merged) remaining
         where pkginfo' = pkginfo { pkgTarball = Just blobid }
      OnlyInLeft (pkgid, _)          -> Left missing
         where missing = "Package tarball missing metadata " ++ display pkgid
      OnlyInRight            pkginfo -> mergePkgs (pkginfo:merged) remaining

mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b
