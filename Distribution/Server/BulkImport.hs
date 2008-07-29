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
import qualified Distribution.Server.Tar as Tar
         ( Entry(..) )
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog
         ( Entry(..), read )
import Distribution.Server.Types (PkgInfo(..))

import Distribution.Package
         ( PackageIdentifier )
import Distribution.PackageDescription
         ( parsePackageDescription )
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg )
import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( fromUTF8 )

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

importPkgInfo :: PackageIdentifier
              -> Tar.Entry
              -> UploadLog.Entry -> [UploadLog.Entry]
              -> Either String PkgInfo
importPkgInfo pkgid
  Tar.Entry { Tar.fileName = fileName, Tar.fileContent = pkgstr }
  (UploadLog.Entry time user _) others
  = case parsePackageDescription (fromUTF8 (BS.unpack pkgstr)) of
      ParseFailed err -> fail $ fileName
                             ++ maybe "" (\n -> ":" ++ show n) lineno
                             ++ ": " ++ message
        where (lineno, message) = locatedErrorMsg err

      ParseOk _ pkg   -> return PkgInfo {
        pkgInfoId     = pkgid,
        pkgDesc       = pkg,
        pkgData       = pkgstr,
        pkgUploadTime = time,
        pkgUploadUser = user,
        pkgUploadOld  = [ (time', user')
                        | UploadLog.Entry time' user' _ <- others]
      }

read :: ByteString -> String -> Either String ([PkgInfo], [UploadLog.Entry])
read indexFile logFile = do
  pkgs    <- PackageIndex.read (,) (GZip.decompress indexFile)
  entries <-    UploadLog.read logFile
  
  mergePkgs [] [] $
    mergeBy comparingPackageId
      (sortBy (comparing fst) pkgs)
      (sortBy (comparing (\(UploadLog.Entry _ _ pkgid, _) -> pkgid)) entries)
  where
    comparingPackageId (pkgid, _) (UploadLog.Entry _ _ pkgid', _) =
      compare pkgid pkgid'

    mergePkgs merged nonexistant []  = Right (merged, nonexistant)
    mergePkgs merged nonexistant (next:remaining) = case next of
      InBoth (pkgid, tarEntry) (logEntry, logEntries) ->
        case importPkgInfo pkgid tarEntry logEntry logEntries of
          Left problem  -> Left problem
          Right ok      -> mergePkgs (ok:merged) nonexistant remaining
      OnlyInLeft (pkgid, _) ->
        Left $ "Package with no upload log " ++ display pkgid
      OnlyInRight (entry,_) -> mergePkgs merged (entry:nonexistant) remaining

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
