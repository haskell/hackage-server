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

import qualified Distribution.Server.IndexUtils as PackageIndex (readGeneric)
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
import Control.Monad.Error () --intance Monad (Either String)
import Data.Time.LocalTime
         ( zonedTimeToUTC )
import Data.List
         ( sortBy )
import Data.Ord
         ( comparing )

import Prelude hiding (read)

importPkgInfo :: PackageIdentifier -> Tar.Entry -> UploadLog.Entry
              -> Either String PkgInfo
importPkgInfo pkgid
  Tar.Entry { Tar.fileName = fileName, Tar.fileContent = pkgstr }
  (UploadLog.Entry _ ztime user others)
  = case parsePackageDescription (fromUTF8 (BS.unpack pkgstr)) of
      ParseFailed err -> fail $ fileName
                             ++ maybe "" (\n -> ":" ++ show n) lineno
                             ++ ": " ++ message
        where (lineno, message) = locatedErrorMsg err

      ParseOk _ pkg   -> return PkgInfo {
        pkgInfoId     = pkgid,
        pkgDesc       = pkg,
        pkgUploadTime = zonedTimeToUTC ztime,
        pkgData       = pkgstr
      }

read :: ByteString -> String -> Either String [PkgInfo]
read indexFile logFile = do
  pkgs    <- PackageIndex.readGeneric (,) indexFile
  entries <-    UploadLog.read logFile
  
  mergePkgs [] $
    mergeBy comparingPackageId
      (sortBy (comparing fst) pkgs)
      (sortBy (comparing (\(UploadLog.Entry pkgid _ _ _) -> pkgid)) entries)
  where
    comparingPackageId (pkgid, _) (UploadLog.Entry pkgid' _ _ _) =
      compare pkgid pkgid'

    mergePkgs merged []  = Right merged
    mergePkgs merged (next:remaining) = case next of
      InBoth (pkgid, tarEntry) logEntry ->
        case importPkgInfo pkgid tarEntry logEntry of
          Left problem  -> Left problem
          Right ok      -> mergePkgs (ok:merged) remaining
      OnlyInLeft  entry ->
        Left $ "Package with no upload log " ++ display (fst entry)
      OnlyInRight entry -> mergePkgs merged remaining
--        Left $ "Upload log for non-existant package " ++ show entry

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
