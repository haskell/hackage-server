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
module Distribution.Server.LegacyImport.BulkImport (
  importPkgIndex,
  importUploadLog,
  importTarballs,
  importUsers,
  mergePkgInfo,
  mergeMaintainers
  ) where

import qualified Distribution.Server.Util.Index as PackageIndex (read)
import qualified Distribution.Server.Users.Users as Users
import           Distribution.Server.Users.Users  (Users)
--import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Codec.Archive.Tar.Entry as Tar (Entry(..), entryPath, EntryContent(..))
import qualified Distribution.Server.LegacyImport.UploadLog as UploadLog
import qualified Distribution.Server.LegacyImport.HtPasswdDb as HtPasswdDb
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Packages.Types (CabalFileText(..), cabalFileString, PkgInfo(..), PkgTarball(..), pkgUploadUser)

import Distribution.Package
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg)
import Distribution.Text (display)
import Distribution.Server.Util.Merge

import Data.Maybe
import System.FilePath (takeExtension)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Codec.Compression.GZip as GZip
import Control.Monad.Error () --intance Monad (Either String)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sortBy)
import Data.Ord (comparing)

import Prelude hiding (read)

mergeMaintainers :: [PkgInfo] -> Map PackageName Group.UserList
mergeMaintainers infos = Map.map Group.fromList $ Map.fromListWith (++) $ map assocUpload infos
  where assocUpload info = (packageName info, [pkgUploadUser info])


newPkgInfo :: PackageIdentifier
           -> (FilePath, CabalFileText)
           -> UploadLog.Entry -> [UploadLog.Entry]
           -> Users
           -> Either String (PkgInfo, Users)
newPkgInfo pkgid (cabalFilePath, cabalFile) (UploadLog.Entry time user _) _ users =
  case parse cabalFile of
      ParseFailed err -> fail $ cabalFilePath
                             ++ maybe "" (\n -> ":" ++ show n) lineno
                             ++ ": " ++ message
        where (lineno, message) = locatedErrorMsg err

      ParseOk _ pkg   -> return (PkgInfo {
        pkgInfoId     = pkgid,
        pkgDesc       = pkg,
        pkgData       = cabalFile,
        pkgTarball    = [],
        pkgUploadData = (time, uid),
        pkgDataOld    = []
      }, fromMaybe users musers)

  where parse = parsePackageDescription . cabalFileString
        (musers, uid) = Users.requireName user users

importPkgIndex :: ByteString -> Either String [(PackageIdentifier, Tar.Entry)]
importPkgIndex = PackageIndex.read (,) . GZip.decompress

importUploadLog :: String -> Either String [UploadLog.Entry]
importUploadLog = UploadLog.read

-- | Actually write the tarballs to disk and return an association of
-- 'PackageIdentifier' to the 'BlobStorage.BlobId' of the tarball added to the
-- 'BlobStorage' area.
--
importTarballs :: BlobStorage
               -> Maybe ByteString
               -> IO [(PackageIdentifier, PkgTarball)]
importTarballs _      Nothing           = return []
importTarballs store (Just archiveFile) =
  case PackageIndex.read (,) archiveFile of
    Left  problem  -> fail problem
    Right tarballs -> sequence
      [ do blobId <- BlobStorage.add store fileContent
           blobIdDecompressed <- BlobStorage.add store (GZip.decompress fileContent)
           return (pkgid, PkgTarball { pkgTarballGz = blobId,
                                       pkgTarballNoGz = blobIdDecompressed })
      | (pkgid, entry@Tar.Entry {
                  Tar.entryContent = Tar.NormalFile fileContent _
                }) <- tarballs
      , takeExtension (Tar.entryPath entry) == ".gz" ] --FIXME: .tar.gz

-- | The active users are simply all those listed in the current htpasswd file.
--
importUsers :: Maybe String -> Either String Users
importUsers Nothing             = Right Users.empty
importUsers (Just htpasswdFile) = importUsers' Users.empty
                              =<< HtPasswdDb.parse htpasswdFile
  where
    importUsers' users [] = Right users
    importUsers' _users ((_userName, _userAuth):_rest) =
      error "TODO: need to be able to add old users in special mode with old auth info"
{-
      case Users.add userName (Users.UserAuth userAuth BasicAuth) users of
        Nothing                -> Left (alreadyPresent userName)
        Just (users', _userId) -> importUsers' users' rest

    alreadyPresent name = "User " ++ show name ++ " is already present"
-}

-- | Merge all the package and user info together
--
mergePkgInfo :: [(PackageIdentifier, Tar.Entry)]
             -> [UploadLog.Entry]
             -> [(PackageIdentifier, PkgTarball)]
             -> Users
             -> Either String ([PkgInfo], Users, [UploadLog.Entry])
mergePkgInfo pkgDescs logEntries tarballInfo users = do
  let logEntries'       = UploadLog.group logEntries
  (pkgs, extraLogEntries, users') <- mergeIndexWithUploadLog pkgDescs logEntries' users
  pkgs' <- mergeTarballs tarballInfo pkgs
  return (pkgs', users', extraLogEntries)

-- | Merge the package index meta data with the upload log to make initial
--   'PkgInfo' records, but without tarballs.
--
-- Also returns any upload log entries with no corresponding package info.
-- This happens for packages which got uploaded but subsequently deleted.
--
mergeIndexWithUploadLog :: [(PackageIdentifier, Tar.Entry)]
                        -> [(UploadLog.Entry, [UploadLog.Entry])]
                        -> Users
                        -> Either String ([PkgInfo], [UploadLog.Entry], Users)
mergeIndexWithUploadLog pkgs entries usersInit =
  mergePkgs usersInit [] [] $
    mergeBy comparingPackageId
      (sortBy (comparing fst)
              [ (pkgid, (path, CabalFileText cabalFile))
              | (pkgid, entry@Tar.Entry {
                          Tar.entryContent = Tar.NormalFile cabalFile _
                        }) <- pkgs
              , let path = Tar.entryPath entry
              , takeExtension path == ".cabal" ])
      (sortBy (comparing (\(UploadLog.Entry _ _ pkgid, _) -> pkgid)) entries)
  where
    comparingPackageId (pkgid, _) (UploadLog.Entry _ _ pkgid', _) =
      compare pkgid pkgid'

    mergePkgs users merged nonexistant []  = Right (reverse merged, nonexistant, users)
    mergePkgs users merged nonexistant (next:remaining) = case next of
      InBoth (pkgid, cabalFile) (logEntry, logEntries) ->
        case newPkgInfo pkgid cabalFile logEntry logEntries users of
          Left problem       -> Left problem
          Right (ok, users') -> mergePkgs users' (ok:merged) nonexistant remaining
      OnlyInLeft (pkgid, _) ->
        Left $ "Package with no upload log " ++ display pkgid
      OnlyInRight (entry,_) -> mergePkgs users merged (entry:nonexistant) remaining

-- | The tarball info with the existing 'PkgInfo' records.
--
-- It's an errror to find additional tarballs, but we don't mind at the moment
-- if not all packages have a tarball, as that makes testing easier.
--
mergeTarballs :: [(PackageIdentifier, PkgTarball)]
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
      InBoth (_, tarball) pkginfo -> mergePkgs (pkginfo':merged) remaining
         where pkginfo' = pkginfo { pkgTarball = (tarball, pkgUploadData pkginfo):pkgTarball pkginfo }
      OnlyInLeft (pkgid, _)          -> Left missing
         where missing = "Package tarball missing metadata " ++ display pkgid
      OnlyInRight            pkginfo -> mergePkgs (pkginfo:merged) remaining
