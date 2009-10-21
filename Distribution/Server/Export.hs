{-
Create a tarball with the following structure:


backup-timestamp/
  users/
   authinfo
    user: UserName
     active: True/False
     basic-auth:######
    <white-space>
   permissions
    Group: GroupName
     memebers: User1,User2
    <whitespace>
  package/
    package-id/
     package.cabal
     upload.tar.gz
     documentation.tar.gz -- optional
     uploads.txt
      On: ISO-time
       user: UserName
     build-reports/
       <n>/
        buildreport
        buildlog        -- optional
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Export
    ( export
    ) where

import Distribution.Simple.Utils (toUTF8)
import qualified Data.ByteString.Lazy.Char8 as BS8

import Data.Maybe (maybe)

import Text.CSV

import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Distribution.Server.Export.FlatFiles

import qualified Distribution.Server.Util.BlobStorage as Blob

import Distribution.Server.BuildReport.BuildReports (BuildReports)
import qualified Distribution.Server.BuildReport.BuildReports as Build
import qualified Distribution.Server.BuildReport.BuildReport as Build

import Distribution.Server.Users.Types
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Users.Permissions (Permissions, GroupName(..))
import qualified Distribution.Server.Users.Permissions as Permissions

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import Distribution.Simple.PackageIndex
import Distribution.Package

import Distribution.Server.Util.BlobStorage

import Distribution.Text

import qualified Data.ByteString.Lazy as BSL
import Codec.Compression.GZip (compress)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import System.FilePath
import System.Locale
import Data.Time

export :: Users
       -> Permissions.Permissions
       -> PackageIndex PkgInfo
       -> Documentation
       -> BuildReports
       -> BlobStorage
       -> IO BSL.ByteString
export users permissions pkgs docs reports storage
    = (compress . Tar.write) `fmap` tarball
 where
   tarball :: IO [Tar.Entry]
       = mkExportEntries users permissions pkgs docs reports storage

mkExportEntries :: Users
                -> Permissions.Permissions
                -> PackageIndex PkgInfo
                -> Documentation
                -> BuildReports
                -> BlobStorage
                -> IO [Tar.Entry]
mkExportEntries users perms pkgs docs reports storage
    = do

  baseDir <- mkBaseDir `fmap` getCurrentTime
  packageEntries <- mkPackageEntries baseDir pkgs docs reports storage
  
  return $ concat
    [ mkUserEntries baseDir users
    , mkPermsEntries baseDir perms
    , packageEntries
    ]

mkBaseDir :: UTCTime -> FilePath
mkBaseDir time = "export-" ++ formatTime defaultTimeLocale (iso8601DateFormat Nothing) time

mkPackageEntries baseDir pkgs docs reports storage
    = let pkgList = allPackages pkgs
      in concatMapM (mkPackageEntry baseDir storage docs reports) pkgList

mkPackageEntry baseDir storage docs reports pkgInfo
    = do

  -- docs and build reports each ship separate
  docsEntry <- mkDocumentationEntry baseDir pkgInfo docs storage
  buildReportEntries <- mkBuildReportEntries baseDir pkgInfo reports storage

  -- the source, uploads, and cabal files get lumped together in their
  -- own tar file
  sourceEntry <- mkSourceEntry pkgInfo storage
  let uploadsEntry = csvToEntry (uploadsToCSV pkgInfo) $
                     "uploads" <.> "csv"
      cabalEntry = mkCabalEntry pkgInfo

      pkgTar = Tar.write $ catMaybes
               [ sourceEntry
               , Just cabalEntry
               , Just uploadsEntry
               ]
      pkgEntry
          = bsToEntry pkgTar $
            pkgEntryPath baseDir pkgInfo <.> "tar"
  return $ pkgEntry : maybe [] return docsEntry ++ buildReportEntries


-- | Tar entry for the source tarball
mkSourceEntry pkgInfo storage
    = case pkgTarball pkgInfo of
        Nothing   -> return Nothing
        Just blob -> Just `fmap` (mkBlobEntry storage blob $
                     display (packageName pkgInfo) <.> "tar" <.> "gz")

-- | Tar entry for the documentation for this package
mkDocumentationEntry baseDir pkgInfo (Documentation docs) storage
    = case Map.lookup (packageId pkgInfo) docs of
        Nothing   -> return Nothing
        Just blob -> Just `fmap` (mkBlobEntry storage blob $
                     pkgEntryPath baseDir pkgInfo </> "documentation" <.> "tar" <.> "gz")

-- | Tar entries for build reports.
--
-- NOTE - there's no current way to export
-- build reports that correspond to package ids
-- that are not in the passed-in package index
mkBuildReportEntries baseDir pkgInfo reports storage
    = flip concatMapM
      (Build.lookupPackageReports reports (packageId pkgInfo))

      $ \(reportId, report) ->
          case Build.lookupBuildLog reports reportId of
            Nothing -> return [mkBuildReportEntry baseDir reportId report]
            Just (Build.BuildLog blob) -> do
              buildLogEntry <- mkBuildLogEntry baseDir reportId blob storage
              let buildReportEntry = mkBuildReportEntry baseDir reportId report
              return [buildReportEntry, buildLogEntry]

-- | Tar entry for a single build report
mkBuildReportEntry baseDir reportId report
    = bsToEntry (stringToBytes . Build.show $ report) $
      reportBasePath baseDir reportId </> "report.txt"

-- I'm not sure about the extension here
-- | Tar entry for a single build log
mkBuildLogEntry baseDir reportId blob storage
    = mkBlobEntry storage blob $
      reportBasePath baseDir reportId </> "log.txt"

reportBasePath baseDir reportId
    = baseDir </> "build-reports" </> display reportId


-- | Tar entry for the .cabal file
mkCabalEntry pkgInfo
    = bsToEntry (pkgData pkgInfo) $
      (display $ packageName pkgInfo)  <.> "cabal"


pkgEntryPath baseDir pkgInfo
    = baseDir
      </> "package"
      </> (display . packageName $ pkgInfo)
      </> (display . packageId $ pkgInfo)

pkgEntryBaseFileName baseDir pkgInfo
    = pkgEntryPath baseDir pkgInfo </> (display . packageId $ pkgInfo)


-- | Tar entry for the users db
mkUserEntries baseDir users
    = return $ csvToEntry (usersToCSV users) $
      baseDir </> "users" </> "auth" <.> "csv"

-- | Tar entry for the permissions db
mkPermsEntries baseDir perms
    = return $ csvToEntry (permsToCSV perms) $
      baseDir </> "users" </> "permissions" <.> "csv"


-- | Create a tat entry for an entry
-- in the blob store
mkBlobEntry storage blob path
    = do
  contents <- Blob.fetch storage blob
  return $ bsToEntry contents path

-- | Convert a CSV to a tar entry (UTF8)
csvToEntry csv path
    = let chunk = csvToBytes csv
      in bsToEntry chunk path

-- | Convert a ByteString to a tar entry
bsToEntry chunk path
    = case Tar.toTarPath False path of
        Right tarPath -> Tar.fileEntry tarPath chunk
        Left err -> error $ "Error in export: " ++ err

-- via UTF8 conversion.
stringToBytes = BS8.pack . toUTF8

csvToBytes = stringToBytes . printCSV

concatM :: (Monad m, Functor m) => [m [a]] -> m [a]
concatM x = concat `fmap` sequence x

concatMapM :: (Monad m, Functor m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concatM $ map f xs
