module Distribution.Server.Features.Distro.Backup (
    dumpBackup,
    restoreBackup,

    distroUsersToExport,
    distroUsersToCSV,
    distrosToExport,
    distroToCSV
  ) where

import qualified Distribution.Server.Features.Distro.Distributions as Distros
import Distribution.Server.Features.Distro.Distributions (DistroName, Distributions(..), DistroVersions(..), DistroPackageInfo(..))
import Distribution.Server.Features.Distro.State
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Text
import Text.CSV (CSV, Record)

import Data.Version
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import System.FilePath (takeExtension)


dumpBackup  :: Distros -> [BackupEntry]
dumpBackup allDist =
    let distros  = distDistros allDist
        versions = distVersions allDist
    in distroUsersToExport distros:distrosToExport distros versions

restoreBackup :: RestoreBackup Distros
restoreBackup =
  updateDistros Distros.emptyDistributions Distros.emptyDistroVersions Map.empty

updateDistros :: Distributions -> DistroVersions -> Map DistroName UserIdSet -> RestoreBackup Distros
updateDistros distros versions maintainers = RestoreBackup {
    restoreEntry = \entry ->
      case entry of
        BackupByteString ["packages", distro] bs | takeExtension distro == ".csv" -> do
          csv <- importCSV distro bs
          (distros', versions') <- importDistro csv distros versions
          return (updateDistros distros' versions' maintainers)
        BackupByteString ["maintainers.csv"] bs -> do
          csv <- importCSV "maintainers.csv" bs
          maintainers' <- importMaintainers csv maintainers
          return (updateDistros distros versions maintainers')
        _ ->
          return (updateDistros distros versions maintainers)
  , restoreFinalize = do
      let distros' = foldl' (\dists (name, group) -> Distros.modifyDistroMaintainers name (const group) dists) distros (Map.toList maintainers)
      return (Distros distros' versions)
  }

importDistro :: CSV -> Distributions -> DistroVersions -> Restore (Distributions, DistroVersions)
importDistro csv dists = \versions -> do
    let [[distroStr]] = take 1 $ drop 1 csv --no bounds checking..
    distro    <- parseText "distribution name" distroStr
    dists'    <- addDistribution distro dists
    versions' <- concatM (map (fromRecord distro) (drop 3 csv)) versions
    return (dists', versions')
  where
    fromRecord :: DistroName -> Record -> DistroVersions -> Restore DistroVersions
    fromRecord distro [packageStr, versionStr, uri] versions = do
        package <- parseText "package name" packageStr
        version <- parseText "version" versionStr
        return (Distros.addPackage distro package (DistroPackageInfo version uri) versions)
    fromRecord _ x _ = fail $ "Invalid distribution record " ++ show x

addDistribution :: DistroName -> Distributions -> Restore Distributions
addDistribution distro dists = do
  case Distros.addDistro distro dists of
    Just dists' -> return dists'
    Nothing     -> fail $ "Could not add distro: " ++ display distro

importMaintainers :: CSV -> Map DistroName UserIdSet -> Restore (Map DistroName UserIdSet)
importMaintainers = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Map DistroName UserIdSet -> Restore (Map DistroName UserIdSet)
    fromRecord (distroStr:idStr) maintainers = do
        distro <- parseText "distribution name" distroStr
        uids <- mapM (parseText "user id") idStr
        return (Map.insert distro (UserIdSet.fromList uids) maintainers)
    fromRecord x _ = fail $ "Invalid distro maintainer record: " ++ show x

--------------------------------------------------------------------------
distroUsersToExport :: Distributions -> BackupEntry
distroUsersToExport distros =
    csvToBackup ["maintainers.csv"] (distroUsersToCSV assocUsers)
  where
    assocUsers = map (\(name, uidset) -> (name, UserIdSet.toList uidset))
               . Map.toList
               $ Distros.nameMap distros

distroUsersToCSV :: [(DistroName, [UserId])] -> CSV
distroUsersToCSV users =
    [showVersion distrosCSVVer]
  : distrosCSVKey
  : map (\(name, uids) -> display name : map display uids) users
  where
    distrosCSVKey = ["distro", "maintainers"]
    distrosCSVVer = Version [0,1] ["unstable"]

distrosToExport :: Distributions -> DistroVersions -> [BackupEntry]
distrosToExport dists distInfo = map distroEntry (Distros.enumerate dists)
  where distroEntry distro = csvToBackup ["packages", display distro ++ ".csv"] (distroToCSV distro distInfo)

distroToCSV :: DistroName -> DistroVersions -> CSV
distroToCSV distro distInfo
    = let stats = Distros.distroStatus distro distInfo
      in ([showVersion distrosCSVVer]:) $
         ([display distro]:) $
         (distrosCSVKey:) $
         flip map stats . uncurry $
           \name (DistroPackageInfo version url) ->
               [display name, display version, url]
  where
    distrosCSVKey = ["package", "version", "url"]
    distrosCSVVer = Version [0,1] ["unstable"]

