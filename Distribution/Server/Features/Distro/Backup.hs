module Distribution.Server.Features.Distro.Backup (
    dumpBackup,
    restoreBackup,

    distroUsersToExport,
    distroUsersToCSV,
    distrosToExport,
    distroToCSV
  ) where

import Data.Acid
import qualified Distribution.Server.Features.Distro.Distributions as Distros
import Distribution.Server.Features.Distro.Distributions (DistroName, Distributions(..), DistroVersions(..), DistroPackageInfo(..))
import Distribution.Server.Features.Distro.State
import Distribution.Server.Users.Group (UserList(..))
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text
import Data.Version
import Text.CSV (CSV)

import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import Data.Monoid (mempty)
import Control.Arrow (second)
import System.FilePath (takeExtension)

dumpBackup  :: AcidState Distros -> IO [BackupEntry]
dumpBackup distrosState = do
    allDist <- query distrosState GetDistributions
    let distros  = distDistros allDist
        versions = distVersions allDist
    return $ distroUsersToExport distros:distrosToExport distros versions

restoreBackup :: AcidState Distros -> RestoreBackup
restoreBackup distrosState = updateDistros distrosState Distros.emptyDistributions Distros.emptyDistroVersions Map.empty

updateDistros :: AcidState Distros -> Distributions -> DistroVersions -> Map DistroName UserList -> RestoreBackup
updateDistros distrosState distros versions maintainers = fix $ \restorer -> RestoreBackup
      { restoreEntry = \(path, bs) -> do
            case path of
              ["package", distro] | takeExtension distro == ".csv" -> do
                res <- runImport (distros, versions) (importDistro distro bs)
                case res of
                    Right (distros', versions') -> return . Right $
                        updateDistros distrosState distros' versions' maintainers
                    Left bad -> return (Left bad)
              ["maintainers.csv"] -> do
                res <- runImport maintainers (importMaintainers bs)
                case res of
                    Right maintainers' -> return . Right $
                        updateDistros distrosState distros versions maintainers'
                    Left bad -> return (Left bad)
              _ -> return . Right $ restorer
      , restoreFinalize = do
            let distros' = foldl' (\dists (name, group) -> Distros.modifyDistroMaintainers name (const group) dists) distros (Map.toList maintainers)
            return . Right $ finalizeDistros distrosState distros' versions
      , restoreComplete = return ()
      }

finalizeDistros :: AcidState Distros -> Distributions -> DistroVersions -> RestoreBackup
finalizeDistros distrosState distros versions = mempty
  { restoreComplete = update distrosState $ ReplaceDistributions distros versions
  }

importMaintainers :: ByteString -> Import (Map DistroName UserList) ()
importMaintainers contents = importCSV "maintainers.csv" contents $ \csv -> do
    mapM_ fromRecord (drop 2 csv)
  where
    fromRecord (distroStr:idStr) = do
        distro <- parseText "distribution name" distroStr
        ids <- mapM (parseRead "user id") idStr
        modify $ Map.insert distro (UserList $ IntSet.fromList ids)
    fromRecord x = fail $ "Invalid distro maintainer record: " ++ show x

importDistro :: String -> ByteString -> Import (Distributions, DistroVersions) ()
importDistro filename contents = importCSV filename contents $ \csv -> do
    let [[distroStr]] = take 1 $ drop 1 csv --no bounds checking..
    distro <- parseText "distribution name" distroStr
    addDistribution distro
    mapM_ (fromRecord distro) (drop 3 csv)
 where
    fromRecord distro [packageStr, versionStr, uri] = do
        package <- parseText "package name" packageStr
        version <- parseText "version" versionStr
        addDistroPackage distro package $ DistroPackageInfo version uri
    fromRecord _ x = fail $ "Invalid distribution record in " ++ filename ++ ": " ++ show x

addDistribution :: DistroName -> Import (Distributions, DistroVersions) ()
addDistribution distro = do
    (dists, versions) <- get
    case Distros.addDistro distro dists of
        Just dists' -> put (dists', versions)
        Nothing     -> fail $ "Could not add distro: " ++ display distro

addDistroPackage :: DistroName -> PackageName -> DistroPackageInfo -> Import (Distributions, DistroVersions) ()
addDistroPackage distro package info =
    modify $ \dists -> second (Distros.addPackage distro package info) dists

--------------------------------------------------------------------------
distroUsersToExport :: Distributions -> BackupEntry
distroUsersToExport distros = csvToBackup ["maintainers.csv"] (distroUsersToCSV assocUsers)
  where assocUsers = map (\(name, UserList ul) -> (name, IntSet.toList ul)) . Map.toList $ Distros.nameMap distros

distroUsersToCSV :: [(DistroName, [Int])] -> CSV
distroUsersToCSV users = [showVersion distrosCSVVer]:distrosCSVKey:map (\(name, ids) -> display name:map show ids) users
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
               [display name, showVersion version, url]
  where
    distrosCSVKey = ["package", "version", "url"]
    distrosCSVVer = Version [0,1] ["unstable"]

