{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module Distribution.Server.Features.DownloadCount.State where

import Data.Time.Calendar (Day(..))
import Data.Version (Version)
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Control.Monad (liftM)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, put, liftIO, execStateT, modify)
-- import Control.Monad.IO.Class (liftIO)
import qualified Text.PrettyPrint as PP (integer)
import qualified Data.Map as Map
import System.FilePath ((</>))
import System.Directory (getDirectoryContents)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BSL
import System.IO (withFile, IOMode (AppendMode), hPutStr)
import Text.CSV (printCSV)

import Data.Acid
import Data.SafeCopy (base, deriveSafeCopy, safeGet, safePut)
import Data.Serialize.Get (runGetLazy)
import Data.Serialize.Put (runPutLazy)

import Distribution.Package (
    PackageIdentifier(..)
  , PackageId
  , PackageName
  , packageName
  , packageVersion
  )
import Distribution.Text (Text(..), simpleParse, display)
import Distribution.Compat.ReadP (readS_to_P)

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.CountingMap

data InMemStats = InMemStats {
    inMemToday  :: Day
  , inMemCounts :: SimpleCountingMap PackageId
  }
  deriving (Show, Eq, Typeable)

newtype OnDiskStats = OnDiskStats {
    onDisk :: NestedCountingMap PackageName OnDiskPerPkg
  }
  deriving (CountingMap (PackageName, (Day, Version)), Show, Eq)

newtype OnDiskPerPkg = OnDiskPerPkg {
    onDiskPerPkgCounts :: NestedCountingMap Day (SimpleCountingMap Version)
  }
  deriving (CountingMap (Day, Version), Show, Eq)

newtype RecentDownloads = RecentDownloads {
    recentDownloads :: SimpleCountingMap PackageName
  }
  deriving (CountingMap PackageName, Show, Eq)

{------------------------------------------------------------------------------
  Initial instances
------------------------------------------------------------------------------}

initInMemStats :: Day -> InMemStats
initInMemStats day = InMemStats {
    inMemToday  = day
  , inMemCounts = cmEmpty
  }

type DayRange = [Day]

initRecentDownloads :: DayRange -> OnDiskStats -> RecentDownloads
initRecentDownloads dayRange (OnDiskStats (NCM _ perPackage)) =
    foldr (.) id (map goDay dayRange) cmEmpty
  where
    goDay :: Day -> RecentDownloads -> RecentDownloads
    goDay day = foldr (.) id $ map (goPackage day) (Map.toList perPackage)

    goPackage :: Day -> (PackageName, OnDiskPerPkg) -> RecentDownloads -> RecentDownloads
    goPackage day (pkgName, OnDiskPerPkg (NCM _ perDay)) =
      case Map.lookup day perDay of
        Nothing         -> id
        Just perVersion -> cmInsert pkgName (cmTotal perVersion)

{------------------------------------------------------------------------------
  Pure updates/queries
------------------------------------------------------------------------------}

updateHistory :: InMemStats -> OnDiskStats -> OnDiskStats
updateHistory (InMemStats day perPkg) =
    foldr (.) id $ map goPackage (cmToList perPkg)
  where
    goPackage :: (PackageId, Int) -> OnDiskStats -> OnDiskStats
    goPackage (pkgId, count) =
      cmInsert (packageName pkgId, (day, packageVersion pkgId)) count

instance Text Day where
  disp  = PP.integer . toModifiedJulianDay
  parse = ModifiedJulianDay `liftM` readS_to_P (reads :: ReadS Integer)

{------------------------------------------------------------------------------
  MemSize
------------------------------------------------------------------------------}

instance MemSize InMemStats where
  memSize (InMemStats a b) = memSize2 a b

deriving instance MemSize RecentDownloads

{------------------------------------------------------------------------------
  Serializing on-disk stats
------------------------------------------------------------------------------}

readOnDiskStats :: FilePath -> IO OnDiskStats
readOnDiskStats stateDir = flip execStateT cmEmpty $ do
    pkgs <- liftIO $ getDirectoryContents (stateDir </> "pkg")

    forM_ pkgs $ \pkgStr -> forM_ (simpleParse pkgStr) $ \pkgName -> do
      let pkgFile = stateDir </> "pkg" </> pkgStr
      mPkgStats <- liftIO $ runGetLazy safeGet <$> BSL.readFile pkgFile
      case mPkgStats of
        Left  _        -> return () -- Ignore files with errors
        Right pkgStats -> modify (aux pkgName pkgStats)
  where
    aux :: PackageName -> OnDiskPerPkg -> OnDiskStats -> OnDiskStats
    aux pkgName pkgStats (OnDiskStats (NCM total perPkg)) =
      -- This is correct only because we see each package name at most once
      OnDiskStats $ NCM (total + cmTotal pkgStats)
                        (Map.insert pkgName pkgStats perPkg)

writeOnDiskStats :: FilePath -> OnDiskStats -> IO ()
writeOnDiskStats stateDir (OnDiskStats (NCM _ onDisk)) =
   forM_ (Map.toList onDisk) $ \(pkgName, perPkg) -> do
     let pkgFile = stateDir </> "pkg" </> display pkgName
     BSL.writeFile pkgFile $ runPutLazy (safePut perPkg)

{------------------------------------------------------------------------------
  The append-only all-time log
------------------------------------------------------------------------------}

appendToLog :: FilePath -> InMemStats -> IO ()
appendToLog stateDir (InMemStats _ inMemStats) =
  withFile (stateDir </> "log") AppendMode $ \h ->
    hPutStr h $ printCSV (cmToCSV inMemStats)

{------------------------------------------------------------------------------
  ACID stuff
------------------------------------------------------------------------------}

deriveSafeCopy 0 'base ''InMemStats
deriveSafeCopy 0 'base ''OnDiskPerPkg

getInMemStats :: Query InMemStats InMemStats
getInMemStats = ask

replaceInMemStats :: InMemStats -> Update InMemStats ()
replaceInMemStats = put

recordedToday :: Query InMemStats Day
recordedToday = asks inMemToday

registerDownload :: PackageId -> Update InMemStats ()
registerDownload pkgId = do
  InMemStats day counts <- get
  put $ InMemStats day (cmInsert pkgId 1 counts)

makeAcidic ''InMemStats [ 'getInMemStats
                        , 'replaceInMemStats
                        , 'recordedToday
                        , 'registerDownload
                        ]



{-

{------------------------------------------------------------------------------
  Data types
------------------------------------------------------------------------------}

data InMemStats = InMemStats {
    -- The day we are currently keep statistics for
    inMemToday  :: Day
    -- Per-version download count for today only
  , inMemCounts :: Map PackageId Int
  } deriving (Eq, Show, Typeable)

data OnDiskStats = OnDiskStats {
    totalDownloads :: Int
  , downloadMap    :: Map PackageName DownloadInfo
  } deriving (Eq, Show, Typeable)

data DownloadInfo = DownloadInfo {
    monthDownloads   :: Map (Int, Int) PackageDownloads
  , dayDownloads     :: Map Day PackageDownloads
  , packageDownloads :: PackageDownloads
  } deriving (Eq, Show, Typeable)

data PackageDownloads = PackageDownloads {
    allDownloads     :: Int
  , versionDownloads :: Map Version Int
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''InMemStats

deriveSafeCopy 0 'base ''OnDiskStats
deriveSafeCopy 0 'base ''DownloadInfo
deriveSafeCopy 0 'base ''PackageDownloads

instance MemSize InMemStats where
    memSize (InMemStats a b) = memSize2 a b

{------------------------------------------------------------------------------
  Initial instances
------------------------------------------------------------------------------}

initOnDiskStats :: OnDiskStats
initOnDiskStats = OnDiskStats 0 Map.empty

initDownloadInfo :: DownloadInfo
initDownloadInfo = DownloadInfo Map.empty Map.empty initPackageDownloads

initPackageDownloads :: PackageDownloads
initPackageDownloads = PackageDownloads 0 Map.empty

initInMemStats :: Day -> InMemStats
initInMemStats today = InMemStats today Map.empty

{------------------------------------------------------------------------------
  Pure updates
------------------------------------------------------------------------------}

updateHistory :: InMemStats -> OnDiskStats -> OnDiskStats
updateHistory (InMemStats today counts) =
    foldr (.) id $ map (uncurry aux) (Map.toList counts)
  where
    aux :: PackageId -> Int -> OnDiskStats -> OnDiskStats
    aux pkgId = incrementCounts today (packageName pkgId) (packageVersion pkgId)

incrementCounts :: Day -> PackageName -> Version -> Int -> OnDiskStats -> OnDiskStats
incrementCounts day pkgname version count (OnDiskStats total perPackage) =
    OnDiskStats
      (total + count)
      (adjustFrom (incrementInfo day version count) pkgname initDownloadInfo perPackage)

incrementInfo :: Day -> Version -> Int -> DownloadInfo -> DownloadInfo
incrementInfo day version count (DownloadInfo perMonth perDay total) =
    DownloadInfo
      (adjustFrom (incrementPackage version count) (fromIntegral year, month) initPackageDownloads perMonth)
      (adjustFrom (incrementPackage version count) day initPackageDownloads perDay)
      (incrementPackage version count total)
  where
    (year, month, _) = toGregorian day

incrementPackage :: Version -> Int -> PackageDownloads -> PackageDownloads
incrementPackage version count (PackageDownloads total perVersion) =
    PackageDownloads (total + count) (adjustFrom (+count) version 0 perVersion)

{------------------------------------------------------------------------------
  Stateful queries/updates
------------------------------------------------------------------------------}

readOnDiskStats :: FilePath -> IO OnDiskStats
readOnDiskStats histFile = do
  histExists <- doesFileExist histFile
  mHistory   <- if histExists
                  then runGetLazy safeGet <$> BSL.readFile histFile
                  else return . Right $ initOnDiskStats
  case mHistory of
    Left  err     -> throwIO . userError $ "recordToday: " ++ err
    Right history -> return history

writeOnDiskStats :: FilePath -> OnDiskStats -> IO ()
writeOnDiskStats histFile = BSL.writeFile histFile . runPutLazy . safePut

registerDownload :: PackageId -> Update InMemStats ()
registerDownload pkgId = do
  InMemStats today counts <- get
  put $ InMemStats {
      inMemToday  = today
    , inMemCounts = adjustFrom (+ 1) pkgId 0 counts
    }

{------------------------------------------------------------------------------
  Totals cache
------------------------------------------------------------------------------}

initTotalsCache :: OnDiskStats -> IO (MemState (Map PackageName Int))
initTotalsCache onDiskStats  = do
  let totals = Map.map (allDownloads . packageDownloads) (downloadMap onDiskStats)
  newMemStateWHNF (force totals) -- Ensure we don't retain the on-disk stats

updateTotalsCache :: MonadIO m => MemState (Map PackageName Int) -> PackageId -> m ()
updateTotalsCache cache pkgId =
  modifyMemState cache $ adjustFrom (+ 1) (pkgName pkgId) 0

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  Generate acid-state infrastructure
------------------------------------------------------------------------------}
--
makeAcidic ''InMemStats [ 'registerDownload
                        , 'getInMemStats
                        , 'replaceInMemStats
                        , 'recordedToday
                        ]

-}
