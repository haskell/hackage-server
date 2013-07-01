{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Distribution.Server.Features.DownloadCount.State (
    -- * In-memory statistics
    InMemStats(..)
  , initInMemStats
    -- * On-disk statistics
  , OnDiskStats(..)
  , DownloadInfo(..)
  , PackageDownloads(..)
  , updateOnDiskStats
  , readOnDiskStats
  , writeOnDiskStats
    -- * Totals cache
  , initTotalsCache
  , updateTotalsCache
    -- * ACID stuff
  , GetInMemStats(..)
  , ReplaceInMemStats(..)
  , RecordedToday(..)
  , RegisterDownload(..)
  ) where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.MemState

import Distribution.Package
import Distribution.Version

import Data.Acid
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time.Calendar
import Data.Typeable (Typeable)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask, asks)

import qualified Data.ByteString.Lazy as BSL
import Data.Serialize (runGetLazy, runPutLazy)
import Data.SafeCopy (safeGet, safePut)
import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import System.Directory (doesFileExist)
import Control.DeepSeq (force)
import Control.Monad.Trans (MonadIO)

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

updateOnDiskStats :: FilePath -> InMemStats -> IO ()
updateOnDiskStats path inMemStats =
  readOnDiskStats path >>= writeOnDiskStats path . updateHistory inMemStats

recordedToday :: Query InMemStats Day
recordedToday = asks inMemToday

registerDownload :: PackageId -> Update InMemStats ()
registerDownload pkgId = do
  InMemStats today counts <- get
  put $ InMemStats {
      inMemToday  = today
    , inMemCounts = adjustFrom (+ 1) pkgId 0 counts
    }

getInMemStats :: Query InMemStats InMemStats
getInMemStats = ask

replaceInMemStats :: InMemStats -> Update InMemStats ()
replaceInMemStats = put

{------------------------------------------------------------------------------
  Totals cache
------------------------------------------------------------------------------}

initTotalsCache :: FilePath -> IO (MemState (Map PackageName Int))
initTotalsCache histFile  = do
  onDisk <- readOnDiskStats histFile
  let totals = Map.map (allDownloads . packageDownloads) (downloadMap onDisk)
  newMemStateWHNF (force totals) -- Ensure we don't retain the on-disk stats

updateTotalsCache :: MonadIO m => MemState (Map PackageName Int) -> PackageId -> m ()
updateTotalsCache cache pkgId =
  modifyMemState cache $ adjustFrom (+ 1) (pkgName pkgId) 0

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

adjustFrom :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustFrom func key value = Map.alter (Just . func . fromMaybe value) key

{------------------------------------------------------------------------------
  Generate acid-state infrastructure
------------------------------------------------------------------------------}
--
makeAcidic ''InMemStats [ 'registerDownload
                        , 'getInMemStats
                        , 'replaceInMemStats
                        , 'recordedToday
                        ]
