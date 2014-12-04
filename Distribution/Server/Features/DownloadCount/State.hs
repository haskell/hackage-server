{-# LANGUAGE TemplateHaskell, StandaloneDeriving, GeneralizedNewtypeDeriving,
             DeriveDataTypeable, TypeFamilies, FlexibleInstances,
             MultiParamTypeClasses, BangPatterns #-}
module Distribution.Server.Features.DownloadCount.State where

import Data.Time.Calendar (Day(..))
import Data.Version (Version)
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Control.Arrow (first)
import Control.Monad (liftM)
import Data.List (foldl', groupBy)
import Data.Function (on)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, put)
import qualified Data.Map.Lazy as Map
import System.FilePath ((</>))
import System.Directory (
    getDirectoryContents
  , createDirectoryIfMissing
  )
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BSL
import System.IO (withFile, IOMode (..), hPutStr)
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.CSV (printCSV)
import Control.Exception (evaluate)

import Data.Acid (Update, Query, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy, safeGet, safePut)
import Data.Serialize.Get (runGetLazy)
import Data.Serialize.Put (runPutLazy)

import Distribution.Package (
    PackageId
  , PackageName
  , packageName
  , packageVersion
  )
import Distribution.Text (simpleParse, display)
import Distribution.Simple.Utils (writeFileAtomic)

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.CountingMap

{------------------------------------------------------------------------------
  Data types
------------------------------------------------------------------------------}

data InMemStats = InMemStats {
    inMemToday  :: !Day
  , inMemCounts :: !(SimpleCountingMap PackageId)
  }
  deriving (Show, Eq, Typeable)

newtype OnDiskStats = OnDiskStats {
    onDiskStats :: NestedCountingMap PackageName OnDiskPerPkg
  }
  deriving (Show, Eq, MemSize)

instance CountingMap (PackageName, (Day, Version)) OnDiskStats where
  cmEmpty                            = OnDiskStats $ cmEmpty
  cmTotal  (OnDiskStats ncm)         = cmTotal ncm
  cmInsert kl n (OnDiskStats ncm)    = OnDiskStats $ cmInsert kl n ncm
  cmFind   k (OnDiskStats ncm)       = cmFind k ncm
  cmUnion    (OnDiskStats a)
             (OnDiskStats b)         = OnDiskStats (cmUnion a b)
  cmToList   (OnDiskStats ncm)       = cmToList ncm
  cmToCSV    (OnDiskStats ncm)       = cmToCSV ncm
  cmInsertRecord r (OnDiskStats ncm) = first OnDiskStats `liftM` cmInsertRecord r ncm

newtype OnDiskPerPkg = OnDiskPerPkg {
    onDiskPerPkgCounts :: NestedCountingMap Day (SimpleCountingMap Version)
  }
  deriving (Show, Eq, Ord, MemSize)

instance CountingMap (Day, Version) OnDiskPerPkg where
  cmEmpty  = OnDiskPerPkg $ cmEmpty
  cmTotal  (OnDiskPerPkg ncm) = cmTotal ncm
  cmInsert kl n (OnDiskPerPkg ncm) = OnDiskPerPkg $ cmInsert kl n ncm
  cmFind   k (OnDiskPerPkg ncm) = cmFind k ncm
  cmUnion  (OnDiskPerPkg a) (OnDiskPerPkg b) = OnDiskPerPkg (cmUnion a b)
  cmToList (OnDiskPerPkg ncm) = cmToList ncm
  cmToCSV  (OnDiskPerPkg ncm) = cmToCSV ncm
  cmInsertRecord r (OnDiskPerPkg ncm) = first OnDiskPerPkg `liftM` cmInsertRecord r ncm

newtype RecentDownloads = RecentDownloads {
    recentDownloads :: SimpleCountingMap PackageName
  }
  deriving (Show, Eq, MemSize)

instance CountingMap PackageName RecentDownloads where
  cmEmpty  = RecentDownloads $ cmEmpty
  cmTotal  (RecentDownloads ncm) = cmTotal ncm
  cmInsert kl n (RecentDownloads ncm) = RecentDownloads $ cmInsert kl n ncm
  cmFind   k (RecentDownloads ncm) = cmFind k ncm
  cmUnion  (RecentDownloads a) (RecentDownloads b) = RecentDownloads (cmUnion a b)
  cmToList (RecentDownloads ncm) = cmToList ncm
  cmToCSV  (RecentDownloads ncm) = cmToCSV ncm
  cmInsertRecord r (RecentDownloads ncm) = first RecentDownloads `liftM` cmInsertRecord r ncm

newtype TotalDownloads = TotalDownloads {
    totalDownloads :: SimpleCountingMap PackageName
  }
  deriving (Show, Eq, MemSize)

instance CountingMap PackageName TotalDownloads where
  cmEmpty  = TotalDownloads $ cmEmpty
  cmTotal  (TotalDownloads ncm) = cmTotal ncm
  cmInsert kl n (TotalDownloads ncm) = TotalDownloads $ cmInsert kl n ncm
  cmFind   k (TotalDownloads ncm) = cmFind k ncm
  cmUnion  (TotalDownloads a) (TotalDownloads b) = TotalDownloads (cmUnion a b)
  cmToList (TotalDownloads ncm) = cmToList ncm
  cmToCSV  (TotalDownloads ncm) = cmToCSV ncm
  cmInsertRecord r (TotalDownloads ncm) = first TotalDownloads `liftM` cmInsertRecord r ncm

{------------------------------------------------------------------------------
  Initial instances
------------------------------------------------------------------------------}

initInMemStats :: Day -> InMemStats
initInMemStats day = InMemStats {
    inMemToday  = day
  , inMemCounts = cmEmpty
  }

type DayRange = (Day, Day)

initRecentAndTotalDownloads :: DayRange -> OnDiskStats
                            -> (RecentDownloads, TotalDownloads)
initRecentAndTotalDownloads dayRange (OnDiskStats (NCM _ m)) =
    foldl' (\(recent, total) (pname, pstats) ->
              let !recent' = accumRecentDownloads dayRange pname pstats recent
                  !total'  = accumTotalDownloads  pname pstats total
               in (recent', total'))
           (emptyRecentDownloads, emptyTotalDownloads)
           (Map.toList m)

emptyRecentDownloads :: RecentDownloads
emptyRecentDownloads = RecentDownloads cmEmpty

accumRecentDownloads :: DayRange
                     -> PackageName -> OnDiskPerPkg
                     -> RecentDownloads -> RecentDownloads
accumRecentDownloads dayRange pkgName (OnDiskPerPkg (NCM _ perDay))
  | let rangeTotal = sum (map cmTotal (lookupRange dayRange perDay))
  , rangeTotal > 0
  = cmInsert pkgName rangeTotal

  | otherwise = id

lookupRange :: Ord k => (k,k) -> Map.Map k a -> [a]
lookupRange (l,u) m =
  let (_,ml,above)  = Map.splitLookup l m
      (middle,mu,_) = Map.splitLookup u above
   in maybe [] (\x->[x]) ml
   ++ Map.elems middle
   ++ maybe [] (\x->[x]) mu

emptyTotalDownloads :: TotalDownloads
emptyTotalDownloads = TotalDownloads cmEmpty

accumTotalDownloads :: PackageName -> OnDiskPerPkg
                    -> TotalDownloads -> TotalDownloads
accumTotalDownloads pkgName (OnDiskPerPkg perPkg) =
    cmInsert pkgName (cmTotal perPkg)

{------------------------------------------------------------------------------
  Pure updates/queries
------------------------------------------------------------------------------}

updateHistory :: InMemStats -> OnDiskStats -> OnDiskStats
updateHistory (InMemStats day perPkg) (OnDiskStats (NCM _ m)) =
    OnDiskStats (NCM 0 (Map.unionWith cmUnion m updatesMap))
  where
    updatesMap :: Map.Map PackageName OnDiskPerPkg
    updatesMap = Map.fromList
      [ (pkgname, applyUpdates pkgs)
      | pkgs <- groupBy ((==) `on` (packageName . fst))
                        (cmToList perPkg :: [(PackageId, Int)])
      , let pkgname = packageName (fst (head pkgs))
      ]

    applyUpdates :: [(PackageId, Int)] -> OnDiskPerPkg
    applyUpdates pkgs = foldr (.) id 
                          [ cmInsert (day, packageVersion pkgId) count
                          | (pkgId, count) <- pkgs ]
                          cmEmpty

{------------------------------------------------------------------------------
  MemSize
------------------------------------------------------------------------------}

instance MemSize InMemStats where
  memSize (InMemStats a b) = memSize2 a b

{------------------------------------------------------------------------------
  Serializing on-disk stats
------------------------------------------------------------------------------}

readOnDiskStats :: FilePath -> IO OnDiskStats
readOnDiskStats stateDir = do
    createDirectoryIfMissing True stateDir
    pkgStrs <- getDirectoryContents stateDir
    OnDiskStats . NCM 0 . Map.fromList <$> sequence
      [ do onDiskPerPkg <- unsafeInterleaveIO $
                             either (const cmEmpty) id
                               <$> readOnDiskPerPkg pkgFile
           return (pkgName, onDiskPerPkg)
      | Just pkgName <- map simpleParse pkgStrs
      , let pkgFile = stateDir </> display pkgName ]

readOnDiskPerPkg :: FilePath -> IO (Either String OnDiskPerPkg)
readOnDiskPerPkg pkgFile =
    withFile pkgFile ReadMode $ \h ->
      -- By evaluating the Either result from the parser we force
      -- all contents to be read
      evaluate =<< (runGetLazy safeGet <$> BSL.hGetContents h)

writeOnDiskStats :: FilePath -> OnDiskStats -> IO ()
writeOnDiskStats stateDir (OnDiskStats (NCM _ onDisk)) = do
   createDirectoryIfMissing True stateDir
   forM_ (Map.toList onDisk) $ \(pkgName, perPkg) -> do
     let pkgFile = stateDir </> display pkgName
     writeFileAtomic pkgFile $ runPutLazy (safePut perPkg)

{------------------------------------------------------------------------------
  The append-only all-time log
------------------------------------------------------------------------------}

appendToLog :: FilePath -> InMemStats -> IO ()
appendToLog stateDir (InMemStats _ inMemStats) =
  withFile (stateDir </> "log") AppendMode $ \h ->
    hPutStr h $ printCSV (cmToCSV inMemStats)

reconstructLog :: FilePath -> OnDiskStats -> IO ()
reconstructLog stateDir onDisk =
  withFile (stateDir </> "log") WriteMode $ \h ->
    hPutStr h $ printCSV (cmToCSV onDisk)

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
