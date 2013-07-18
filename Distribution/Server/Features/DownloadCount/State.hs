{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module Distribution.Server.Features.DownloadCount.State where

import Data.Time.Calendar (Day(..))
import Data.Version (Version)
import Data.Typeable (Typeable)
import Data.Foldable (forM_)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, put, liftIO, execStateT, modify)
-- import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BSL
import System.IO (withFile, IOMode (AppendMode, ReadMode), hPutStr)
import Text.CSV (printCSV)
import Control.Exception (evaluate)

import Data.Acid
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

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.CountingMap

{------------------------------------------------------------------------------
  Data types
------------------------------------------------------------------------------}

data InMemStats = InMemStats {
    inMemToday  :: Day
  , inMemCounts :: SimpleCountingMap PackageId
  }
  deriving (Show, Eq, Typeable)

newtype OnDiskStats = OnDiskStats {
    onDiskStats :: NestedCountingMap PackageName OnDiskPerPkg
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
    pkgs <- liftIO $ do createDirectoryIfMissing True stateDir
                        getDirectoryContents stateDir

    forM_ pkgs $ \pkgStr -> forM_ (simpleParse pkgStr) $ \pkgName -> do
      let pkgFile = stateDir </> pkgStr
      mPkgStats <- liftIO $ withFile pkgFile ReadMode $ \h ->
        -- By evaluating the Either result from the parser we force
        -- all contents to be read
        evaluate =<< (runGetLazy safeGet <$> BSL.hGetContents h)
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
writeOnDiskStats stateDir (OnDiskStats (NCM _ onDisk)) = do
   createDirectoryIfMissing True stateDir
   forM_ (Map.toList onDisk) $ \(pkgName, perPkg) -> do
     let pkgFile = stateDir </> display pkgName
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
