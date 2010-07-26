{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}

module Distribution.Server.Packages.Downloads where

import Distribution.Server.Instances ()
import Distribution.Package
import Distribution.Version

import qualified Happstack.State as State (Version)
import Happstack.State hiding (Version)
import Data.Time.Calendar
import Data.Typeable (Typeable)
import Data.Map (Map)
import Data.List (delete)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask, asks)

-----------------------------------------
-- DownloadCounts is where the download records are converted to an historical
-- format at leisure
data DownloadCounts = DownloadCounts {
    totalDownloads :: Int,
    -- TODO: combine downloadHistogram and downloadMap into a single PSQueue?
    downloadHistogram :: IntMap [PackageName],
    downloadMap :: Map PackageName DownloadInfo
} deriving (Show, Typeable)
emptyDownloadCounts :: DownloadCounts
emptyDownloadCounts = DownloadCounts 0 IntMap.empty Map.empty

data DownloadInfo = DownloadInfo {
    monthDownloads :: Map (Int, Int) PackageDownloads,
    dayDownloads :: Map Day PackageDownloads,
    packageDownloads :: PackageDownloads
} deriving (Show, Typeable)
emptyDownloadInfo :: DownloadInfo
emptyDownloadInfo = DownloadInfo Map.empty Map.empty emptyPackageDownloads

data PackageDownloads = PackageDownloads {
    allDownloads :: Int,
    versionDownloads :: Map Version Int
} deriving (Show, Typeable)
emptyPackageDownloads :: PackageDownloads
emptyPackageDownloads = PackageDownloads 0 Map.empty

incrementInfo :: Day -> Version -> Int -> DownloadInfo -> DownloadInfo
incrementInfo day version count (DownloadInfo perMonth perDay total) = 
    DownloadInfo
      (adjustFrom (incrementPackage version count) (fromIntegral year, month) emptyPackageDownloads perMonth)
      (adjustFrom (incrementPackage version count) day emptyPackageDownloads perDay)
      (incrementPackage version count total)
  where
    (year, month, _) = toGregorian day

incrementPackage :: Version -> Int -> PackageDownloads -> PackageDownloads
incrementPackage version count (PackageDownloads total perVersion) =
    PackageDownloads (total + count) (adjustFrom (+count) version 0 perVersion)

adjustFrom :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustFrom func key value = Map.alter (Just . func . fromMaybe value) key

----
registerDownload :: Day -> PackageId -> Int -> Update DownloadCounts ()
registerDownload day pkgid count = do
    dc <- get
    let infoMap = downloadMap dc
        info = Map.findWithDefault emptyDownloadInfo pkgname infoMap
    if count <= 0
      then put $ dc { downloadMap = Map.insert pkgname info infoMap }
      else do
        let prevCount = allDownloads . packageDownloads $ info
            info' = incrementInfo day (packageVersion pkgid) count info
            newCount  = allDownloads . packageDownloads $ info'
            histogram' = IntMap.alter putInPackage newCount
                       . IntMap.alter takeOutPackage prevCount
                       $ downloadHistogram dc
        put $ dc { downloadMap = Map.insert pkgname info' infoMap 
                 , downloadHistogram = histogram'
                 , totalDownloads = (+count) $ totalDownloads dc }
  where
    pkgname = packageName pkgid
    takeOutPackage Nothing = Nothing
    takeOutPackage (Just l) = case delete (packageName pkgid) l of
        [] -> Nothing
        l' -> Just l'
    putInPackage Nothing  = Just [pkgname]
    putInPackage (Just l) = Just (pkgname:l)

getDownloadCounts :: Query DownloadCounts DownloadCounts
getDownloadCounts = ask

getDownloadInfo pkgname = asks (Map.findWithDefault emptyDownloadInfo pkgname . downloadMap)

--------------------------------------------------------------------------------

instance State.Version DownloadCounts where mode = Versioned 0 Nothing
getDownloadInfo :: PackageName -> Query DownloadCounts DownloadInfo
$(deriveSerialize ''DownloadCounts)
instance State.Version DownloadInfo where mode = Versioned 0 Nothing
$(deriveSerialize ''DownloadInfo)
instance State.Version PackageDownloads where mode = Versioned 0 Nothing
$(deriveSerialize ''PackageDownloads)

instance Component DownloadCounts where
    type Dependencies DownloadCounts = End
    initialValue = emptyDownloadCounts

$(mkMethods ''DownloadCounts ['registerDownload
                             ,'getDownloadCounts
                             ,'getDownloadInfo
                             ])

