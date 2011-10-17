{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}

module Distribution.Server.Packages.Downloads where

import Distribution.Server.Framework.Instances ()
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
import Control.DeepSeq

-----------------------------------------
-- DownloadCounts is where the download records are converted to an historical
-- format at leisure
data DownloadCounts = DownloadCounts {
    totalDownloads :: Int,
    downloadMap :: Map PackageName DownloadInfo
} deriving (Show, Typeable)
emptyDownloadCounts :: DownloadCounts
emptyDownloadCounts = DownloadCounts 0 Map.empty

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

packageDowns :: DownloadInfo -> Int
packageDowns = allDownloads . packageDownloads

lookupPackageDowns :: DownloadCounts -> PackageName -> Int
lookupPackageDowns dcs pkgname = maybe 0 packageDowns $ Map.lookup pkgname (downloadMap dcs)

incrementCounts :: Day -> PackageName -> Version -> Int -> DownloadCounts -> DownloadCounts
incrementCounts day pkgname version count (DownloadCounts total perPackage) =
    DownloadCounts
      (total + count)
      (adjustFrom (incrementInfo day version count) pkgname emptyDownloadInfo perPackage)

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
replacePackageDownloads :: DownloadCounts -> Update DownloadCounts ()
replacePackageDownloads = put

registerDownload :: Day -> PackageId -> Int -> Update DownloadCounts (Int, Int)
registerDownload day pkgid count = do
    dc <- get
    let pkgname = packageName pkgid
        dc' = incrementCounts day pkgname (packageVersion pkgid) count dc
    put dc'
    return (lookupPackageDowns dc pkgname, lookupPackageDowns dc' pkgname)

getDownloadCounts :: Query DownloadCounts DownloadCounts
getDownloadCounts = ask

getDownloadInfo :: PackageName -> Query DownloadCounts DownloadInfo
getDownloadInfo pkgname = asks (Map.findWithDefault emptyDownloadInfo pkgname . downloadMap)

--------------------------------------------------------------------------------

$(deriveSafeCopy 0 'base ''DownloadCounts)
$(deriveSafeCopy 0 'base ''DownloadInfo)
$(deriveSafeCopy 0 'base ''PackageDownloads)

instance NFData PackageDownloads where
    rnf (PackageDownloads a b) = rnf a `seq` rnf b
instance NFData DownloadInfo where
    rnf (DownloadInfo a b c) = rnf a `seq` rnf b `seq` rnf c
instance NFData DownloadCounts where
    rnf (DownloadCounts a b) = rnf a `seq` rnf b

initialDownloadCounts :: DownloadCounts
initialDownloadCounts = emptyDownloadCounts

$(makeAcidic ''DownloadCounts ['replacePackageDownloads
                              ,'registerDownload
                              ,'getDownloadCounts
                              ,'getDownloadInfo
                              ])

