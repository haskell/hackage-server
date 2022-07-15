module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Core
import           Distribution.Server.Features.DownloadCount
import           Distribution.Server.Features.DownloadCount.State
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.Upload
import           Distribution.Server.Framework  ( ServerPartE )
import           Distribution.Server.Packages.Types
import           Distribution.Server.Users.Group
                                                ( queryUserGroups
                                                , size
                                                )
import           Distribution.Types.Version
import Distribution.Server.Util.CountingMap (cmFind)

import           Control.Monad.IO.Class         ( liftIO )
import           Data.List                      ( sort
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( max
                                                , min
                                                )
import           Data.Time.Clock                ( UTCTime(..)
                                                , diffUTCTime
                                                , getCurrentTime
                                                , nominalDay
                                                )
import           GHC.Float                      ( int2Double )

data Scorer = Scorer
  { maximum :: Double
  , score   :: Double
  }

add (Scorer a b) (Scorer c d) = Scorer (a + c) (b + d)

total (Scorer a b) = a / b

freshnessScore :: [Version] -> UTCTime -> Bool -> IO Double
freshnessScore [] _ app = return 0
freshnessScore (x : xs) lastUpd app =
  daysPastExpiration
    >>= (\dExp -> return $ max 0 $ (decayDays - dExp) / decayDays)
 where
  versionLatest = versionNumbers x
  isNightly     = case major versionLatest of
    0 -> True
    _ -> False
  daysPastExpiration =
    age >>= (\a -> return $ max 0 a - expectedUpdateInterval)
  expectedUpdateInterval =
    int2Double (min (versionStabilityInterval versionLatest) $ length (x : xs))
      / (if isNightly then 4 else 1)
  versionStabilityInterval v | patches v > 3 && major v > 0 = 700
                             | patches v > 3                = 450
                             | patches v > 0                = 300
                             | major v > 0                  = 200
                             | minor v > 3                  = 140
                             | otherwise                    = 80
  age =
    getCurrentTime
      >>= (\x ->
            return
              $ fromRational
              $ toRational
              $ diffUTCTime x lastUpd
              / fromRational (toRational nominalDay)
          )
  -- expected_update_interval/2 + if cr.is_nightly { 30 } else if is_app_only {300} else {200};
  decayDays =
    expectedUpdateInterval
      / 2
      + (if isNightly then 30 else (if app then 300 else 200))
  major (x : xs) = x
  major _        = 0
  minor (x : y : xs) = y
  minor _            = 0
  patches (x : y : xs) = sum xs
  patches _            = 0

rankPackageIO core versions download upload p = liftIO maintNum
 where
  pkgNm :: PackageName
  pkgNm = pkgName $ package p
  isApp        = (isNothing . library) p && (not . null . executables) p
  -- Number of maintainers
  maintNum :: IO Double
  maintNum = do
    maint <- queryUserGroups [maintainersGroup upload pkgNm]
    return . int2Double $ size maint
  info         = lookupPackageName core pkgNm
  descriptions = do
    infPkg <- info
    return (pkgDesc <$> infPkg)
  downloadScore :: IO Scorer
  downloadScore = recentPackageDownloads download >>=return.calcDownScore.(cmFind pkgNm)
  calcDownScore i = Scorer 5 $ (logBase 2 (int2Double$max 0 (i-100) + 100) - 6.6) / (if isApp then 5 else 6)
  versionList =
    do
        sortBy (flip compare)
      .   map (pkgVersion . package . packageDescription)
      <$> descriptions

  partVer :: ServerPartE ([Version], [Version], [Version])
  partVer =
    versionList
      >>= (\y ->
            liftIO
              $   queryGetPreferredInfo versions pkgNm
              >>= (\x -> return $ partitionVersions x y)
          )
  lastUploads = do
    infPkg <- info
    return $ sortBy (flip compare) $ fst . pkgOriginalUploadInfo <$> infPkg

rankPackagePure p = reverseDeps + usageTrend + docScore + reverseDeps
 where
  reverseDeps  = 1
  dependencies = allBuildDepends p
  usageTrend   = 1
  docScore     = 1
  testsBench   = (bool2Double . hasTests) p + (bool2Double . hasBenchmarks) p
  isApp        = (isNothing . library) p && (not . null . executables) p

bool2Double :: Bool -> Double
bool2Double true  = 1
bool2Double false = 0

rankPackage
  :: CoreResource
  -> VersionsFeature
  -> DownloadFeature
  -> UploadFeature
  -> PackageDescription
  -> ServerPartE Double
rankPackage core versions download upload p =
  rankPackageIO core versions download upload p
    >>= (\x -> return $ x + rankPackagePure p)


