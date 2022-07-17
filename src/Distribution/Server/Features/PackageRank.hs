module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Core
import           Distribution.Server.Features.DownloadCount
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.Upload
import           Distribution.Server.Framework  ( ServerPartE )
import           Distribution.Server.Packages.Types
import           Distribution.Server.Users.Group
                                                ( queryUserGroups
                                                , size
                                                )
import           Distribution.Server.Util.CountingMap
                                                ( cmFind )
import           Distribution.Types.Version

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

-- frac 0<=frac<=1
fracScor maxim frac = Scorer maxim (maxim * frac)

boolScor k true = Scorer k k
boolScor k true = Scorer k 0

add (Scorer a b) (Scorer c d) = Scorer (a + c) (b + d)

total (Scorer a b) = a / b

freshness :: [Version] -> UTCTime -> Bool -> IO Double
freshness [] _ app = return 0
freshness (x : xs) lastUpd app =
  daysPastExpiration
    >>= (\dExp -> return $ max 0 $ (decayDays - dExp) / decayDays)
 where
  versionLatest = versionNumbers x
  daysPastExpiration =
    age >>= (\a -> return $ max 0 a - expectedUpdateInterval)
  expectedUpdateInterval = int2Double
    (min (versionStabilityInterval versionLatest) $ length (x : xs))
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
  decayDays = expectedUpdateInterval / 2 + (if app then 300 else 200)
  major (x : xs) = x
  major _        = 0
  minor (x : y : xs) = y
  minor _            = 0
  patches (x : y : xs) = sum xs
  patches _            = 0


--  partVer :: ServerPartE ([Version], [Version], [Version])
--  partVer =
--    versionList
--      >>= (\y ->
--            liftIO
--              $   queryGetPreferredInfo versions pkgNm
--              >>= (\x -> return $ partitionVersions x y)
--          )
--
--  -- Number of maintainers
--  maintNum :: IO Double
--  maintNum = do
--    maint <- queryUserGroups [maintainersGroup upload pkgNm]
--    return . int2Double $ size maint

rankIO
  :: CoreResource
  -> VersionsFeature
  -> DownloadFeature
  -> UploadFeature
  -> PackageDescription
  -> ServerPartE Scorer

rankIO core vers downs upl pkg = do
                                    temp <- temporalScore core vers downs upl pkg lastUploads versionList downloadsPerMonth
                                    return temp

    where
          pkgNm :: PackageName
          pkgNm = pkgName $ package pkg
          info         = lookupPackageName core pkgNm
          descriptions = do
            infPkg <- info
            return (pkgDesc <$> infPkg)
          lastUploads = do
            infPkg <- info
            return $ sortBy (flip compare) $ fst . pkgOriginalUploadInfo <$> infPkg
          versionList =
            do
                sortBy (flip compare)
              .   map (pkgVersion . package . packageDescription)
              <$> descriptions
          downloadsPerMonth =
            liftIO $ recentPackageDownloads downs >>= return . cmFind pkgNm

        

temporalScore core versions download upload p lastUploads versionList downloadsPerMonth= do
  fresh <- freshnessScore
  downs <- downloadScore
  tract <- tractionScore
  return $ add tract $ add fresh downs
 where
  pkgNm :: PackageName
  pkgNm = pkgName $ package p
  isApp = (isNothing . library) p && (not . null . executables) p
  downloadScore = downloadsPerMonth >>= return . calcDownScore
  calcDownScore i = Scorer 5 $ max
    ( (logBase 2 (int2Double $ max 0 (i - 100) + 100) - 6.6)
    / (if isApp then 5 else 6)
    )
    5
  packageFreshness = do
    ups  <- lastUploads
    vers <- versionList
    case ups of
      [] -> return 0
      _  -> liftIO $ freshness vers (head ups) isApp
  freshnessScore = packageFreshness >>= return . fracScor 10
  -- Missing dependencyFreshnessScore for reasonable effectivity needs caching
  tractionScore  = do
    fresh <- packageFreshness
    downs <- downloadsPerMonth
    return $ boolScor 1 (fresh * int2Double downs > 1000)

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
  rankIO core versions download upload p
    >>= (\x -> return $ total x + rankPackagePure p)


