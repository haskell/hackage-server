module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.DownloadCount
import           Distribution.Server.Features.PackageInfoJSON.State
                                                ( getVersionsFor )
import           Distribution.Server.Features.Upload
import           Distribution.Server.Users.Group
                                                ( queryUserGroups
                                                , size
                                                )
import           Distribution.Types.Version

import           Data.Maybe                     ( isNothing )

data Scorer = Scorer
  { total :: Double
  , score :: Double
  }

instance Num Scorer where
  Scorer a b + Scorer c d = Scorer (a + c) (b + d)

rankPackageIO download upload p = maintNum
 where
  -- Number of maintainers
  maintNum :: IO Double
  maintNum = do
    maint <- queryUserGroups [maintainersGroup upload pkgNm]
    return . fromInteger . toInteger $ size maint
  versionsPkg :: IO Double
  versionsPkg = getVersionsFor pkgNm >>= return length
  pkgNm :: PackageName
  pkgNm = pkgName $ package p

rankPackagePure p =
  reverseDeps
    + usageTrend
    + docScore
    + reverseDeps
 where
  reverseDeps        = 1
  dependencies       = allBuildDepends p
  usageTrend         = 1
  docScore           = 1
  testsBench = (bool2Double . hasTests) p + (bool2Double . hasBenchmarks) p
  isApp              = (isNothing . library) p && (not . null . executables) p
  bool2Double :: Bool -> Double
  bool2Double true  = 1
  bool2Double false = 0

rankPackage
  :: DownloadFeature -> UploadFeature -> PackageDescription -> IO Double
rankPackage download upload p =
  rankPackageIO download upload p >>= (\x -> return $ x + rankPackagePure p)


