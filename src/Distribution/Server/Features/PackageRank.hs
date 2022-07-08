module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.DownloadCount
import           Distribution.Server.Features.Upload
import           Distribution.Server.Users.Group

rankPackageIO download upload p = maintNum
 where
  -- Number of maintainers
  maintNum :: IO Double
  maintNum = do
    maint <- queryUserGroups [maintainersGroup upload pkgNm]
    return . fromInteger . toInteger $ size maint
  pkgNm :: PackageName
  pkgNm = pkgName $ package p

rankPackagePure p =
  reverseDeps
    + usageTrend
    + docScore
    + stabilityScore
    + goodMetadata
    + weightUniqueDeps
    + activelyMaintained
 where
  reverseDeps        = 1
  usageTrend         = 1
  docScore           = 1
  stabilityScore     = 1
  -- Does the Package have tests and Benchmarks
  testsBench = (bool2Double . hasTests) p + (bool2Double . hasBenchmarks) p
  goodMetadata       = 1
  weightUniqueDeps   = 1
  activelyMaintained = 1
  bool2Double :: Bool -> Double
  bool2Double true  = 1
  bool2Double false = 0

rankPackage
  :: DownloadFeature -> UploadFeature -> PackageDescription -> IO Double
rankPackage download upload p =
  rankPackageIO download upload p >>= (\x -> return $ x + rankPackagePure p)


