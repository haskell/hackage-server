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
import           Distribution.Server.Framework
import           Distribution.Server.Packages.PackageIndex
                                                ( PackageIndex )
import qualified Distribution.Server.Packages.PackageIndex
                                               as PackageIndex
import           Distribution.Server.Packages.Types
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

add (Scorer a b) (Scorer c d) = Scorer (a + c) (b + d)

rankPackageIO core versions download upload p = maintNum
 where
  pkgNm :: PackageName
  pkgNm = pkgName $ package p
  -- Number of maintainers
  maintNum :: IO Double
  maintNum = do
    maint <- queryUserGroups [maintainersGroup upload pkgNm]
    return . fromInteger . toInteger $ size maint
  descriptions = do
    desc <- lookupPackageName core pkgNm
    return (pkgDesc <$> desc)
  partVer :: ServerPartE (IO ([Version], [Version], [Version]))
  partVer = do
    desc <- descriptions
    return
      $   queryGetPreferredInfo versions pkgNm
      >>= (\x -> return $ partitionVersions
            x
            (map (pkgVersion . package . packageDescription) desc)
          )



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
  -> IO Double
rankPackage core versions download upload p =
  rankPackageIO core versions download upload p
    >>= (\x -> return $ x + rankPackagePure p)


