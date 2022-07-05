module Distribution.Server.Features.PackageRank (
    rankPackage
  ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Server.Users.Group
import Distribution.Server.Features.Upload

rankPackageIO upload p=maintNum
    where
            maintNum :: IO Double
            maintNum=do  
                        maint<-queryUserGroups$[maintainersGroup upload pkgNm]
                        return.fromInteger.toInteger$size maint
            pkgNm :: PackageName
            pkgNm=pkgName$package p
rankPackagePure p=reverseDeps+usageTrend+docScore+stabilityScore
                    +goodMetadata+weightUniqueDeps+activelyMaintained
    where   reverseDeps=1
            usageTrend=1
            docScore=1
            stabilityScore=1
            goodMetadata=1
            weightUniqueDeps=1
            activelyMaintained=1

rankPackage :: UploadFeature -> PackageDescription -> IO Double
rankPackage upload p=rankPackageIO upload p>>=(\x->return$x + rankPackagePure p)
