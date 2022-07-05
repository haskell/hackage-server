module Distribution.Server.Features.PackageRank (
    rankPackage
  ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Server.Users.Group
import Distribution.Server.Features.Upload

rankPackage :: UploadFeature -> PackageDescription -> IO Double
rankPackage upload p=do
                maintainers <- maintNum
                return$maintainers+reverseDeps+usageTrend+docScore+stabilityScore
                    +goodMetadata+weightUniqueDeps+activelyMaintained
    where   reverseDeps=1
            usageTrend=1
            docScore=1
            stabilityScore=1
            maintNum :: IO Double
            maintNum=do  
                        maint<-queryUserGroups$[maintainersGroup upload pkgNm]
                        return.fromInteger.toInteger$size maint
            goodMetadata=1
            weightUniqueDeps=1
            activelyMaintained=1
            pkgNm :: PackageName
            pkgNm=pkgName$package p
