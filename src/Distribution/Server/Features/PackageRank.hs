module Distribution.Server.Features.PackageRank (
    rankPackage
  ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Server.Features.Upload
import Distribution.Server.Users.UserIdSet as UserIdSet

rankPackage :: PackageDescription -> IO Double
rankPackage p=do
                maintainers <- maintNum
                return maintainers+reverseDeps+usageTrend+docScore+stability
                    +goodMetadata+weightUniqueDeps+activelyMaintained
    where   reverseDeps=1
            usageTrend=1
            docScore=1
            stability=1
            maintNum :: IO Double
            maintNum=do  
                        maintSet<-queryUserGroup$maintainersGroupDescription pkgNm
                        return fromInteger.UserIdSet$size maintSet
            goodMetadata=1
            weightUniqueDeps=1
            activelyMaintained=1
            pkgNm :: PackageName
            pkgNm=pkgName$package p
