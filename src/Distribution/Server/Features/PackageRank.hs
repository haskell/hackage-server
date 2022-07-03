module Distribution.Server.Features.PackageRank (
    rankPackage
  ) where

import Distribution.Package

rankPackage :: (Package a) => a -> IO Double
rankPackage p=return$ reverseDeps+usageTrend+docScore+stability
                +authNum+goodMetadata+weightUniqueDeps+activelyMaintained
    where   reverseDeps=1
            usageTrend=1
            docScore=1
            stability=1
            authNum=1
            goodMetadata=1
            weightUniqueDeps=1
            activelyMaintained=1
