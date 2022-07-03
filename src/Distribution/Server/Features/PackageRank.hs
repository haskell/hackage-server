module Distribution.Server.Features.PackageRank (
    rankPackage
  ) where

import Distribution.Package
import Distribution.Server.Packages.Types

rankPackage :: (Package a) => a -> IO Double
rankPackage p=return 0
