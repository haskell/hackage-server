module Main (main) where

import Distribution.Package (PackageIdentifier(..))
import Distribution.Text    (display, simpleParse)
import HAppS.Server
import HAppS.State

import Distribution.Server.PackagesState
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Verbosity as Verbosity (normal)
import Hackage.IndexUtils (readPackageIndex)
import Hackage.Types (PkgInfo(..))

main = do
  pkgindex <- readPackageIndex Verbosity.normal "."
  simpleHTTP nullConf { port = 5000 } (impl pkgindex)


handlePackageById :: PackageIndex PkgInfo -> PackageIdentifier -> [ServerPart Response]
handlePackageById pkgindex pkgid =
  [ anyRequest $ ok $ toResponse $
       "Package " ++ display pkgid ++ " addressed.\n"
    ++ case PackageIndex.lookupPackageId pkgindex pkgid of
         Nothing -> "No such package"
	 Just pkg -> PD.author pkg_desc
	   where pkg_desc = PD.packageDescription (pkgDesc pkg)
  ]

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

impl pkgindex =
  [ dir "packages" [ path $ handlePackageById pkgindex ]
  ]

