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

import System.Environment
import Control.Exception
import Data.Version
import Control.Monad

hackageEntryPoint :: Proxy PackagesState
hackageEntryPoint = Proxy


main = bracket (startSystemState hackageEntryPoint) (shutdownSystem) $ \ctl ->
       do args <- getArgs -- FIXME: use GetOpt
          forM_ args $ \pkgFile ->
              do pkgIndex <- readPackageIndex Verbosity.normal pkgFile
                 update $ BulkImport (PackageIndex.allPackages pkgIndex)
          simpleHTTP nullConf { port = 5000 } impl


handlePackageById :: PackageIdentifier -> [ServerPart Response]
handlePackageById pkgid =
  [ anyRequest $ do mbPkgInfo <- query $ LookupPackageId pkgid
                    ok $ toResponse $
                           "Package " ++ display pkgid ++ " addressed.\n"
                           ++ case mbPkgInfo of
                                Nothing -> "No such package"
	                        Just pkg -> PD.author pkg_desc
                                    where pkg_desc = PD.packageDescription (pkgDesc pkg)
  ]

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

impl =
  [ dir "packages" [ path $ handlePackageById ]
  ]

