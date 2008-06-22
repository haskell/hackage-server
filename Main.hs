module Main (main) where

import Distribution.Package (PackageIdentifier(..))
import Distribution.Text    (display, simpleParse)
import HAppS.Server

main = simpleHTTP nullConf { port = 5000 } impl


handlePackageById :: PackageIdentifier -> [ServerPart Response]
handlePackageById id = [ anyRequest $ ok $ toResponse $ "Project num " ++ display id ++ " addressed."]

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

impl = [ dir "projects"
                 [ path $ handlePackageById ]
       ]

