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
import Data.Maybe
import Control.Monad
import Control.Monad.Trans

import Unpack
import qualified Distribution.Server.BlobStorage as Blob

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as Lazy

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

downloadPackageById pkgid =
    [ anyRequest $ do mbPkgInfo <- query $ LookupPackageId pkgid
                      blobId <- undefined
                      store <- liftIO $ Blob.open "packages"
                      file <- liftIO $ Blob.fetch store blobId
                      ok $ toResponse $ Tarball file
    ]

newtype Tarball = Tarball Lazy.ByteString

instance ToMessage Tarball where
    toContentType _ = BS.pack "application/gzip"
    toResponse (Tarball bs) = (toResponse ()) { rsBody = bs }

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

impl =
  [ dir "packages" [ path $ handlePackageById
                   , dir "download"
                     [ path $ downloadPackageById ]
                   ]
--  , dir "test"     [ support [("text/plain", ok $ toResponse "plain" )
--                             ,("text/html", ok $ toResponse "html" )] ]
  , dir "upload" [ withDataFn (lookInput "upload") $ \input ->
                       [ anyRequest $
                         do ret <- liftIO $ unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input) False
                            case ret of
                              Left err -> ok $ toResponse $ err
                              Right (pkgDesc, warns) ->
                                  do store <- liftIO $ Blob.open "packages"
                                     blobId <- liftIO $ Blob.add store (inputValue input)
                                     ok $ toResponse "Package valid"
                       ]
                 , fileServe [] "upload.html"
                 ]
  ]

