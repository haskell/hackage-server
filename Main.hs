module Main (main) where

import Distribution.Package (PackageIdentifier(..),packageVersion)
import Distribution.Text    (display, simpleParse)
import HAppS.Server
import HAppS.State

import Distribution.Server.State
import Distribution.Server.Caches
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Server.IndexUtils as PackageIndex (read)
import Distribution.Server.Types (PkgInfo(..))

import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages

import System.Environment
import System.IO (hFlush, stdout)
import Control.Exception
import Data.Maybe; import Data.Version
import Control.Monad
import Control.Monad.Trans
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative

import Unpack (unpackPackage)
import qualified Distribution.Server.BlobStorage as Blob

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

hackageEntryPoint :: Proxy PackagesState
hackageEntryPoint = Proxy

main :: IO ()
main = do
  putStr "hackage-server initialising..."
  hFlush stdout
  bracket (startSystemState hackageEntryPoint) shutdownSystem $ \_ctl -> do
          cacheThread
          args <- getArgs -- FIXME: use GetOpt
          forM_ args $ \pkgFile ->
              do pkgIndex <- either fail return
                           . PackageIndex.read PkgInfo
                         =<< BS.Lazy.readFile pkgFile
                 update $ BulkImport (PackageIndex.allPackages pkgIndex)
          putStr " ready.\n"
          hFlush stdout
          simpleHTTP nullConf { port = 5000 } impl


handlePackageById :: PackageIdentifier -> [ServerPart Response]
handlePackageById pkgid | pkgVersion pkgid == Version [] [] =
  [ method GET $ do index <- packageList <$> query GetPackagesState
                    ok $ case PackageIndex.lookupPackageName index (pkgName pkgid) of
                      []   -> toResponse "No such package"
                      pkgs -> toResponse (Pages.packagePage pkg pkgs)
                        where pkg = maximumBy (comparing packageVersion) pkgs
  ]

handlePackageById pkgid =
  [ method GET $ do index <- packageList <$> query GetPackagesState
                    ok $ case PackageIndex.lookupPackageId index pkgid of
                           Nothing -> toResponse "No such package"
                           Just pkg -> toResponse (Pages.packagePage pkg pkgs)
                             where pkgs = PackageIndex.lookupPackageName index (pkgName pkgid)
  , dir "cabal"
    [ method GET $ do
         index <- packageList <$> query GetPackagesState
         ok $ case PackageIndex.lookupPackageId index pkgid of
           Nothing -> toResponse "No such package" --FIXME: 404
           Just pkg -> toResponse (CabalFile (pkgData pkg))
--  , method PUT $ do ...
    ]
  ]

downloadPackageById :: PackageIdentifier -> [ServerPart Response]
downloadPackageById pkgid =
    [ anyRequest $ do index <- packageList <$> query GetPackagesState
                      blobId <- undefined
                      store <- liftIO $ Blob.open "packages"
                      file <- liftIO $ Blob.fetch store blobId
                      ok $ toResponse $ Tarball file
    ]

newtype Tarball = Tarball BS.Lazy.ByteString

instance ToMessage Tarball where
    toContentType _ = BS.pack "application/gzip"
    toMessage (Tarball bs) = bs

newtype CabalFile = CabalFile BS.Lazy.ByteString

instance ToMessage CabalFile where
    toContentType _ = BS.pack "text/plain"
    toMessage (CabalFile bs) = bs

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

impl =
  [ dir "packages" [ path $ handlePackageById
                   , dir "download"
                     [ path $ downloadPackageById ]
                   , method GET $ do
                       liftIO fetchPackagesPage
                   ]
--  , dir "test"     [ support [("text/plain", ok $ toResponse "plain" )
--                             ,("text/html", ok $ toResponse "html" )] ]
  , dir "upload" [ withDataFn (lookInput "upload") $ \input ->
                       [ anyRequest $
                         do ret <- liftIO $ unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input)
                            case ret of
                              Left err -> ok $ toResponse $ err
                              Right (pkgDesc, warns) ->
                                  do store <- liftIO $ Blob.open "packages"
                                     blobId <- liftIO $ Blob.add store (inputValue input)
                                     ok $ toResponse "Package valid"
                       ]
                 , fileServe [] "upload.html"
                 ]
  , dir "00-index.tar.gz" [ method GET $ do tarball <- liftIO $ fetchIndexTarball
                                            ok $ toResponse $ Tarball tarball ]
  , fileServe ["hackage.html"] "static"
  ]

