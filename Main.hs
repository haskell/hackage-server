module Main (main) where

import Distribution.Package (PackageIdentifier(..), packageName, packageVersion)
import Distribution.Text    (simpleParse)
import HAppS.Server
import HAppS.State

import Distribution.Server.State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Server.IndexUtils as PackageIndex (read)
import Distribution.Server.Types (PkgInfo(..))

import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.IndexUtils as PackageIndex (write)

import System.Environment
import System.IO (hFlush, stdout)
import Control.Exception
import Data.Maybe; import Data.Version
import Control.Monad
import Control.Monad.Trans
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

import Unpack (unpackPackage)
import qualified Distribution.Server.BlobStorage as Blob

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import qualified Codec.Compression.GZip as GZip

hackageEntryPoint :: Proxy PackagesState
hackageEntryPoint = Proxy

main :: IO ()
main = do
  putStr "hackage-server initialising..."
  hFlush stdout
  bracket (startSystemState hackageEntryPoint) shutdownSystem $ \_ctl -> do
          cache <- Cache.new . stateToCache =<< query GetPackagesState
          args <- getArgs -- FIXME: use GetOpt
          forM_ args $ \pkgFile ->
              do pkgIndex <- either fail return
                           . PackageIndex.read
                         =<< BS.Lazy.readFile pkgFile
                 update $ BulkImport (PackageIndex.allPackages pkgIndex)
                 Cache.put cache . stateToCache =<< query GetPackagesState
          putStr " ready.\n"
          hFlush stdout
          simpleHTTP nullConf { port = 5000 } (impl cache)

stateToCache :: PackagesState -> Cache.State
stateToCache state =
  Cache.State {
    Cache.packagesPage = toResponse (Pages.packageIndex index),
    Cache.indexTarball = GZip.compress (PackageIndex.write index)
  }
  where index = packageList state

handlePackageById :: PackageIdentifier -> [ServerPart Response]
handlePackageById pkgid =
  [ method GET $
      withPackage $ \pkg pkgs ->
        ok $ toResponse (Pages.packagePage pkg pkgs)

  , dir "cabal"
    [ method GET $
      withPackage $ \pkg _pkgs ->
        ok $ toResponse (CabalFile (pkgData pkg))
--  , method PUT $ do ...
    ]
  ]
  
  where
    withPackage action = do
      index <- return . packageList =<< query GetPackagesState
      case PackageIndex.lookupPackageName index (packageName pkgid) of
        []   -> notFound $ toResponse "No such package"
        pkgs  | pkgVersion pkgid == Version [] []
             -> action pkg pkgs
          where pkg = maximumBy (comparing packageVersion) pkgs
 
        pkgs -> case listToMaybe [ pkg | pkg <- pkgs, packageVersion pkg
                                               == packageVersion pkgid ] of
          Nothing  -> notFound $ toResponse "No such package version"
          Just pkg -> action pkg pkgs
{-
downloadPackageById :: PackageIdentifier -> [ServerPart Response]
downloadPackageById pkgid =
    [ anyRequest $ do index <- return . packageList =<< query GetPackagesState
                      blobId <- undefined
                      store <- liftIO $ Blob.open "packages"
                      file <- liftIO $ Blob.fetch store blobId
                      ok $ toResponse $ Tarball file
    ]
-}
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

basicUsers :: Map.Map String String
basicUsers = Map.fromList [("Lemmih","kodeord")]

impl :: Cache.Cache -> [ServerPartT IO Response]
impl cache =
  [ dir "packages" [ path $ handlePackageById
--                   , dir "download"
--                     [ path $ downloadPackageById ]
                   , method GET $ do
                       cacheState <- Cache.get cache
                       ok $ Cache.packagesPage cacheState
                   ]
  , dir "upload" [ methodSP POST $
                   basicAuth "hackage" basicUsers
                   [ withDataFn (lookInput "upload") $ \input ->
                       [ anyRequest $
                         do ret <- liftIO $ unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input)
                            case ret of
                              Left err -> ok $ toResponse $ err
                              Right (_pkgDesc, _warns) ->
                                  do store <- liftIO $ Blob.open "packages"
                                     _blobId <- liftIO $ Blob.add store (inputValue input)
                                     ok $ toResponse "Package valid"
                       ]
                   ]
                 , fileServe [] "upload.html"
                 ]
  , dir "00-index.tar.gz" [ method GET $ do cacheState <- Cache.get cache
                                            ok $ toResponse $ Tarball (Cache.indexTarball cacheState) ]
  , fileServe ["hackage.html"] "static"
  ]

