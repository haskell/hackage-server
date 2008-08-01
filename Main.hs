module Main (main) where

import Distribution.Package (PackageIdentifier(..), packageName, packageVersion)
import Distribution.Text    (simpleParse, display)
import HAppS.Server hiding (port)
import qualified HAppS.Server
import HAppS.State

import Distribution.Server.State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Types (PkgInfo(..))

import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.Recent  as Pages
import qualified Distribution.Server.IndexUtils as PackageIndex (write)
import qualified Distribution.Server.BulkImport as BulkImport (read)

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception
import Data.Maybe; import Data.Version
import Control.Monad
import Control.Monad.Trans
import Data.List (maximumBy, intersperse, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Text.RSS
import Data.Time.Clock
import Network.BSD
         ( getHostName )
import Network.URI
         ( URIAuth(URIAuth) )

import Unpack (unpackPackage)
import qualified Distribution.Server.BlobStorage as Blob

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import qualified Codec.Compression.GZip as GZip

import qualified Paths_hackage_server as Distribution.Server (version)

import Prelude hiding (log)

hackageEntryPoint :: Proxy PackagesState
hackageEntryPoint = Proxy

main :: IO ()
main = do
  opts <- getOpts
  imports <- case (optImportIndex opts, optImportLog opts) of
    (Nothing, Nothing) -> return Nothing
    (Just indexFileName, Just logFileName) -> do
      indexFile <- BS.Lazy.readFile indexFileName
      logFile   <-         readFile logFileName
      (pkgsInfo, badlog) <- either die return (BulkImport.read indexFile logFile)
      unless (null badlog) $ putStr $
           "Warning: Upload log entries for non-existant packages:\n"
        ++ unlines (map display (sort badlog))
      return (Just pkgsInfo)
    _ -> die "A package index and log file must be supplied together."
  port <- case optPort opts of
    Nothing    -> return 5000
    Just str   -> case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> die $ "bad port number " ++ show str
  hostname <- maybe getHostName return (optHost opts)
  let host = URIAuth "" hostname (if port == 80 then "" else ':' : show port)

  log "hackage-server: initialising..."
  bracket (startSystemState hackageEntryPoint) shutdownSystem $ \_ctl -> do
    cache <- Cache.new =<< stateToCache host =<< query GetPackagesState

    case imports of
     Nothing -> return ()
     Just pkgsInfo -> do
       update $ BulkImport pkgsInfo
       Cache.put cache =<< stateToCache host =<< query GetPackagesState

    log (" ready. Serving on " ++ hostname ++" port " ++ show port ++ "\n")
    simpleHTTP nullConf { HAppS.Server.port = port } (impl cache)

log :: String -> IO ()
log msg = putStr msg   >> hFlush stdout

die :: String -> IO a
die msg = putStrLn msg >> exitWith (ExitFailure 1)

stateToCache :: URIAuth -> PackagesState -> IO Cache.State
stateToCache host state = getCurrentTime >>= \now -> return
  Cache.State {
    Cache.packagesPage  = toResponse (Pages.packageIndex index),
    Cache.indexTarball  = GZip.compress (PackageIndex.write index),
    Cache.recentChanges = toResponse (Pages.recentPage recentChanges),
    Cache.packagesFeed  = toResponse (Pages.recentFeed host now recentChanges)
  }
  where index = packageList state
        recentChanges = reverse $ sortBy (comparing pkgUploadTime) (PackageIndex.allPackages index)

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

  , dir "tarball"
    [ method GET $
        withPackage $ \pkg _pkgs -> do
          case pkgTarball pkg of
            Nothing -> notFound $ toResponse "No tarball available"
            Just blobId -> do
              store <- liftIO $ Blob.open "packages"
              file <- liftIO $ Blob.fetch store blobId
              ok $ toResponse $ Tarball file
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

newtype Tarball = Tarball BS.Lazy.ByteString

instance ToMessage Tarball where
    toContentType _ = BS.pack "application/gzip"
    toMessage (Tarball bs) = bs

newtype CabalFile = CabalFile BS.Lazy.ByteString

instance ToMessage CabalFile where
    toContentType _ = BS.pack "text/plain"
    toMessage (CabalFile bs) = bs

instance ToMessage RSS where
    toContentType _ = BS.pack "application/rss+xml"
    toMessage rss = BS.Lazy.pack $ showXML $ rssToXML rss

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

basicUsers :: Map.Map String String
basicUsers = Map.fromList [("Lemmih","kodeord")]

impl :: Cache.Cache -> [ServerPartT IO Response]
impl cache =
  [ dir "packages" [ path $ handlePackageById
                   , dir "00-index.tar.gz"
                     [ method GET $
                         movePermanently "/00-index.tar.gz" (toResponse "")
                     ]
                   , method GET $ do
                       cacheState <- Cache.get cache
                       ok $ Cache.packagesPage cacheState
                   ]
  , dir "recent.rss"
      [ method GET $ ok . Cache.packagesFeed =<< Cache.get cache ]
  , dir "recent.html"
      [ method GET $ ok . Cache.recentChanges =<< Cache.get cache ]
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

-- GetOpt

data Options = Options {
    optPort        :: Maybe String,
    optHost        :: Maybe String,
    optImportIndex :: Maybe FilePath,
    optImportLog   :: Maybe FilePath,
    optVersion     :: Bool,
    optHelp        :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    optPort        = Nothing,
    optHost        = Nothing,
    optImportIndex = Nothing,
    optImportLog   = Nothing,
    optVersion     = False,
    optHelp        = False
  }

getOpts :: IO Options
getOpts = do
  args <- getArgs
  case accumOpts $ getOpt RequireOrder optionDescriptions args of
    (opts, _,    _)
      | optHelp opts    -> printUsage
    (opts, [],  [])
      | optVersion opts -> printVersion
      | otherwise       -> return opts
    (_,     _, errs)    -> printErrors errs
  where
    printErrors errs = die (concat (intersperse "\n" errs))
    printUsage = do
      putStrLn (usageInfo usageHeader optionDescriptions)
      exitWith ExitSuccess
    usageHeader  = "hackage web server\n\nusage: hackage-server [OPTION ...]"
    printVersion = do
      putStrLn $ "hackage-server version "
              ++ display Distribution.Server.version
      exitWith ExitSuccess
    accumOpts (opts, args, errs) =
      (foldr (flip (.)) id opts defaultOptions, args, errs)

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help text"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Print version information"
  , Option [] ["port"]
      (ReqArg (\port opts -> opts { optPort = Just port }) "PORT")
      "Port number to serve on (default 5000)"
  , Option [] ["host"]
      (ReqArg (\host opts -> opts { optHost = Just host }) "NAME")
      "Server's host name (defaults to machine name)"
  , Option [] ["import-index"]
      (ReqArg (\file opts -> opts { optImportIndex = Just file }) "TARBALL")
      "Import an existing hackage index file (00-index.tar.gz)"
  , Option [] ["import-log"]
      (ReqArg (\file opts -> opts { optImportLog = Just file }) "LOG")
      "Import an existing hackage upload log file"
  ]
