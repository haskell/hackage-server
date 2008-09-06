module Main (main) where

import qualified Distribution.Server (main, Config(..), version)
import qualified Distribution.Server.BlobStorage as BlobStorage (open)
import qualified Distribution.Server.BulkImport as BulkImport

import Distribution.Text
         ( display )

import System.Environment
         ( getArgs )
import System.Exit
         ( exitWith, ExitCode(..) )
import System.FilePath
         ( (</>) )
import System.Directory
         ( createDirectoryIfMissing )
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Network.BSD
         ( getHostName )
import Data.List
         ( sort, intersperse )
import Data.Maybe
         ( fromMaybe )
import Control.Monad
         ( unless )
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

-- | Handle the command line args and hand off to "Distribution.Server"
--
main :: IO ()
main = do
  opts <- getOpts

  let stateDir = fromMaybe "state" (optStateDir opts)
  createDirectoryIfMissing False stateDir
  store <- BlobStorage.open (stateDir </> "blobs")

  imports <- case (optImportIndex opts, optImportLog opts) of
    (Nothing, Nothing) -> return []
    (Just indexFileName, Just logFileName) -> do
      indexFile <- BS.Lazy.readFile indexFileName
      logFile   <-         readFile logFileName
      tarballs  <- case optImportArchive opts of
        Nothing          -> return []
        Just archiveFile -> BulkImport.importTarballs store
                        =<< BS.Lazy.readFile archiveFile
      (pkgsInfo, badlog) <- either die return
        (BulkImport.importPkgInfo indexFile logFile tarballs)
      unless (null badlog) $ putStr $
           "Warning: Upload log entries for non-existant packages:\n"
        ++ unlines (map display (sort badlog))
      return pkgsInfo
    _ -> die "A package index and log file must be supplied together."

  port <- case optPort opts of
    Nothing    -> return 5000
    Just str   -> case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> die $ "bad port number " ++ show str
  hostname <- maybe getHostName return (optHost opts)

  let config = Distribution.Server.Config {
        Distribution.Server.hostName  = hostname,
        Distribution.Server.portNum   = port,
        Distribution.Server.stateDir  = stateDir,
        Distribution.Server.blobStore = store
      }
   in Distribution.Server.main config imports

die :: String -> IO a
die msg = putStrLn msg >> exitWith (ExitFailure 1)

-- GetOpt

data Options = Options {
    optPort          :: Maybe String,
    optHost          :: Maybe String,
    optStateDir      :: Maybe FilePath,
    optImportIndex   :: Maybe FilePath,
    optImportLog     :: Maybe FilePath,
    optImportArchive :: Maybe FilePath,
    optVersion       :: Bool,
    optHelp          :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    optPort          = Nothing,
    optHost          = Nothing,
    optStateDir      = Nothing,
    optImportIndex   = Nothing,
    optImportLog     = Nothing,
    optImportArchive = Nothing,
    optVersion       = False,
    optHelp          = False
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
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optStateDir = Just file }) "DIR")
      "Directory in which to store the persistent state of the server"
  , Option [] ["import-index"]
      (ReqArg (\file opts -> opts { optImportIndex = Just file }) "TARBALL")
      "Import an existing hackage index file (00-index.tar.gz)"
  , Option [] ["import-log"]
      (ReqArg (\file opts -> opts { optImportLog = Just file }) "LOG")
      "Import an existing hackage upload log file"
  , Option [] ["import-archive"]
      (ReqArg (\file opts -> opts { optImportArchive = Just file }) "LOG")
      "Import an existing hackage package tarball archive file (archive.tar)"
  ]
