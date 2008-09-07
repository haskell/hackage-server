module Main (main) where

import qualified Distribution.Server
import Distribution.Server (Config(..))

import Distribution.Text
         ( display )

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( handleJust, errorCalls, userErrors )
import System.IO
         ( stdout, hFlush )
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Data.List
         ( sort, intersperse )
import Data.Maybe
         ( fromMaybe )
import Control.Monad
         ( unless, MonadPlus(mplus) )
import qualified Data.ByteString.Lazy.Char8 as BS

import Paths_hackage_server (version)

-- | Handle the command line args and hand off to "Distribution.Server"
--
main :: IO ()
main = topHandler $ do
  opts <- getOpts

  maybeImports <- checkImportOpts (optImportIndex opts)
                                  (optImportLog opts)
                                  (optImportArchive opts)

  defaults <- Distribution.Server.defaultConfig

  port <- checkPortOpt defaults (optPort opts)
  let hostname = fromMaybe (confHostName defaults) (optHost opts)
      stateDir = fromMaybe (confStateDir defaults) (optStateDir opts)

      config = defaults {
        confHostName = hostname,
        confPortNum  = port,
        confStateDir = stateDir
      }

  info "initialising..."
  server <- Distribution.Server.initialise config

  case maybeImports of
    Nothing -> return ()
    Just imports -> do
      info "importing..."
      doBulkImport server imports

  info $ "ready, serving on '" ++ hostname ++ "' port " ++ show port
  Distribution.Server.run server

  where
    checkPortOpt defaults Nothing    = return (confPortNum defaults)
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

    checkImportOpts Nothing Nothing _ = return Nothing
    checkImportOpts (Just indexFileName) (Just logFileName) archiveFile = do
      indexFile <- BS.readFile indexFileName
      logFile   <-    readFile logFileName
      tarballs  <- maybe (return Nothing) (fmap Just . BS.readFile) archiveFile
      return (Just (indexFile, logFile, tarballs))

    checkImportOpts _ _ _ =
      fail "A package index and log file must be supplied together."

    doBulkImport server (indexFile, logFile, tarballs) = do
      badLogEntries <- Distribution.Server.bulkImport server
                         indexFile logFile tarballs
      unless (null badLogEntries) $ putStr $
           "Warning: Upload log entries for non-existant packages:\n"
        ++ unlines (map display (sort badLogEntries))

topHandler :: IO a -> IO a
topHandler = handleJust (\e -> errorCalls e `mplus` userErrors e) die

die :: String -> IO a
die msg = do
  info msg
  exitWith (ExitFailure 1)

info :: String -> IO ()
info msg = do
  pname <- getProgName
  putStrLn (pname ++ ": " ++ msg)
  hFlush stdout

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
    printErrors errs = fail (concat (intersperse "\n" errs))
    printUsage = do
      putStrLn (usageInfo usageHeader optionDescriptions)
      exitWith ExitSuccess
    usageHeader  = "hackage web server\n\nusage: hackage-server [OPTION ...]"
    printVersion = do
      putStrLn $ "hackage-server version " ++ display version
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
