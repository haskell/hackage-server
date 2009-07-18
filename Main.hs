module Main (main) where

import qualified Distribution.Server
import Distribution.Server (Config(..)) -- serverTxControl))

import Distribution.Text
         ( display )

import Happstack.State.Control
         ( waitForTermination ) -- , createCheckpoint )
import Control.Concurrent
         ( forkIO )

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception.Extensible
         ( handleJust, ErrorCall(ErrorCall) )
import System.IO
         ( stdout, hFlush )
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Data.List
         ( sort, intersperse )
import Data.Maybe
         ( fromMaybe )
import Control.Monad
         ( unless )
import qualified Data.ByteString.Lazy.Char8 as BS

import Paths_hackage_server (version)

-- | Handle the command line args and hand off to "Distribution.Server"
--
main :: IO ()
main = topHandler $ do
  opts <- getOpts

  maybeImports <- checkImportOpts
    (optImportIndex   opts) (optImportLog      opts)
    (optImportArchive opts) (optImportHtPasswd opts)
    (optImportAdmins opts)

  defaults <- Distribution.Server.defaultConfig

  port <- checkPortOpt defaults (optPort opts)
  let hostname  = fromMaybe (confHostName  defaults) (optHost      opts)
      stateDir  = fromMaybe (confStateDir  defaults) (optStateDir  opts)
      staticDir = fromMaybe (confStaticDir defaults) (optStaticDir opts)

      config = defaults {
        confHostName  = hostname,
        confPortNum   = port,
        confStateDir  = stateDir,
        confStaticDir = staticDir
      }

  info "initialising..."
  server <- Distribution.Server.initialise config

  -- only process optInitialize if not importing
  case maybeImports of
    Just imports -> do
      info "importing..."
      doBulkImport server imports

    Nothing ->
      if optInitialize opts
        then do
          info "creating initial state..."
          Distribution.Server.initState server
        else return ()

  info $ "ready, serving on '" ++ hostname ++ "' port " ++ show port
  forkIO $ Distribution.Server.run server

  waitForTermination
  --info $ "committing checkpoint"
  --createCheckpoint (serverTxControl server)

  where
    checkPortOpt defaults Nothing    = return (confPortNum defaults)
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

    checkImportOpts Nothing Nothing Nothing Nothing Nothing= return Nothing
    checkImportOpts _ _ _ Nothing Just{} =
        fail "Currently cannot import administrators witout users"
    checkImportOpts (Just indexFileName) (Just logFileName)
                    archiveFile htpasswdFile adminsFile = do
      indexFile <- BS.readFile indexFileName
      logFile   <-    readFile logFileName
      tarballs  <- maybe (return Nothing) (fmap Just . BS.readFile) archiveFile
      htpasswd  <- maybe (return Nothing) (fmap Just . readFile) htpasswdFile
      admins    <- maybe (return Nothing) (fmap Just . readFile) adminsFile
      return (Just (indexFile, logFile, tarballs, htpasswd, admins))

    checkImportOpts Nothing Nothing (Just _) _ _ =
      fail "Currently an archive file is only imported along with an index"
    checkImportOpts Nothing Nothing _ (Just _) _ =
      fail "Currently an htpasswd file is only imported along with an index"
    checkImportOpts _ _ _ _ _ =
      fail "A package index and log file must be supplied together."

    doBulkImport server (indexFile, logFile, tarballs, htpasswd, admins) = do
      badLogEntries <- Distribution.Server.bulkImport server
                         indexFile logFile tarballs htpasswd admins
      unless (null badLogEntries) $ putStr $
           "Warning: Upload log entries for non-existant packages:\n"
        ++ unlines (map display (sort badLogEntries))

topHandler :: IO a -> IO a
topHandler =
  handleJust (\err -> case err of
                        ErrorCall e -> Just e
                        _           -> Nothing)
             die

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
    optStaticDir     :: Maybe FilePath,
    optImportIndex   :: Maybe FilePath,
    optImportLog     :: Maybe FilePath,
    optImportArchive :: Maybe FilePath,
    optImportHtPasswd:: Maybe FilePath,
    optImportAdmins  :: Maybe FilePath,
    optInitialize    :: Bool,
    optVersion       :: Bool,
    optHelp          :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    optPort          = Nothing,
    optHost          = Nothing,
    optStateDir      = Nothing,
    optStaticDir     = Nothing,
    optImportIndex   = Nothing,
    optImportLog     = Nothing,
    optImportArchive = Nothing,
    optImportHtPasswd= Nothing,
    optImportAdmins  = Nothing,
    optInitialize    = False,
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
  , Option [] ["initialize"]
      (NoArg (\opts -> opts { optInitialize = True }))
      "Initialize the server state to a useful default"
  , Option [] ["port"]
      (ReqArg (\port opts -> opts { optPort = Just port }) "PORT")
      "Port number to serve on (default 5000)"
  , Option [] ["host"]
      (ReqArg (\host opts -> opts { optHost = Just host }) "NAME")
      "Server's host name (defaults to machine name)"
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optStateDir = Just file }) "DIR")
      "Directory in which to store the persistent state of the server"
  , Option [] ["static-dir"]
      (ReqArg (\file opts -> opts { optStaticDir = Just file }) "DIR")
      "Directory in which to find the html and other static files"
  , Option [] ["import-index"]
      (ReqArg (\file opts -> opts { optImportIndex = Just file }) "TARBALL")
      "Import an existing hackage index file (00-index.tar.gz)"
  , Option [] ["import-log"]
      (ReqArg (\file opts -> opts { optImportLog = Just file }) "LOG")
      "Import an existing hackage upload log file"
  , Option [] ["import-archive"]
      (ReqArg (\file opts -> opts { optImportArchive = Just file }) "LOG")
      "Import an existing hackage package tarball archive file (archive.tar)"
  , Option [] ["import-accounts"]
      (ReqArg (\file opts -> opts { optImportHtPasswd = Just file }) "HTPASSWD")
      "Import an existing apache 'htpasswd' user account database file"
  , Option [] ["import-admins"]
      (ReqArg (\file opts -> opts { optImportAdmins = Just file}) "ADMINS")
      "Import a text file containing a list a users which should be administrators"
  ]
