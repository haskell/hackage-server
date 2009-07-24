module Main (main) where

import qualified Distribution.Server
import Distribution.Server (Config(..), Server)

import Distribution.Text
         ( display )

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( handle, ErrorCall(ErrorCall), bracket )
import System.Posix.Signals as Signal
         ( installHandler, Handler(Catch), userDefinedSignal1 )
import System.IO
         ( stdout, hFlush )
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Data.List
         ( sort, intersperse )
import Data.Maybe
         ( fromMaybe, isJust )
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

  -- Do some pre-init sanity checks
  hasSavedState <- Distribution.Server.hasSavedState config
  checkAccidentalDataLoss hasSavedState maybeImports opts
  checkBlankServerState   hasSavedState maybeImports opts

  -- Startup the server, including the data store
  withServer config $ \server -> do

    -- Import data or set initial data (ie. admin user account) if requested
    handleInitialDbstate server maybeImports opts

    -- setup a Unix signal handler so we can checkpoint the server state
    withCheckpointHandler server $ do

      -- Go!
      info $ "ready, serving on '" ++ hostname ++ "' port " ++ show port
      Distribution.Server.run server

  where
    withServer :: Config -> (Server -> IO ()) -> IO ()
    withServer config = bracket initialise shutdown
      where
        initialise = do
          info "initialising..."
          Distribution.Server.initialise config

        shutdown server = do
          -- TODO: we probably do not want to write a checkpint every time,
          -- perhaps only after a certain amount of time or number of updates.
          -- info "writing checkpoint..."
          -- Distribution.Server.checkpoint server
          info "shutting down..."
          Distribution.Server.shutdown server

    -- Set a Unix signal handler for SIG USR1 to create a state checkpoint.
    -- Useage:
    -- > kill -USR1 $the_pid
    --
    withCheckpointHandler :: Server -> IO () -> IO ()
    withCheckpointHandler server action =
        bracket (setHandler handler) setHandler (\_ -> action)
      where
        handler = Signal.Catch $ do
          info "writing checkpoint..."
          Distribution.Server.checkpoint server
        setHandler h =
          Signal.installHandler Signal.userDefinedSignal1 h Nothing

    -- Option handling:
    --
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

    -- Sanity checking
    --
    checkAccidentalDataLoss hasSavedState maybeImports opts
      | (optInitialize opts || isJust maybeImports)
     && hasSavedState = die $
            "The server already has an initialised database!!\n"
         ++ "If you really *really* intend to completely reset the "
         ++ "whole database then use the additional flag "
         ++ "--obliterate-all-existing-data"
      | otherwise = return ()

    checkBlankServerState   hasSavedState maybeImports opts
      | not (optInitialize opts || isJust maybeImports)
     && not hasSavedState = die $
            "There is no existing server state.\nYou can either import "
         ++ "existing data using the various --import-* flags, or start with "
         ++ "an empty state using --initialise. Either way, we have to make "
         ++ "sure that there is at least one admin user account, otherwise "
         ++ "you'll not be able to administer your shiny new hackage server!"
      | otherwise = return ()


    -- Importing
    --
    handleInitialDbstate server _maybeImports opts | optInitialize opts = do
      info "creating initial state..."
      Distribution.Server.initState server

    handleInitialDbstate server
      (Just (indexFile, logFile, tarballs, htpasswd, admins)) _opts = do
      info "importing..."
      badLogEntries <- Distribution.Server.bulkImport server
                         indexFile logFile tarballs htpasswd admins
      unless (null badLogEntries) $ putStr $
           "Warning: Upload log entries for non-existant packages:\n"
        ++ unlines (map display (sort badLogEntries))

    handleInitialDbstate _ _ _ = return ()

topHandler :: IO a -> IO a
topHandler = handle $ \(ErrorCall msg) -> die msg

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
