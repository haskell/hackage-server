{-# LANGUAGE PatternGuards #-}

module Main where

import qualified Distribution.Server as Server
import Distribution.Server (ServerConfig(..), Server)

import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( wrapText )

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( bracket )
import System.Posix.Signals as Signal
         ( installHandler, Handler(Catch), userDefinedSignal1 )
import System.IO
         ( stdout, stderr, hFlush, hPutStr )
import System.IO.Error
         ( ioeGetErrorString )
import System.Directory
         ( doesDirectoryExist )
import System.Console.GetOpt
         ( OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo )
import Data.List
         ( sort )
import Data.Maybe
         ( fromMaybe, isJust, listToMaybe )
import Data.Traversable
         ( traverse )
import Control.Monad
         ( unless, when )
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BS

import Paths_hackage_server as Paths (version)

-- | Handle the command line args and hand off to "Distribution.Server"
--
main :: IO ()
main = topHandler $ getOpts >>= \options -> case options of
    -- Help, version, or error parsing options
    ExitMode str -> putStrLn str
    -- Running the server with pre-existing data
    RunMode opts -> do
        defaults <- Server.defaultServerConfig

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
        -- Be helpful to people running from the build tree
        exists <- doesDirectoryExist staticDir
        when (not exists) $
          if isJust (optStaticDir opts)
            then fail $ "The given static files directory " ++ staticDir
                  ++ " does not exist."
            else fail $ "It looks like you are running the server without installing "
                  ++ "it. That is fine but you will have to give the location of "
                  ++ "the static html files with the --static-dir flag."

        checkBlankServerState =<< Server.hasSavedState config

        withServer config $ \server -> withCheckpointHandler server $ do
            info $ "Ready, serving on '" ++ hostname ++ "' port " ++ show port
            Server.run server

    -- Initializing the server on a blank-slate        
    NewMode opts -> do
        defaults <- Server.defaultServerConfig

        let stateDir = fromMaybe (confStateDir defaults) (optNewDir opts)
            config = defaults { confStateDir  = stateDir }
            parseAdmin adminStr = case break (==':') adminStr of
                (uname, ':':pass) -> Just (uname, pass)
                _ -> Nothing

        admin <- case optNewAdmin opts of
            Nothing  -> return ("admin", "admin")
            Just str -> case parseAdmin str of
                Just arg -> return arg
                Nothing -> fail $ "Couldn't parse username:password in " ++ show str

        checkAccidentalDataLoss =<< Server.hasSavedState config

        withServer config $ \server -> do
            info "Creating initial state..."
            Server.initState server admin
            info "Done"

    RestoreMode opts -> case optRestore opts of
      Nothing -> fail "No restore tarball given"
      Just tarFile -> do
        defaults <- Server.defaultServerConfig

        let stateDir = fromMaybe (confStateDir defaults) (optRestoreDir opts)
            config = defaults { confStateDir  = stateDir }

        checkAccidentalDataLoss =<< Server.hasSavedState config

        withServer config $ \server -> do
            tar <- BS.readFile tarFile
            info "Parsing import tarball..."
            res <- Server.importServerTar server tar
            case res of
                Just err -> fail err
                _ -> info "Successly imported."

    BackupMode opts -> do
        defaults <- Server.defaultServerConfig

        let stateDir = fromMaybe (confStateDir defaults) (optBackupDir opts)
            config = defaults { confStateDir = stateDir }
            exportPath = fromMaybe "export.tar" (optBackupDir opts)

        withServer config $ \server -> do
            info "Preparing export tarball"
            tar <- Server.exportServerTar server            
            info "Saving export tarball"
            BS.writeFile exportPath tar
            info "Done"

    ConvertMode opts -> case opts of
        ConvertOpts _ _ _ Nothing Just{} _ -> fail "Cannot import administrators without users"

        ConvertOpts (Just indexFileName) (Just logFileName)
                  archiveFile htpasswdFile adminsFile exportFile -> do

            indexFile <- BS.readFile indexFileName
            logFile   <- readFile logFileName
            tarballs  <- traverse BS.readFile archiveFile
            htpasswd  <- traverse readFile htpasswdFile
            admins    <- traverse readFile adminsFile
            let exportPath = fromMaybe "export.tar" exportFile

            -- todo: get rid of using blob storage for conversion
            defaults <- Server.defaultServerConfig
            let stateDir  = confStateDir defaults ++ "/blobs"

            info "Creating export tarball..."
            (badLogEntries, bulkTar) <- Server.bulkImport stateDir indexFile logFile tarballs htpasswd admins
            BS.writeFile exportPath bulkTar
            info "Done"
            unless (null badLogEntries) $ putStr $
                "Warning: Upload log entries for non-existant packages:\n"
                ++ unlines (map display (sort badLogEntries))

        ConvertOpts Nothing Nothing Just{} _ _ _ -> fail "An archive tar should be imported along with an index tarball."
        ConvertOpts Nothing Nothing _ Just{} _ _ -> fail "An htpasswd file should be imported along with an index."
        ConvertOpts {} -> fail "A package index and log file must be supplied together."

  where
    withServer :: ServerConfig -> (Server -> IO ()) -> IO ()
    withServer config = bracket initialise shutdown
      where
        initialise = do
          info "Initializing happstack-state..."
          server <- Server.initialise config
          info "Server data loaded into memory"
          return server

        shutdown server = do
          -- TODO: we probably do not want to write a checkpint every time,
          -- perhaps only after a certain amount of time or number of updates.
          -- info "writing checkpoint..."
          -- Server.checkpoint server
          info "Shutting down..."
          Server.shutdown server

    -- Set a Unix signal handler for SIG USR1 to create a state checkpoint.
    -- Useage:
    -- > kill -USR1 $the_pid
    --
    withCheckpointHandler :: Server -> IO () -> IO ()
    withCheckpointHandler server action =
        bracket (setHandler handler) setHandler (\_ -> action)
      where
        handler = Signal.Catch $ do
          info "Writing checkpoint..."
          Server.checkpoint server
        setHandler h =
          Signal.installHandler Signal.userDefinedSignal1 h Nothing

    -- Option handling:
    --
    checkPortOpt defaults Nothing    = return (confPortNum defaults)
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

    -- Import utilities
    checkAccidentalDataLoss hasSavedState = when hasSavedState . die $
            "The server already has an initialised database!!\n"
         ++ "If you really *really* intend to completely reset the "
         ++ "whole database you should remove the state/ directory."

    checkBlankServerState  hasSavedState = when (not hasSavedState) . die $
            "There is no existing server state.\nYou can either import "
         ++ "existing data using the various import modes, or start with"
         ++ "an empty state using the new mode. Either way, we have to make "
         ++ "sure that there is at least one admin user account, otherwise "
         ++ "you'll not be able to administer your shiny new hackage server!\n"
         ++ "Use --help for more information."

topHandler :: IO a -> IO a
topHandler prog = catch prog handle
  where
    handle ioe = do
      hFlush stdout
      pname <- getProgName
      let message = wrapText (pname ++ ": " ++ ioeGetErrorString ioe)
      hPutStr stderr message
      exitWith (ExitFailure 1)

die :: String -> IO a
die msg = ioError (userError msg)

info :: String -> IO ()
info msg = do
  pname <- getProgName
  putStrLn (pname ++ ": " ++ msg)
  hFlush stdout

getOpts :: IO HackageModes
getOpts = do
    args <- getArgs
    return $ case accumOpts defaultGlobalOpts $ getOpt RequireOrder globalDescriptions args of
        -- version
        (opts, _, _) | optVersion opts -> ExitMode $ "hackage-server " ++ display version
        -- help
        (opts, others, _) | optHelp opts -> case others of
            -- help for a specific command (this is `--help command', not `command --help', currently)
            (modeStr:_) | Just mode <- lookup modeStr theModes -> ExitMode $ modeUsageInfo mode (modeHeader mode)
            -- the general help for run mode and listing of commands
            _ -> ExitMode $ usageInfo topHeader globalDescriptions ++
                            modeUsageInfo (snd $ head theModes) "\nServer run configuration:" ++ availableCommands
        -- run it - select the correct mode
        -- ignore errors for now, as they might just be arguments from other modes
        (_, others, _) -> let (mmode, args') = case others of
                                                   [] -> (fmap snd $ listToMaybe theModes, args)
                                                   (str:strs) -> (lookup str theModes, strs)
                              in case mmode of
            -- no mode under the command name found
            Nothing   -> ExitMode $ "Not a valid command. Available commands are:\n" ++ availableCommands
            -- mode found, parse the options, and run it if it works
            Just mode -> case parseHackageMode mode args' of
                Left errs -> ExitMode $ concat errs ++ "\n" ++ modeUsageInfo mode (modeHeader mode)
                Right modeOpts -> modeOpts
        -- none of the above worked.. at the moment, try to assume run mode
        -- (_, _, errs) -> ExitMode $ unlines errs ++ "Try --help."
  where
    availableCommands = "\nAvailable commands:\n" ++
        concatMap (\(name, mode) -> printf "    %-9s %s\n" name (modeOneLiner mode)) theModes ++
        "\nUse `hackage-server --help COMMAND' to see options for each mode." ++
        "\nNote: happstack-state's data lock prevents two state-accessing modes from being run simultaneously.\n"
    -- global usage header
    topHeader = "Usage: hackage-server [COMMAND] [OPTIONS...]\n" ++
                "If the command is excluded, run mode will be assumed.\n\nGlobal options:"
    -- per-mode usage header
    modeHeader mode = "usage: hackage-server " ++ modeCommandName mode ++ " [OPTIONS...]: " ++ modeOneLiner mode
    theModes = -- commands. head is the default.
      [ make $ ModeOptions "run" "Run an already-initialized Hackage server."
                    runDescriptions defaultRunOpts RunMode
      , make $ ModeOptions "new" "Initialize the server state to a useful default."
                    newDescriptions defaultNewOpts NewMode
      , make $ ModeOptions "restore" "Import server state from a backup tarball."
                    restoreDescriptions defaultRestoreOpts RestoreMode
      , make $ ModeOptions "backup" "Export a backup tarball from server state."
                    backupDescriptions defaultBackupOpts BackupMode
      , make $ ModeOptions "convert" "Convert legacy data to a newer backup tarball."
                    convertDescriptions defaultConvertOpts ConvertMode
      ]

accumOpts :: a -> ([a -> a], [String], [String]) -> (a, [String], [String])
accumOpts defaults (opts, args, errs) = (foldr (flip (.)) id opts defaults, args, errs)

make :: ModeOptions a -> (String, ModeCommand)
make (ModeOptions short long accum empty convert) = (,) short $ ModeCommand short (flip usageInfo accum) long $ \args ->
    case accumOpts empty $ getOpt RequireOrder accum args of
        (opts, [], [])  -> Right $ convert opts
        -- it would be nice to specify if --help was included without making it part
        -- of the accum list, but this seems difficult with GetOpt presently.
        -- using getOpt' instead lets other unrecognized-command errors escape
        (_, _, err) -> Left err


data ModeCommand = ModeCommand {
    modeCommandName :: String,
    modeUsageInfo :: String -> String,
    modeOneLiner  :: String,
    parseHackageMode :: [String] -> Either [String] HackageModes
}

data ModeOptions a = ModeOptions {
    modeName :: String,
    modeDesc :: String,
    accumOptions :: [OptDescr (a -> a)],
    emptyOption  :: a,
    -- this could be replaced by an explicit action
    toHackageMode :: a -> HackageModes
}

--instead of having a mode to case on, each mode could be associated with an action, like cabal does
data HackageModes = RunMode RunOpts | NewMode NewOpts
                  | RestoreMode RestoreOpts | BackupMode BackupOpts
                  | ConvertMode ConvertOpts | ExitMode String deriving (Show)


data GlobalOpts = GlobalOpts {
    optVersion       :: Bool,
    optHelp          :: Bool
} deriving (Show)
defaultGlobalOpts :: GlobalOpts
defaultGlobalOpts = GlobalOpts False False
globalDescriptions :: [OptDescr (GlobalOpts -> GlobalOpts)]
globalDescriptions =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "Show this help text"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optVersion = True }))
      "Print version information"
  ]

data RunOpts = RunOpts {
    optPort      :: Maybe String,
    optHost      :: Maybe String,
    optStateDir  :: Maybe FilePath,
    optStaticDir :: Maybe FilePath
} deriving (Show)
defaultRunOpts :: RunOpts
defaultRunOpts = RunOpts Nothing Nothing Nothing Nothing
runDescriptions :: [OptDescr (RunOpts -> RunOpts)]
runDescriptions =
  [ Option [] ["port"]
      (ReqArg (\port opts -> opts { optPort = Just port }) "PORT")
      "Port number to serve on (default 8080)"
  , Option [] ["host"]
      (ReqArg (\host opts -> opts { optHost = Just host }) "NAME")
      "Server's host name (defaults to machine name)"
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optStateDir = Just file }) "DIR")
      "Directory in which to store the persistent state of the server (default state/)"
  , Option [] ["static-dir"]
      (ReqArg (\file opts -> opts { optStaticDir = Just file }) "DIR")
      "Directory in which to find the html and other static files (default: cabal location)"
  ]

data NewOpts = NewOpts {
    optNewAdmin :: Maybe String,
    optNewDir :: Maybe FilePath
} deriving (Show)

defaultNewOpts :: NewOpts
defaultNewOpts = NewOpts Nothing Nothing

newDescriptions :: [OptDescr (NewOpts -> NewOpts)]
newDescriptions =
  [ Option [] ["admin"]
      (ReqArg (\name opts -> opts { optNewAdmin = Just name }) "NAME:PASS")
      "New server's administrator, name:password (default: admin:admin)"
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optNewDir = Just file }) "DIR")
      "Directory in which to store the persistent state of the server (default state/)"
  ]

data RestoreOpts = RestoreOpts {
    optRestore :: Maybe FilePath,
    optRestoreDir :: Maybe FilePath
} deriving (Show)

defaultRestoreOpts :: RestoreOpts
defaultRestoreOpts = RestoreOpts Nothing Nothing

restoreDescriptions :: [OptDescr (RestoreOpts -> RestoreOpts)]
restoreDescriptions = 
   [ Option [] ["tarball"]
      (ReqArg (\file opts -> opts { optRestore = Just file }) "TARBALL")
      "Backup tarball produced by the server."
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optRestoreDir = Just file }) "DIR")
      "Directory in which to store the persistent state of the server (default state/)"
   ]


data BackupOpts = BackupOpts {
    optBackup :: Maybe FilePath,
    optBackupDir :: Maybe FilePath
} deriving (Show)

defaultBackupOpts :: BackupOpts
defaultBackupOpts = BackupOpts Nothing Nothing

backupDescriptions :: [OptDescr (BackupOpts -> BackupOpts)]
backupDescriptions = 
  [ Option ['o'] ["output"]
      (ReqArg (\file opts -> opts { optBackup = Just file }) "TARBALL")
      "The path to write the backup tarball (default export.tar)"
  , Option [] ["state-dir"]
      (ReqArg (\file opts -> opts { optBackupDir = Just file }) "DIR")
      "Directory from which to read persistent state of the server (default state/)"
   ]


data ConvertOpts = ConvertOpts {
    optImportIndex    :: Maybe FilePath,
    optImportLog      :: Maybe FilePath,
    optImportArchive  :: Maybe FilePath,
    optImportHtPasswd :: Maybe FilePath,
    optImportAdmins   :: Maybe FilePath,
    optConvertTarball :: Maybe FilePath
} deriving (Show)
defaultConvertOpts :: ConvertOpts
defaultConvertOpts = ConvertOpts Nothing Nothing Nothing Nothing Nothing Nothing
convertDescriptions :: [OptDescr (ConvertOpts -> ConvertOpts)]
convertDescriptions =
  [ Option [] ["index"]
      (ReqArg (\file opts -> opts { optImportIndex = Just file }) "TARBALL")
      "Import an existing hackage index file (00-index.tar.gz)"
  , Option [] ["log"]
      (ReqArg (\file opts -> opts { optImportLog = Just file }) "LOG")
      "Import an existing hackage upload log file"
  , Option [] ["archive"]
      (ReqArg (\file opts -> opts { optImportArchive = Just file }) "LOG")
      "Import an existing hackage package tarball archive file (archive.tar)"
  , Option [] ["accounts"]
      (ReqArg (\file opts -> opts { optImportHtPasswd = Just file }) "HTPASSWD")
      "Import an existing apache 'htpasswd' user account database file"
  , Option [] ["admins"]
      (ReqArg (\file opts -> opts { optImportAdmins = Just file}) "ADMINS")
      "Import a text file containing a list a users which should be administrators"
  , Option ['o'] ["output"]
      (ReqArg (\file opts -> opts { optConvertTarball = Just file }) "TARBALL")
      "The path to write the backup tarball (default export.tar)"
  ]

