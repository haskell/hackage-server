{-# LANGUAGE PatternGuards #-}

module Main where

import qualified Distribution.Server as Server
import Distribution.Server (ServerConfig(..), Server)
import Distribution.Server.Framework.BackupRestore (equalTarBall)

import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( topHandler, die )

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( bracket )
import System.Posix.Signals as Signal
         ( installHandler, Handler(Catch), userDefinedSignal1 )
import System.IO
         ( stdout, hFlush )
import System.Directory
         ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
         , Permissions(..), getPermissions )
import System.FilePath
         ( (</>) )
import Distribution.Simple.Command
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToList, flagToMaybe )
import Data.List
         ( sort, intersperse )
import Data.Traversable
         ( forM )
import Control.Monad
         ( unless, when )
import qualified Data.ByteString.Lazy as BS

import Paths_hackage_server as Paths (version)

-------------------------------------------------------------------------------
-- Top level command handling
--

main :: IO ()
main = topHandler $ do
    args <- getArgs
    case commandsRun globalCommand commands args of
      CommandHelp   help  -> printHelp help
      CommandList   opts  -> printOptionsList opts
      CommandErrors errs  -> printErrors errs
      CommandReadyToGo (flags, commandParse) ->
        case commandParse of
          _ | fromFlag (flagVersion flags) -> printVersion
          CommandHelp      help    -> printHelp help
          CommandList      opts    -> printOptionsList opts
          CommandErrors    errs    -> printErrors errs
          CommandReadyToGo action  -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitWith (ExitFailure 1)
    printVersion = putStrLn $ "hackage-server " ++ display version

    commands =
      [ runCommand     `commandAddActionNoArgs` runAction
      , initCommand    `commandAddActionNoArgs` initAction
      , backupCommand  `commandAddActionNoArgs` backupAction
      , restoreCommand `commandAddAction`       restoreAction
      , convertCommand `commandAddActionNoArgs` convertAction
      , testBackupCommand `commandAddActionNoArgs` testBackupAction
      ]

    commandAddActionNoArgs cmd action =
      commandAddAction cmd $ \flags extraArgs -> do
        when (not (null extraArgs)) $
          die $ "'" ++ commandName cmd
             ++ "' does not take any extra arguments: " ++ unwords extraArgs
        action flags



info :: String -> IO ()
info msg = do
  pname <- getProgName
  putStrLn (pname ++ ": " ++ msg)
  hFlush stdout


-------------------------------------------------------------------------------
-- Global command
--

data GlobalFlags = GlobalFlags {
    flagVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    flagVersion = Flag False
  }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandUsage        = \_ ->
         "Hackage server: serves a collection of Haskell Cabal packages\n",
    commandDescription  = Just $ \pname ->
         "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "Steps to create a new empty server instance:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["init", "run"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         flagVersion (\v flags -> flags { flagVersion = v })
         (noArg (Flag True))
      ]
  }

-------------------------------------------------------------------------------
-- Run command
--

data RunFlags = RunFlags {
    flagPort      :: Flag String,
    flagHost      :: Flag String,
    flagStateDir  :: Flag FilePath,
    flagStaticDir :: Flag FilePath,
    flagTmpDir    :: Flag FilePath,
    flagTemp      :: Flag Bool
  }

defaultRunFlags :: RunFlags
defaultRunFlags = RunFlags {
    flagPort      = NoFlag,
    flagHost      = NoFlag,
    flagStateDir  = NoFlag,
    flagStaticDir = NoFlag,
    flagTemp      = Flag False,
    flagTmpDir    = NoFlag
  }

runCommand :: CommandUI RunFlags
runCommand = makeCommand name shortDesc longDesc defaultRunFlags options
  where
    name       = "run"
    shortDesc  = "Run an already-initialized Hackage server."
    longDesc   = Just $ \progname ->
                  "Note: the " ++ progname ++ " data lock prevents two "
               ++ "state-accessing modes from\nbeing run simultaneously.\n\n"
               ++ "On unix systems you can tell the server to checkpoint its "
               ++ "database state using:\n"
               ++ " $ kill -USR1 $the_pid\n"
               ++ "where $the_pid is the process id of the running server.\n"
    options _  =
      [ option [] ["port"]
          "Port number to serve on (default 8080)"
          flagPort (\v flags -> flags { flagPort = v })
          (reqArgFlag "PORT")
      , option [] ["host"]
          "Server's host name (defaults to machine name)"
          flagHost (\v flags -> flags { flagHost = v })
          (reqArgFlag "NAME")
      , option [] ["state-dir"]
          "Directory in which to store the persistent state of the server (default state/)"
          flagStateDir (\v flags -> flags { flagStateDir = v })
          (reqArgFlag "DIR")
      , option [] ["static-dir"]
          "Directory in which to find the html and other static files (default: cabal location)"
          flagStaticDir (\v flags -> flags { flagStaticDir = v })
          (reqArgFlag "DIR")
      , option [] ["tmp-dir"]
          "Temporary directory in which to store file uploads until they are moved to a permanent location."
          flagTmpDir (\v flags -> flags { flagTmpDir = v })
          (reqArgFlag "DIR")
      , option [] ["temp-run"]
          "Set up a temporary server while initializing state for maintenance restarts"
          flagTemp (\v flags -> flags { flagTemp = v })
          (noArg (Flag True))
      ]

runAction :: RunFlags -> IO ()
runAction opts = do
    defaults <- Server.defaultServerConfig

    port <- checkPortOpt defaults (flagToMaybe (flagPort opts))
    let hostname  = fromFlagOrDefault (confHostName  defaults) (flagHost      opts)
        stateDir  = fromFlagOrDefault (confStateDir  defaults) (flagStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagStaticDir opts)
        tmpDir    = fromFlagOrDefault (stateDir </> "tmp") (flagTmpDir    opts)
        config = defaults {
            confHostName  = hostname,
            confPortNum   = port,
            confStateDir  = stateDir,
            confStaticDir = staticDir,
            confTmpDir    = tmpDir
        }

    checkBlankServerState =<< Server.hasSavedState config
    checkStaticDir staticDir (flagStaticDir opts)
    checkTmpDir    tmpDir

    let useTempServer = fromFlag (flagTemp opts)
    withServer config useTempServer $ \server ->
      withCheckpointHandler server $ do
        info $ "Ready! Point your browser at http://localhost"
            ++ if port == 80 then "/" else ":" ++ show port ++ "/"

        Server.run server

  where
    -- Option handling:
    --
    checkPortOpt defaults Nothing    = return (confPortNum defaults)
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

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
          info "Done"
        setHandler h =
          Signal.installHandler Signal.userDefinedSignal1 h Nothing

    checkBlankServerState  hasSavedState = when (not hasSavedState) . die $
            "There is no existing server state.\nYou can either import "
         ++ "existing data using the various import modes, or start with "
         ++ "an empty state using the new mode. Either way, we have to make "
         ++ "sure that there is at least one admin user account, otherwise "
         ++ "you'll not be able to administer your shiny new hackage server!\n"
         ++ "Use --help for more information."

-- Check that tmpDir exists and is readable & writable
checkTmpDir :: FilePath -> IO ()
checkTmpDir tmpDir = do
  exists <- doesDirectoryExist tmpDir
  when (not exists) $ fail $ "The temporary directory " ++ tmpDir ++ " does not exist. Create the directory or use --tmp-dir to specify an alternate location."
  perms <- getPermissions tmpDir
  when (not $ readable perms) $
    fail $ "The temporary directory " ++ tmpDir ++ " is not readable by the server. Fix the permissions or use --tmp-dir to specify an alternate location."
  when (not $ writable perms) $
    fail $ "The temporary directory " ++ tmpDir ++ " is not writable by the server. Fix the permissions or use --tmp-dir to specify an alternate location."

-- Check that staticDir exists and is readable
checkStaticDir :: FilePath -> Flag FilePath -> IO ()
checkStaticDir staticDir staticDirFlag = do
    exists <- doesDirectoryExist staticDir
    when (not exists) $
      case staticDirFlag of
        Flag _ -> die $ "The given static files directory " ++ staticDir
                     ++ " does not exist."
        -- Be helpful to people running from the build tree
        NoFlag -> die $ "It looks like you are running the server without "
                     ++ "installing it. That is fine but you will have to "
                     ++ "give the location of the static html files with the "
                     ++ "--static-dir flag."
    perms <- getPermissions staticDir
    when (not $ readable perms) $
      die $ "The static files directory " ++ staticDir
          ++ " exists but is not readable by the server."


-------------------------------------------------------------------------------
-- Init command
--

data InitFlags = InitFlags {
    flagInitAdmin     :: Flag String,
    flagInitStateDir  :: Flag FilePath,
    flagInitStaticDir :: Flag FilePath
  }

defaultInitFlags :: InitFlags
defaultInitFlags = InitFlags {
    flagInitAdmin     = NoFlag,
    flagInitStateDir  = NoFlag,
    flagInitStaticDir = NoFlag
  }

initCommand :: CommandUI InitFlags
initCommand = makeCommand name shortDesc longDesc defaultInitFlags options
  where
    name       = "init"
    shortDesc  = "Initialize the server state to a useful default."
    longDesc   = Just $ \_ ->
                 "Creates an empty package collection and one admininstrator "
              ++ "account so that you\ncan log in via the web interface and "
              ++ "bootstrap from there.\n"
    options _  =
      [ option [] ["admin"]
          "New server's administrator, name:password (default: admin:admin)"
          flagInitAdmin (\v flags -> flags { flagInitAdmin = v })
          (reqArgFlag "NAME:PASS")
      , option [] ["state-dir"]
          "Directory in which to store the persistent state of the server (default state/)"
          flagInitStateDir (\v flags -> flags { flagInitStateDir = v })
          (reqArgFlag "DIR")
      , option [] ["static-dir"]
          "Directory in which to find the html and other static files (default: cabal location)"
          flagInitStaticDir (\v flags -> flags { flagInitStaticDir = v })
          (reqArgFlag "DIR")
      ]

initAction :: InitFlags -> IO ()
initAction opts = do
    defaults <- Server.defaultServerConfig

    let stateDir  = fromFlagOrDefault (confStateDir defaults)  (flagInitStateDir opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagInitStaticDir opts)
        config = defaults {
            confStateDir  = stateDir,
            confStaticDir = staticDir
        }
        parseAdmin adminStr = case break (==':') adminStr of
            (uname, ':':pass) -> Just (uname, pass)
            _                 -> Nothing

    admin <- case flagInitAdmin opts of
        NoFlag   -> return ("admin", "admin")
        Flag str -> case parseAdmin str of
            Just arg -> return arg
            Nothing  -> fail $ "Couldn't parse username:password in " ++ show str

    checkAccidentalDataLoss =<< Server.hasSavedState config
    checkStaticDir staticDir (flagInitStaticDir opts)

    withServer config False $ \server -> do
        info "Creating initial state..."
        Server.initState server admin
        createDirectory (stateDir </> "tmp")
        when (flagInitAdmin opts == NoFlag) $
          info $ "Using default administrator account "
              ++ "(user admin, passwd admin)"
        info "Done"


-------------------------------------------------------------------------------
-- Backup command
--

data BackupFlags = BackupFlags {
    flagBackup    :: Flag FilePath,
    flagBackupDir :: Flag FilePath
  }

defaultBackupFlags :: BackupFlags
defaultBackupFlags = BackupFlags {
    flagBackup    = NoFlag,
    flagBackupDir = NoFlag
  }

backupCommand :: CommandUI BackupFlags
backupCommand = makeCommand name shortDesc longDesc defaultBackupFlags options
  where
    name       = "backup"
    shortDesc  = "Create a backup tarball of the server's database."
    longDesc   = Just $ \_ ->
                 "Creates a tarball containing all of the data that the server "
              ++ "manages.\nThe purpose is for backup and for data integrity "
              ++ "across server upgrades.\nThe tarball contains files in "
              ++ "standard formats or simple text formats.\nThe backup can be "
              ++ "restored using the 'restore' command.\n"
    options _  =
      [ option ['o'] ["output"]
          "The path to write the backup tarball (default export.tar)"
          flagBackup (\v flags -> flags { flagBackup = v })
          (reqArgFlag "TARBALL")
      , option [] ["state-dir"]
          "Directory from which to read persistent state of the server (default state/)"
          flagBackupDir (\v flags -> flags { flagBackupDir = v })
          (reqArgFlag "DIR")
      ]

backupAction :: BackupFlags -> IO ()
backupAction opts = do
    defaults <- Server.defaultServerConfig

    let stateDir = fromFlagOrDefault (confStateDir defaults) (flagBackupDir opts)
        config = defaults { confStateDir = stateDir }
        exportPath = fromFlagOrDefault "export.tar" (flagBackupDir opts)

    withServer config False $ \server -> do
      info "Preparing export tarball"
      tar <- Server.exportServerTar server
      info "Saving export tarball"
      BS.writeFile exportPath tar
      info "Done"


-------------------------------------------------------------------------------
-- Test backup command
--

data TestBackupFlags = TestBackupFlags {
    flagTestBackupDir     :: Flag FilePath,
    flagTestBackupTmpDir  :: Flag FilePath
  }

defaultTestBackupFlags :: TestBackupFlags
defaultTestBackupFlags = TestBackupFlags {
    flagTestBackupDir    = NoFlag,
    flagTestBackupTmpDir = NoFlag
  }

testBackupCommand :: CommandUI TestBackupFlags
testBackupCommand = makeCommand name shortDesc longDesc defaultTestBackupFlags options
  where
    name       = "test-backup"
    shortDesc  = "Test backup and restore of the server's database."
    longDesc   = Just $ \_ ->
                 "Checks that backing up and then restoring is the identity function on the"
              ++ "server state,\n and that restoring and then backing up is the identity function"
              ++ "on the backup tarball.\n"
    options _  =
      [ option [] ["state-dir"]
          "Directory from which to read persistent state of the server (default state/)"
          flagTestBackupDir (\v flags -> flags { flagTestBackupDir = v })
          (reqArgFlag "DIR")
      , option [] ["tmp-dir"]
          "Temporary directory in which to store temporary information generated by the test."
          flagTestBackupTmpDir (\v flags -> flags { flagTestBackupTmpDir = v })
          (reqArgFlag "DIR")
      ]

-- FIXME: the following acidic types are neither backed up nor tested:
--   PlatformPackages
--   PreferredVersions
--   CandidatePackages
--   IndexUsers
--   TarIndexMap

testBackupAction :: TestBackupFlags -> IO ()
testBackupAction opts = do
    defaults <- Server.defaultServerConfig

    let stateDir = fromFlagOrDefault (confStateDir defaults) (flagTestBackupDir    opts)
        tmpDir   = fromFlagOrDefault (stateDir </> "tmp")    (flagTestBackupTmpDir opts)
        config = defaults {
            confStateDir = stateDir,
            confTmpDir   = tmpDir
          }
        tmpStateDir = tmpDir </> "state"

    checkTmpDir tmpDir

    (tar, test_roundtrip) <- withServer config False $ \server -> do
      info "Taking a snapshot of server state"
      test_roundtrip <- Server.testRoundtrip server
      info "Preparing export tarball"
      tar <- Server.exportServerTar server
      return (tar, test_roundtrip)

    createDirectoryIfMissing True tmpStateDir

    withServer (config { confStateDir = tmpStateDir }) False $ \server -> do
      info "Parsing import tarball"
      res <- Server.importServerTar server tar
      maybe (return ()) fail res
      info "Checking snapshot"
      errs <- test_roundtrip
      unless (null errs) $ do
        mapM_ info errs
        fail "Snapshot check failed!"
      info "Preparing second export tarball"
      tar' <- Server.exportServerTar server
      case tar `equalTarBall` tar' of
        [] -> return ()
        tar_eq_errs -> do
          mapM_ info tar_eq_errs
          fail "Tarballs don't match!"


-------------------------------------------------------------------------------
-- Restore command
--

data RestoreFlags = RestoreFlags {
    flagRestore    :: Flag FilePath,
    flagRestoreDir :: Flag FilePath
  }

defaultRestoreFlags :: RestoreFlags
defaultRestoreFlags = RestoreFlags {
    flagRestore    = NoFlag,
    flagRestoreDir = NoFlag
  }

restoreCommand :: CommandUI RestoreFlags
restoreCommand = makeCommand name shortDesc longDesc defaultRestoreFlags options
  where
    name       = "restore"
    shortDesc  = "Restore server state from a backup tarball."
    longDesc   = Just $ \_ ->
                 "Note that this creates a new server state, so for safety "
              ++ "it requires that the\nserver not be initialised already.\n"
    options _  =
      [ option [] ["state-dir"]
        "Directory in which to store the persistent state of the server (default state/)"
        flagRestoreDir (\v flags -> flags { flagRestoreDir = v })
        (reqArgFlag "DIR")
      ]

restoreAction :: RestoreFlags -> [String] -> IO ()
restoreAction _ [] = die "No restore tarball given."
restoreAction opts [tarFile] = do
    defaults <- Server.defaultServerConfig

    let stateDir = fromFlagOrDefault (confStateDir defaults) (flagRestoreDir opts)
        config = defaults { confStateDir  = stateDir }

    checkAccidentalDataLoss =<< Server.hasSavedState config

    withServer config False $ \server -> do
        tar <- BS.readFile tarFile
        info "Parsing import tarball..."
        res <- Server.importServerTar server tar
        case res of
            Just err -> fail err
            _ -> info "Successfully imported."
restoreAction _ _ = die "There should be exactly one argument: the backup tarball."

-------------------------------------------------------------------------------
-- Convert command
--

data ConvertFlags = ConvertFlags {
    flagImportIndex    :: Flag FilePath,
    flagImportLog      :: Flag FilePath,
    flagImportArchive  :: Flag FilePath,
    flagImportHtPasswd :: Flag FilePath,
    flagImportAdmins   :: Flag FilePath,
    flagConvertTarball :: Flag FilePath
  }

defaultConvertFlags :: ConvertFlags
defaultConvertFlags = ConvertFlags {
    flagImportIndex    = NoFlag,
    flagImportLog      = NoFlag,
    flagImportArchive  = NoFlag,
    flagImportHtPasswd = NoFlag,
    flagImportAdmins   = NoFlag,
    flagConvertTarball = NoFlag
  }

convertCommand :: CommandUI ConvertFlags
convertCommand = makeCommand name shortDesc longDesc defaultConvertFlags options
  where
    name       = "convert"
    shortDesc  = "Convert legacy Hackage data to a new backup tarball."
    longDesc   = Just $ \_ ->
                 "This is not needed by most users. It is just for the "
              ++ "migration of the central\nhackage.haskell.org server from "
              ++ "the old hackage-scripts data formats.\n\nIf you want to create "
              ++ "your own hackage mirror then initialise an empty server\nand "
              ++ "use the hackage-mirror client to copy the packages over.\n"
    options _  =
      [ option [] ["index"]
          "Import an existing hackage index file (00-index.tar.gz)"
          flagImportIndex (\v flags -> flags { flagImportIndex = v })
          (reqArgFlag "TARBALL")
      , option [] ["log"]
          "Import an existing hackage upload log file"
          flagImportLog (\v flags -> flags { flagImportLog = v })
          (reqArgFlag "LOG")
      , option [] ["archive"]
          "Import an existing hackage package tarball archive file (archive.tar)"
          flagImportArchive (\v flags -> flags { flagImportArchive = v })
          (reqArgFlag "LOG")
      , option [] ["accounts"]
          "Import an existing apache 'htpasswd' user account database file"
          flagImportHtPasswd (\v flags -> flags { flagImportHtPasswd = v })
          (reqArgFlag "HTPASSWD")
      , option [] ["admins"]
          "Import a text file containing a list a users which should be administrators"
          flagImportAdmins (\v flags -> flags { flagImportAdmins = v })
          (reqArgFlag "ADMINS")
      , option ['o'] ["output"]
          "The path to write the backup tarball (default export.tar)"
          flagConvertTarball (\v flags -> flags { flagConvertTarball = v })
          (reqArgFlag "TARBALL")
      ]

convertAction :: ConvertFlags -> IO ()
convertAction ConvertFlags {
                flagImportAdmins   = Flag _,
                flagImportHtPasswd = NoFlag
              }
  = die "Cannot import administrators without users"

convertAction ConvertFlags {
                flagImportIndex    = NoFlag,
                flagImportLog      = NoFlag,
                flagImportArchive  = Flag _
              }
  = die "An archive tar should be imported along with an index tarball."

convertAction ConvertFlags {
                flagImportIndex    = NoFlag,
                flagImportLog      = NoFlag,
                flagImportHtPasswd = Flag _
              }
  = die "An htpasswd file should be imported along with an index."

convertAction ConvertFlags {
                flagImportIndex    = Flag indexFileName,
                flagImportLog      = Flag logFileName,
                flagImportArchive  = archiveFile,
                flagImportHtPasswd = htpasswdFile,
                flagImportAdmins   = adminsFile,
                flagConvertTarball = exportFile
              }
  = do
    indexFile <- BS.readFile indexFileName
    logFile   <- readFile logFileName
    tarballs  <- forM (flagToMaybe archiveFile)  BS.readFile
    htpasswd  <- forM (flagToMaybe htpasswdFile) readFile
    admins    <- forM (flagToMaybe adminsFile)   readFile
    let exportPath = fromFlagOrDefault "export.tar" exportFile

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

convertAction _
  = die "A package index and log file must be supplied together."


-------------------------------------------------------------------------------
-- common action functions
--

withServer :: ServerConfig -> Bool -> (Server -> IO a) -> IO a
withServer config doTemp = bracket initialise shutdown
  where
    initialise = do
      mtemp <- case doTemp of
          True  -> do
            info "Setting up temp sever"
            fmap Just $ Server.setUpTemp config 1
          False -> return Nothing
      info "Initializing happstack-state..."
      server <- Server.initialise config
      info "Server data loaded into memory"
      forM mtemp $ \temp -> do
        info "Tearing down temp server"
        Server.tearDownTemp temp
      return server

    shutdown server = do
      -- This only shuts down happstack-state and writes a checkpoint;
      -- the HTTP part takes care of itself
      info "Shutting down..."
      Server.shutdown server

-- Import utilities
checkAccidentalDataLoss :: Bool -> IO ()
checkAccidentalDataLoss hasSavedState =
    when hasSavedState . die $
        "The server already has an initialised database!!\n"
     ++ "If you really *really* intend to completely reset the "
     ++ "whole database you should remove the state/ directory."

-- option utility
reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description
           -> (a -> Flag String) -> (Flag String -> a -> a)
           -> OptDescr a
reqArgFlag ad = reqArg' ad Flag flagToList
