{-# LANGUAGE PatternGuards #-}

module Main where

import qualified Distribution.Server as Server
import Distribution.Server (ListenOn(..), ServerConfig(..), Server)
import Distribution.Server.Framework.Feature
import Distribution.Server.Framework.Logging
import Distribution.Server.Framework.BackupRestore (equalTarBall, restoreServerBackup)
import Distribution.Server.Framework.BackupDump (dumpServerBackup, BackupType(..))
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Util.SigTerm

import Distribution.Simple.Utils
         ( topHandler, dieNoVerbosity )
import Distribution.Verbosity as Verbosity

import System.Environment
         ( getArgs, getProgName )
import System.Exit
         ( exitWith, ExitCode(..) )
import Control.Exception
         ( bracket )
import System.Posix.Signals as Signal
         ( Signal
         , installHandler
         , Handler(Catch)
         , sigUSR1
         , sigUSR2
         , sigHUP
         )
import System.IO
import System.Directory
         ( createDirectory, createDirectoryIfMissing, doesDirectoryExist
         , getDirectoryContents, Permissions(..), getPermissions )
import System.FilePath
         ( (</>), (<.>) )
import Network.URI
         ( URI(..), URIAuth(..), parseAbsoluteURI )
import Distribution.Simple.Command
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, fromFlagOrDefault, flagToList, flagToMaybe )
import Data.Maybe
         ( isNothing )
import Data.List
         ( intercalate, isInfixOf )
import Data.Traversable
         ( forM )
import Data.Version
         ( showVersion )
import Control.Monad
         ( void, unless, when, filterM )
import Control.Applicative
         ( (<$>) )
import Control.Arrow
         ( second )
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Server.Util.GZip as GZip
import qualified Text.Parsec as Parse

import Paths_hackage_server as Paths (version)

-------------------------------------------------------------------------------
-- Top level command handling
--

main :: IO ()
main = topHandler $ do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case commandsRun (globalCommand commands) commands args of
      CommandHelp   help  -> printHelp help
      CommandList   opts  -> printOptionsList opts
      CommandErrors errs  -> printErrors errs
      CommandReadyToGo (flags, commandParse) ->
        case commandParse of
          _ | fromFlag (flagGlobalVersion flags) -> printVersion
          CommandHelp      help    -> printHelp help
          CommandList      opts    -> printOptionsList opts
          CommandErrors    errs    -> printErrors errs
          CommandReadyToGo action  -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (intercalate "\n" errs)
      exitWith (ExitFailure 1)
    printVersion = putStrLn $ "hackage-server " ++ showVersion version

    commands =
      [ runCommand     `commandAddActionNoArgs` runAction
      , initCommand    `commandAddActionNoArgs` initAction
      , backupCommand  `commandAddActionNoArgs` backupAction
      , restoreCommand `commandAddAction`       restoreAction
      , testBackupCommand `commandAddActionNoArgs` testBackupAction
      ]

    commandAddActionNoArgs cmd action =
      commandAddAction cmd $ \flags extraArgs -> do
        when (not (null extraArgs)) $
          dieNoVerbosity $ "'" ++ commandName cmd
             ++ "' does not take any extra arguments: " ++ unwords extraArgs
        action flags


-------------------------------------------------------------------------------
-- Global command
--

data GlobalFlags = GlobalFlags {
    flagGlobalVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    flagGlobalVersion = Flag False
  }

globalCommand :: [Command a] -> CommandUI GlobalFlags
globalCommand commands = CommandUI {
    commandName         = "",
    commandSynopsis     = "Hackage server: serves a collection of Haskell Cabal packages",
    commandUsage        = usageAlternatives "" ["COMMAND [FLAGS]", "[GLOBAL FLAGS]"],
    commandDescription  = Just $ \pname ->
     let  commands' = commands ++ [commandAddAction helpCommandUI undefined]
          cmdDescs = getNormalCommandDescriptions commands'
          maxlen    = maximum $ [length name | (name, _) <- cmdDescs]
          align str = str ++ replicate (maxlen - length str) ' '
      in "Commands:\n"
      ++ unlines [ "  " ++ align name ++ "    " ++ description
                 | (name, description) <- cmdDescs ]
      ++ "\n"
      ++ "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n",
    commandNotes        = Just $ \pname ->
         "Steps to create a new empty server instance:\n"
      ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                | x <- ["init", "run"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         flagGlobalVersion (\v flags -> flags { flagGlobalVersion = v })
         (noArg (Flag True))
      ]
  }

-- Common options
--

optionVerbosity :: (a -> Flag Verbosity)
                -> (Flag Verbosity -> a -> a)
                -> OptionField a
optionVerbosity getter setter =
  option "v" ["verbose"]
    "Control verbosity (n is 0--3, default verbosity level is 1)"
    getter setter
    (optArg "n" (fmap Flag Verbosity.flagToVerbosity)
          (Flag Verbosity.verbose)
          (fmap (Just . showForCabal) . flagToList))

optionStateDir :: (a -> Flag FilePath)
               -> (Flag FilePath -> a -> a)
               -> OptionField a
optionStateDir getter setter =
  option [] ["state-dir"]
    "Directory in which to store the persistent state of the server (default state/)"
    getter setter
    (reqArgFlag "DIR")

optionStaticDir :: (a -> Flag FilePath)
                -> (Flag FilePath -> a -> a)
                -> OptionField a
optionStaticDir getter setter =
  option [] ["static-dir"]
    "Directory in which to find the html templates and static files (default: cabal location)"
    getter setter
    (reqArgFlag "DIR")


-------------------------------------------------------------------------------
-- Run command
--

data RunFlags = RunFlags {
    flagRunVerbosity       :: Flag Verbosity,
    flagRunPort            :: Flag String,
    flagRunIP              :: Flag String,
    flagRunHostURI         :: Flag String,
    flagRunStateDir        :: Flag FilePath,
    flagRunStaticDir       :: Flag FilePath,
    flagRunTmpDir          :: Flag FilePath,
    flagRunTemp            :: Flag Bool,
    flagRunCacheDelay      :: Flag String,
    flagRunLiveTemplates   :: Flag Bool,
    -- Online backup flags
    flagRunBackupOutputDir :: Flag FilePath,
    flagRunBackupLinkBlobs :: Flag Bool,
    flagRunBackupScrubbed  :: Flag Bool
  }

defaultRunFlags :: RunFlags
defaultRunFlags = RunFlags {
    flagRunVerbosity       = Flag Verbosity.normal,
    flagRunPort            = NoFlag,
    flagRunIP              = NoFlag,
    flagRunHostURI         = NoFlag,
    flagRunStateDir        = NoFlag,
    flagRunStaticDir       = NoFlag,
    flagRunTmpDir          = NoFlag,
    flagRunTemp            = Flag False,
    flagRunCacheDelay      = NoFlag,
    flagRunLiveTemplates   = Flag False,
    flagRunBackupOutputDir = Flag "backups",
    flagRunBackupLinkBlobs = Flag False,
    flagRunBackupScrubbed  = Flag False
  }

runCommand :: CommandUI RunFlags
runCommand =
    CommandUI {
      commandName         = "run",
      commandSynopsis     = "Run an already-initialized Hackage server.",
      commandUsage        = usageAlternatives "run" ["[FLAGS]"],
      commandDescription  = Nothing,
      commandNotes        = Just notes,
      commandDefaultFlags = defaultRunFlags,
      commandOptions      = options
    }
  where
    notes pname = "Note: the " ++ pname ++ " data lock prevents two "
               ++ "state-accessing modes from\nbeing run simultaneously.\n\n"
               ++ "On unix systems you can tell the server to checkpoint its "
               ++ "database state using:\n"
               ++ " $ kill -USR1 $the_pid\n"
               ++ "where $the_pid is the process id of the running server. "
               ++ "Similarly,\n"
               ++ " $ kill -USR2 $the_pid\n"
               ++ "starts an online backup.\n"
               ++ "Reload html (and other) templates:\n"
               ++ " $ kill -HUP $the_pid\n"
    options _  =
      [ optionVerbosity
          flagRunVerbosity (\v flags -> flags { flagRunVerbosity = v })
      , option [] ["port"]
          "Port number to serve on (default 8080)"
          flagRunPort (\v flags -> flags { flagRunPort = v })
          (reqArgFlag "PORT")
      , option [] ["ip"]
          "IPv4 address to listen on (default 127.0.0.1)"
          flagRunIP (\v flags -> flags { flagRunIP = v })
          (reqArgFlag "IP")
      , option [] ["base-uri"]
          "Server's public base URI (defaults to machine name)"
          flagRunHostURI (\v flags -> flags { flagRunHostURI = v })
          (reqArgFlag "NAME")
      , optionStateDir
          flagRunStateDir (\v flags -> flags { flagRunStateDir = v })
      , optionStaticDir
          flagRunStaticDir (\v flags -> flags { flagRunStaticDir = v })
      , option [] ["tmp-dir"]
          "Temporary directory in which to store file uploads (default state/tmp/)"
          flagRunTmpDir (\v flags -> flags { flagRunTmpDir = v })
          (reqArgFlag "DIR")
      , option [] ["temp-run"]
          "Set up a temporary server while initializing state for maintenance restarts"
          flagRunTemp (\v flags -> flags { flagRunTemp = v })
          (noArg (Flag True))
      , option [] ["delay-cache-updates"]
          "Save time during bulk imports by delaying cache updates."
          flagRunCacheDelay (\v flags -> flags { flagRunCacheDelay = v })
          (reqArgFlag "SECONDS")
      , option ['o'] ["output-dir"]
          "The directory in which to create the backup (default ./backups/)"
          flagRunBackupOutputDir (\v flags -> flags { flagRunBackupOutputDir = v })
          (reqArgFlag "TARBALL")
      , option [] ["hardlink-blobs"]
          ("Hard-link the blob files in the backup rather than copying them "
           ++ " (reduces disk space and I/O but is less robust to errors).")
          flagRunBackupLinkBlobs (\v flags -> flags { flagRunBackupLinkBlobs = v })
          (noArg (Flag True))
      , option [] ["scrubbed-backup"]
          ("Generate a scrubbed backup containing no user " ++
           "identifying information (for development use)")
          flagRunBackupScrubbed (\v flags -> flags { flagRunBackupScrubbed = v })
          (noArg (Flag True))
      , option [] ["live-templates"]
          "Do not cache templates, for quicker feedback during development."
          flagRunLiveTemplates (\v flags -> flags { flagRunLiveTemplates = v })
          (noArg (Flag True))
      ]

runAction :: RunFlags -> IO ()
runAction opts = do
    defaults <- Server.defaultServerConfig

    port       <- checkPortOpt    defaults (flagToMaybe (flagRunPort       opts))
    ip         <- checkIPOpt      defaults (flagToMaybe (flagRunIP         opts))
    hosturi    <- checkHostURI    defaults (flagToMaybe (flagRunHostURI    opts)) port
    cacheDelay <- checkCacheDelay defaults (flagToMaybe (flagRunCacheDelay opts))
    let stateDir  = fromFlagOrDefault (confStateDir  defaults) (flagRunStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagRunStaticDir opts)
        tmpDir    = fromFlagOrDefault (confTmpDir    defaults) (flagRunTmpDir    opts)
        listenOn  = (confListenOn defaults) {
                       loPortNum = port,
                       loIP      = ip
                    }
        config    = defaults {
                        confHostUri    = hosturi,
                        confListenOn   = listenOn,
                        confStateDir   = stateDir,
                        confStaticDir  = staticDir,
                        confTmpDir     = tmpDir,
                        confCacheDelay = cacheDelay,
                        confLiveTemplates = liveTemplates,
                        confVerbosity  = verbosity
                    }
        outputDir = fromFlag (flagRunBackupOutputDir opts)
        linkBlobs = fromFlag (flagRunBackupLinkBlobs opts)
        scrubbed  = fromFlag (flagRunBackupScrubbed  opts)
        liveTemplates = fromFlag (flagRunLiveTemplates opts)

    checkBlankServerState =<< Server.hasSavedState config
    checkStaticDir staticDir (flagRunStaticDir opts)
    checkTmpDir    tmpDir

    onSigTermCleanShutdown

    let checkpointHandler server = do
          lognotice verbosity "Writing checkpoint..."
          Server.checkpoint server
          lognotice verbosity "Done"

    let backupHandler server = do
          lognotice verbosity "Starting backup..."
          startBackup verbosity scrubbed outputDir linkBlobs server
          lognotice verbosity "Done"

    let reloadHandler server = do
          lognotice verbosity "Dropping cached static files..."
          Server.reloadDatafiles server
          lognotice verbosity "Done"

    let useTempServer = fromFlag (flagRunTemp opts)
    withServer config useTempServer $ \server ->
      withHandler sigUSR1 (checkpointHandler server) $
      withHandler sigUSR2 (backupHandler server) $
      withHandler sigHUP  (reloadHandler server) $ do
        lognotice verbosity $ "Ready! Point your browser at " ++ show hosturi
        Server.run server

  where
    verbosity = fromFlag (flagRunVerbosity opts)

    -- Option handling:
    --
    checkPortOpt defaults Nothing    = return (loPortNum (confListenOn defaults))
    checkPortOpt _        (Just str) = case reads str of
      [(n,"")]  | n >= 1 && n <= 65535
               -> return n
      _        -> fail $ "bad port number " ++ show str

    checkHostURI defaults Nothing port = do
      let guessURI       = confHostUri defaults
          Just authority = uriAuthority guessURI
          portStr | port == 80 = ""
                  | otherwise  = ':' : show port
          guessURI' = guessURI { uriAuthority = Just authority { uriPort = portStr } }
      lognotice verbosity $ "Guessing public URI as " ++ show guessURI'
                        ++ "\n(you can override with the --base-uri= flag)"
      return guessURI'

    checkHostURI _        (Just str) _ = case parseAbsoluteURI str of
      Nothing -> fail $ "Cannot parse as a URI: " ++ str ++ "\n"
                     ++ "Make sure you include the http:// part"
      Just uri
        | uriScheme uri `notElem` ["http:", "https:"] ->
          fail $ "Sorry, the server assumes it will be served (or proxied) "
              ++ " via http or https, so cannot use uri scheme " ++ uriScheme uri
        | isNothing (uriAuthority uri) ->
          fail $ "The base-uri has to include the full host name"
        | uriPath uri `notElem` ["", "/"] ->
          fail $ "Sorry, the server assumes the base-uri to be at the root of "
              ++ " the domain, so cannot use " ++ uriPath uri
        | otherwise -> return uri { uriPath = "" }

    checkIPOpt defaults Nothing    = return (loIP (confListenOn defaults))
    checkIPOpt _        (Just str) =
      let pQuad = do ds <- Parse.many1 Parse.digit
                     let quad = read ds :: Integer
                     when (quad < 0 || quad > 255) $ fail "bad IP address"
                     return quad
          pIPv4 = do q1 <- pQuad
                     void $ Parse.char '.'
                     q2 <- pQuad
                     void $ Parse.char '.'
                     q3 <- pQuad
                     void $ Parse.char '.'
                     q4 <- pQuad
                     Parse.eof
                     return (q1, q2, q3, q4)
      in case Parse.parse pIPv4 str str of
         Left err -> fail (show err)
         Right _ -> return str

    checkCacheDelay defaults Nothing    = return (confCacheDelay defaults)
    checkCacheDelay _        (Just str) = case reads str of
      [(n,"")]  | n >= 0 && n <= 3600
               -> return n
      _        -> fail $ "bad cache delay number " ++ show str

    withHandler :: Signal -> IO () -> IO () -> IO ()
    withHandler signal signalHandler mainAction =
        bracket (setHandler $ Signal.Catch signalHandler)
                setHandler
                (const mainAction)
      where
        setHandler h = Signal.installHandler signal h Nothing

    checkBlankServerState  hasSavedState = when (not hasSavedState) . dieNoVerbosity $
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
        Flag _ -> dieNoVerbosity $ "The given static files directory " ++ staticDir
                     ++ " does not exist."
        -- Be helpful to people running from the build tree
        NoFlag -> dieNoVerbosity $ "It looks like you are running the server without "
                     ++ "installing it. That is fine but you will have to "
                     ++ "give the location of the static html files with the "
                     ++ "--static-dir flag."
    perms <- getPermissions staticDir
    when (not $ readable perms) $
      dieNoVerbosity $ "The static files directory " ++ staticDir
          ++ " exists but is not readable by the server."


-------------------------------------------------------------------------------
-- Init command
--

data InitFlags = InitFlags {
    flagInitVerbosity :: Flag Verbosity,
    flagInitAdmin     :: Flag String,
    flagInitStateDir  :: Flag FilePath,
    flagInitStaticDir :: Flag FilePath
  }

defaultInitFlags :: InitFlags
defaultInitFlags = InitFlags {
    flagInitVerbosity = Flag Verbosity.normal,
    flagInitAdmin     = NoFlag,
    flagInitStateDir  = NoFlag,
    flagInitStaticDir = NoFlag
  }

initCommand :: CommandUI InitFlags
initCommand =
    CommandUI {
      commandName         = "init",
      commandSynopsis     = "Initialize the server state to a useful default.",
      commandUsage        = usageAlternatives "init" ["[FLAGS]"],
      commandDescription  = Just longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultInitFlags,
      commandOptions      = options
    }
  where
    longDesc _ = "Creates an empty package collection and one admininstrator "
              ++ "account so that you\ncan log in via the web interface and "
              ++ "bootstrap from there.\n"
    options _  =
      [ optionVerbosity
          flagInitVerbosity (\v flags -> flags { flagInitVerbosity = v })
      , option [] ["admin"]
          "New server's administrator, name:password (default: admin:admin)"
          flagInitAdmin (\v flags -> flags { flagInitAdmin = v })
          (reqArgFlag "NAME:PASS")
      , optionStateDir
          flagInitStateDir (\v flags -> flags { flagInitStateDir = v })
      , optionStaticDir
          flagInitStaticDir (\v flags -> flags { flagInitStaticDir = v })
      ]

initAction :: InitFlags -> IO ()
initAction opts = do
    defaults <- Server.defaultServerConfig

    let stateDir  = fromFlagOrDefault (confStateDir defaults)  (flagInitStateDir opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagInitStaticDir opts)
        verbosity = fromFlag (flagInitVerbosity opts)
        config    = defaults {
                        confVerbosity = verbosity,
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
        lognotice verbosity "Creating initial state..."
        Server.initState server admin
        createDirectory (stateDir </> "tmp")
        when (flagInitAdmin opts == NoFlag) $
          lognotice verbosity $ "Using default administrator account "
              ++ "(user admin, passwd admin)"
        lognotice verbosity "Done"


-------------------------------------------------------------------------------
-- Backup command
--

data BackupFlags = BackupFlags {
    flagBackupVerbosity   :: Flag Verbosity,
    flagBackupOutputDir   :: Flag FilePath,
    flagBackupStateDir    :: Flag FilePath,
    flagBackupStaticDir   :: Flag FilePath,
    flagBackupLinkBlobs   :: Flag Bool,
    flagBackupScrubbed    :: Flag Bool
  }

defaultBackupFlags :: BackupFlags
defaultBackupFlags = BackupFlags {
    flagBackupVerbosity   = Flag Verbosity.normal,
    flagBackupOutputDir   = Flag "backups",
    flagBackupStateDir    = NoFlag,
    flagBackupStaticDir   = NoFlag,
    flagBackupLinkBlobs   = Flag False,
    flagBackupScrubbed    = Flag False
  }

backupCommand :: CommandUI BackupFlags
backupCommand =
    CommandUI {
      commandName         = "backup",
      commandSynopsis     = "Create a backup of the server's database.",
      commandUsage        = usageAlternatives "backup" ["[FLAGS]"],
      commandDescription  = Just longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultBackupFlags,
      commandOptions      = options
    }
  where
    longDesc _ = "Creates a backup containing all of the data that the server "
              ++ "manages.\nThe purpose is for backup and for data integrity "
              ++ "across server upgrades.\nThe backup consists of a per-backup "
              ++ "tarball plus a shared directory of static\nfiles. The tarball "
              ++ "contains files in standard formats or simple text formats.\n"
              ++ "The backup can be restored using the 'restore' command.\n"
    options _  =
      [ optionVerbosity
          flagBackupVerbosity (\v flags -> flags { flagBackupVerbosity = v })
      , optionStateDir
          flagBackupStateDir (\v flags -> flags { flagBackupStateDir = v })
      , optionStaticDir
          flagBackupStaticDir (\v flags -> flags { flagBackupStaticDir = v })
      , option ['o'] ["output-dir"]
          "The directory in which to create the backup (default ./backups/)"
          flagBackupOutputDir (\v flags -> flags { flagBackupOutputDir = v })
          (reqArgFlag "TARBALL")
      , option [] ["hardlink-blobs"]
          ("Hard-link the blob files in the backup rather than copying them "
           ++ " (reduces disk space and I/O but is less robust to errors).")
          flagBackupLinkBlobs (\v flags -> flags { flagBackupLinkBlobs = v })
          (noArg (Flag True))
      , option [] ["scrubbed-backup"]
          ("Generate a scrubbed backup containing no user " ++
           "identifying information (for development use)")
          flagBackupScrubbed (\v flags -> flags { flagBackupScrubbed = v })
          (noArg (Flag True))
      ]

backupAction :: BackupFlags -> IO ()
backupAction opts = do
    defaults <- Server.defaultServerConfig

    let stateDir  = fromFlagOrDefault (confStateDir defaults)  (flagBackupStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagBackupStaticDir opts)
        outputDir = fromFlag (flagBackupOutputDir opts)
        linkBlobs = fromFlag (flagBackupLinkBlobs opts)
        scrubbed = fromFlag (flagBackupScrubbed opts)
        verbosity = fromFlag (flagBackupVerbosity opts)
        config    = defaults {
                      confVerbosity = verbosity,
                      confStateDir  = stateDir,
                      confStaticDir = staticDir
                     }

    withServer config False $ startBackup verbosity scrubbed outputDir linkBlobs

startBackup :: Verbosity -> Bool -> FilePath -> Bool -> Server -> IO ()
startBackup verbosity scrubbed outputDir linkBlobs server = do
  let store = Server.serverBlobStore (Server.serverEnv server)
      state = Server.serverState server
      backupType = if scrubbed then ScrubbedBackup else FullBackup
  dumpServerBackup verbosity backupType outputDir Nothing store linkBlobs
                   (map (second (\s -> abstractStateBackup s backupType)) state)

-------------------------------------------------------------------------------
-- Test backup command
--

data TestBackupFlags = TestBackupFlags {
    flagTestBackupVerbosity :: Flag Verbosity,
    flagTestBackupStateDir  :: Flag FilePath,
    flagTestBackupStaticDir :: Flag FilePath,
    flagTestBackupTestDir   :: Flag FilePath,
    flagTestBackupLinkBlobs :: Flag Bool,
    flagTestBackupScrubbed  :: Flag Bool,
    flagTestBackupFeatures  :: Flag String
  }

defaultTestBackupFlags :: TestBackupFlags
defaultTestBackupFlags = TestBackupFlags {
    flagTestBackupVerbosity = Flag Verbosity.normal,
    flagTestBackupStateDir  = NoFlag,
    flagTestBackupStaticDir = NoFlag,
    flagTestBackupTestDir   = Flag "test-backup",
    flagTestBackupLinkBlobs = Flag False,
    flagTestBackupScrubbed  = Flag False,
    flagTestBackupFeatures  = NoFlag
  }

testBackupCommand :: CommandUI TestBackupFlags
testBackupCommand =
    CommandUI {
      commandName         = "test-backup",
      commandSynopsis     = "A self-test of the server's database backup/restore system.",
      commandUsage        = usageAlternatives "test-backup" ["[FLAGS]"],
      commandDescription  = Just longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultTestBackupFlags,
      commandOptions      = options

    }
  where
    longDesc _ = "Checks that backing up and then restoring is the identity function on the "
              ++ "server\nstate, and that restoring and then backing up is the identity function"
              ++ "on the\nbackup tarball.\n"
    options _  =
      [ optionVerbosity
          flagTestBackupVerbosity (\v flags -> flags { flagTestBackupVerbosity = v })
      , optionStateDir
          flagTestBackupStateDir (\v flags -> flags { flagTestBackupStateDir = v })
      , optionStaticDir
          flagTestBackupStaticDir (\v flags -> flags { flagTestBackupStaticDir = v })
      , option [] ["test-dir"]
          "Temporary directory in which to store temporary information generated by the test (default test-backup/)."
          flagTestBackupTestDir (\v flags -> flags { flagTestBackupTestDir = v })
          (reqArgFlag "DIR")
      , option [] ["hardlink-blobs"]
          ("Do a partial test, short-circuting the reading and writing of the "
           ++ "blob files (saves on disk I/O, but less test coverage).")
          flagTestBackupLinkBlobs (\v flags -> flags { flagTestBackupLinkBlobs = v })
          (noArg (Flag True))
      , option [] ["scrubbed-backup"]
          ("Generate a scrubbed backup containing no user " ++
           "identifying information (for development use)")
          flagTestBackupScrubbed (\v flags -> flags { flagTestBackupScrubbed = v })
          (noArg (Flag True))
      , option [] ["features"]
          ("Only test the specified features")
          flagTestBackupFeatures (\v flags -> flags { flagTestBackupFeatures = v })
          (reqArgFlag "FEATURES")
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

    let shouldTest  = fromFlagOrDefault (const True) (flip isInfixOf `fmap` flagTestBackupFeatures opts)
        shouldTestM = \(name, _) -> if shouldTest name then putStrLn ("Testing " ++ name) >> return True
                                                       else putStrLn ("Skipping " ++ name) >> return False

    let stateDir    = fromFlagOrDefault (confStateDir  defaults) (flagTestBackupStateDir  opts)
        staticDir   = fromFlagOrDefault (confStaticDir defaults) (flagTestBackupStaticDir opts)
        testDir     = fromFlag (flagTestBackupTestDir  opts)
        linkBlobs   = fromFlag (flagTestBackupLinkBlobs opts)
        scrubbed    = fromFlag (flagTestBackupScrubbed opts)
        backuptype  = if scrubbed then ScrubbedBackup else FullBackup
        verbosity   = fromFlag (flagTestBackupVerbosity opts)
        config      = defaults {
                        confStateDir  = stateDir,
                        confStaticDir = staticDir,
                        confTmpDir    = testDir,
                        confVerbosity = verbosity
                      }

    let dump1Dir    = testDir </> "dump-1"
        restoreDir  = testDir </> "restore"
        dump2Dir    = testDir </> "dump-2"
        tarDumpName = "test-dump"
        dump1Tar    = dump1Dir </> tarDumpName <.> "tar.gz"
        dump2Tar    = dump2Dir </> tarDumpName <.> "tar.gz"

    existsAlready <- doesDirectoryExist testDir
    when existsAlready $ do
      entries <- filter (`notElem` [".", ".."]) <$> getDirectoryContents testDir
      unless (null entries) $
        fail $ "The directory " ++ testDir ++ " contains files. Please remove "
            ++ "or clear it, or select a different one with the --test-dir "
            ++ "flag. (The test procedure needs a clean working area.)"
    mapM_ (createDirectoryIfMissing False) [testDir, dump1Dir, restoreDir, dump2Dir]

    withServer config False $ \server -> do
      let fullState = Server.serverState server
          store = Server.serverBlobStore (Server.serverEnv server)

      state <- filterM shouldTestM fullState

      -- We want to check that our dump/restore correctly preserves all the
      -- data. So we want to do a round trip test, and though it's nice to do
      -- QC tests on each feature's backingup, it adds a lot of confidence to
      -- be able to do a self-test using the full data of your server instance.
      --
      -- Ok, so there are two ways to do a round trip test: compare the internal
      -- representations or compare the external representations. Our strategy
      -- is to do both.
      --
      -- We start with all the data in the server in the internal
      -- representation. We start by writing it all out in the external
      -- representation.
      --
      dumpServerBackup verbosity FullBackup dump1Dir (Just tarDumpName)
                       store linkBlobs
                       (map (second (\s -> abstractStateBackup s backuptype)) state)

      -- Now what we need to do is to keep hold of our current internal state
      -- and construct an extra internal state by restoring from the external
      -- representation that we wrote out previously.
      --
      -- And we can do just that. We've set things up so that every feature in
      -- the server has the capability to initialise a new empty copy of
      -- it's state. That's what abstractStateEmptyCopy does. In addition to
      -- that we get back a comparison action, that when executed will look at
      -- the current value of the state and compare the two, reporting any
      -- mismatches.
      --
      -- So we initialise all these new empty copies, (collecting the comparison
      -- actions)
      --
      (state', compareSts) <-
        unzip <$> sequence
                    [ do (st', cmpSt) <- abstractStateNewEmpty st restoreDir
                         let annotateErr err = featurename ++ ": " ++ err
                         return ((featurename, st'), map annotateErr <$> cmpSt)

                    | (featurename, st) <- state ]

      -- We also need a corresponding empty blob store
      store' <- BlobStorage.open (restoreDir </> "blobs")

      -- And then restore from the external representation into these new empty
      -- copies.
      loginfo verbosity "Restoring from backup tarball"
      let stores' = BlobStorage.BlobStores store' [store]
      res <- restoreServerBackup stores' dump1Tar linkBlobs
                                 (map (second abstractStateRestore) state')
      case res of
        Nothing  -> return ()
        Just err -> fail $ "Error while restoring the backup:\n" ++ err

      -- Write second tarball so that if some of the comparisons go wrong,
      -- we can look at the second backup tarball and manually do some
      -- comparisons
      lognotice verbosity "Preparing second export tarball"
      dumpServerBackup verbosity FullBackup dump2Dir (Just tarDumpName)
                       store' linkBlobs
                       (map (second (\s -> abstractStateBackup s backuptype)) state')

      -- Now we are in a position to check that the original internal state and
      -- the new internal state we get from a dump/restore do actually match up.
      lognotice verbosity "Comparing snapshots before and after dump/restore..."
      stErrs <- concat <$> sequence compareSts
      unless (null stErrs) $ do
        mapM_ (loginfo verbosity) stErrs
        let failLogfile = testDir </> "round-trip-failure.log"
        writeFile failLogfile (intercalate "\n\n" stErrs)
        fail $ "Snapshot check failed!  Log written to " ++ failLogfile
      lognotice verbosity "Snapshots match"

      -- So that was all checking the internal representations matched up after
      -- a round trip. We can also check the external representations match
      -- after a round trip.
      lognotice verbosity "Comparing export tarballs..."
      tar  <- GZip.decompressNamed dump1Tar <$> BS.readFile dump1Tar
      tar' <- GZip.decompressNamed dump2Tar <$> BS.readFile dump2Tar
      let tarErrs = tar `equalTarBall` tar'
      unless (null tarErrs) $ do
        mapM_ (loginfo verbosity) tarErrs
        let failLogfile = testDir </> "round-trip-failure.log"
        writeFile failLogfile (intercalate "\n\n" tarErrs)
        fail $ "Tarballs don't match! Tarballs written to "
            ++ dump1Tar ++ " and " ++ dump2Tar
            ++ " and log written to " ++ failLogfile
      lognotice verbosity "Tarballs match"

-------------------------------------------------------------------------------
-- Restore command
--

data RestoreFlags = RestoreFlags {
    flagRestoreVerbosity :: Flag Verbosity,
    flagRestoreStateDir  :: Flag FilePath,
    flagRestoreStaticDir :: Flag FilePath
  }

defaultRestoreFlags :: RestoreFlags
defaultRestoreFlags = RestoreFlags {
    flagRestoreVerbosity = Flag Verbosity.normal,
    flagRestoreStateDir  = NoFlag,
    flagRestoreStaticDir = NoFlag
  }

restoreCommand :: CommandUI RestoreFlags
restoreCommand =
    CommandUI {
      commandName         = "restore",
      commandSynopsis     = "Restore server state from a backup tarball.",
      commandUsage        = usageAlternatives "restore" ["[FLAGS]"],
      commandDescription  = Nothing,
      commandNotes        = Just notes,
      commandDefaultFlags = defaultRestoreFlags,
      commandOptions      = options
    }
  where
    notes _    = "Note that this creates a new server state, so for safety "
              ++ "it requires that the\nserver not be initialised already.\n"
    options _  =
      [ optionVerbosity
          flagRestoreVerbosity (\v flags -> flags { flagRestoreVerbosity = v })
      , optionStateDir
          flagRestoreStateDir  (\v flags -> flags { flagRestoreStateDir  = v })
      , optionStaticDir
          flagRestoreStaticDir (\v flags -> flags { flagRestoreStaticDir = v })
      ]

restoreAction :: RestoreFlags -> [String] -> IO ()
restoreAction _ [] = dieNoVerbosity "No restore tarball given."
restoreAction opts [tarFile] = do
    defaults <- Server.defaultServerConfig

    let stateDir  = fromFlagOrDefault (confStateDir  defaults) (flagRestoreStateDir  opts)
        staticDir = fromFlagOrDefault (confStaticDir defaults) (flagRestoreStaticDir opts)
        verbosity = fromFlag (flagRestoreVerbosity opts)
        config    = defaults {
                      confStateDir  = stateDir,
                      confStaticDir = staticDir,
                      confVerbosity = verbosity
                    }

    checkAccidentalDataLoss =<< Server.hasSavedState config

    withServer config False $ \server -> do
        let state  = Server.serverState server
            store  = Server.serverBlobStore (Server.serverEnv server)
            stores = BlobStorage.BlobStores store []

        loginfo verbosity "Parsing import tarball..."
        res <- restoreServerBackup stores tarFile False
                                   (map (second abstractStateRestore) state)
        case res of
            Just err -> fail err
            _ ->
                do createDirectory (stateDir </> "tmp")
                   lognotice verbosity "Successfully imported."
restoreAction _ _ = dieNoVerbosity "There should be exactly one argument: the backup tarball."


-------------------------------------------------------------------------------
-- common action functions
--

withServer :: ServerConfig -> Bool -> (Server -> IO a) -> IO a
withServer config doTemp = bracket initialise shutdown
  where
    initialise = do
      mtemp <- case doTemp of
          True  -> do
            loginfo verbosity "Setting up temp sever"
            fmap Just $ Server.setUpTemp config 1
          False -> return Nothing
      loginfo verbosity "Initializing happstack-state..."
      server <- Server.initialise config
      loginfo verbosity "Server data loaded into memory"
      void $ forM mtemp $ \temp -> do
        loginfo verbosity "Tearing down temp server"
        Server.tearDownTemp temp
      return server

    shutdown server = do
      -- This only shuts down happstack-state and writes a checkpoint;
      -- the HTTP part takes care of itself
      loginfo verbosity "Shutting down..."
      Server.shutdown server

    verbosity = confVerbosity config

-- Import utilities
checkAccidentalDataLoss :: Bool -> IO ()
checkAccidentalDataLoss hasSavedState =
    when hasSavedState . dieNoVerbosity $
        "The server already has an initialised database!!\n"
     ++ "If you really *really* intend to completely reset the "
     ++ "whole database you should remove the state/ directory."

-- option utility
reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description
           -> (a -> Flag String) -> (Flag String -> a -> a)
           -> OptDescr a
reqArgFlag ad = reqArg' ad Flag flagToList
