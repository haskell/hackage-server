module Distribution.Client.Mirror.CmdLine (
    MirrorOpts(..)
  , validateOpts
  ) where

-- stdlib
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Exit (exitSuccess)
import System.Console.GetOpt

-- Cabal
import Distribution.Package
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (warn)

-- hackage
import Distribution.Client (validatePackageIds)
import Distribution.Client.Mirror.Config

data MirrorOpts = MirrorOpts {
                    mirrorConfig    :: MirrorConfig,
                    stateDir        :: FilePath,
                    selectedPkgs    :: [PackageId],
                    continuous      :: Maybe Int, -- if so, interval in minutes
                    mo_keepGoing    :: Bool,
                    mirrorUploaders :: Bool,
                    skipExists      :: Bool
                  }

data MirrorFlags = MirrorFlags {
    flagCacheDir        :: Maybe FilePath,
    flagContinuous      :: Bool,
    flagInterval        :: Maybe String,
    flagKeepGoing       :: Bool,
    flagMirrorUploaders :: Bool,
    flagSkipExists      :: Bool,
    flagVerbosity       :: Verbosity,
    flagHelp            :: Bool
}

defaultMirrorFlags :: MirrorFlags
defaultMirrorFlags = MirrorFlags
  { flagCacheDir        = Nothing
  , flagContinuous      = False
  , flagInterval        = Nothing
  , flagKeepGoing       = True
  , flagMirrorUploaders = False
  , flagSkipExists      = False
  , flagVerbosity       = normal
  , flagHelp            = False
  }

mirrorFlagDescrs :: [OptDescr (MirrorFlags -> MirrorFlags)]
mirrorFlagDescrs =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { flagHelp = True }))
      "Show this help text"

  , Option ['v'] []
      (NoArg (\opts -> opts { flagVerbosity = moreVerbose (flagVerbosity opts) }))
      "Verbose mode (can be listed multiple times e.g. -vv)"

  , Option [] ["cache-dir"]
      (ReqArg (\dir opts -> opts { flagCacheDir = Just dir }) "DIR")
      "Where to put downloaded files (default ./mirror-cache/)"

  , Option [] ["continuous"]
      (NoArg (\opts -> opts { flagContinuous = True }))
      "Mirror continuously rather than just once."

  , Option [] ["interval"]
      (ReqArg (\int opts -> opts { flagInterval = Just int }) "MIN")
      "Set the mirroring interval in minutes (default 30)"

  , Option [] ["fail-on-error"]
      (NoArg (\opts -> opts { flagKeepGoing = False }))
      "Fail on mirroring errors."

  , Option [] ["mirror-uploaders"]
      (NoArg (\opts -> opts { flagMirrorUploaders = True }))
      "Mirror the original uploaders which requires that they are already registered on the target hackage."

  , Option [] ["skip-exists"]
      (NoArg (\opts -> opts { flagSkipExists = True }))
      "Skip already mirrored packages (only works for local repos)"
  ]

validateOpts :: [String] -> IO (Verbosity, MirrorOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute mirrorFlagDescrs args
        flags = accum flags0 defaultMirrorFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (configFile:pkgstrs) -> do
        mCfg <- readMirrorConfig configFile
        case mCfg of
          Left theError -> dieNoVerbosity theError
          Right config -> case (mpkgs, minterval) of
            (Left theError, _) -> dieNoVerbosity theError
            (_, Left theError) -> dieNoVerbosity theError
            (Right pkgs, Right interval) ->
              return (flagVerbosity flags, MirrorOpts {
                   mirrorConfig = config,
                   stateDir     = fromMaybe "mirror-cache" (flagCacheDir flags),
                   selectedPkgs = pkgs,
                   continuous   = if flagContinuous flags
                                    then Just interval
                                    else Nothing,
                   mo_keepGoing = flagKeepGoing flags,
                   mirrorUploaders = flagMirrorUploaders flags,
                   skipExists   = flagSkipExists flags
                 })
          where
            mpkgs     = validatePackageIds pkgstrs
            minterval = validateInterval (flagInterval flags)

      _ -> dieNoVerbosity $ "Expected path to a config file.\n"
              ++ "See hackage-mirror --help for details and an example."

  where
    printUsage = do
      putStrLn $ usageInfo usageHeader mirrorFlagDescrs ++ helpExampleStr
      exitSuccess
    usageHeader = helpDescrStr
               ++ "Usage: hackage-mirror configFile [packages] [options]\n"
               ++ "\n"
               ++ "configFile should be a path to a file with format:\n"
               ++ "\n"
               ++ "  source \"name-of-source-repo\"\n"
               ++ "    uri: http://example:port\n"
               ++ "    type: secure\n"
               ++ "  \n"
               ++ "  target \"name-of-target-repo\"\n"
               ++ "    uri: file:/path/to/local/repo\n"
               ++ "    type: local\n"
               ++ "  \n"
               ++ "  post-mirror-hook: \"shell command to execute\"\n"
               ++ "\n"
               ++ "Recognized types are hackage2, secure and local.\n"
               ++ "The post-mirror-hook is optional.\n"
               ++ "\n"
               ++ "Options:"
    printErrors errs = dieNoVerbosity $ concat errs ++ "Try --help."

    accum flags = foldr (flip (.)) id flags

    validateInterval Nothing    = return 30 --default 30 min
    validateInterval (Just str) = do
      int <- case reads str of
               [(int,"")]  -> return int
               [(int,"m")] -> return int
               [(int,"h")] -> return (int * 60)
               _           -> Left ("expected a number of minutes, not '" ++ str ++ "'")
      if int < 0
        then Left "a negative mirroring interval is meaningless"
        else return int

helpDescrStr :: String
helpDescrStr = unlines
  [ "The hackage-mirror client copies packages from one hackage server to another."
  , "By default it copies over all packages that exist on the source but not on"
  , "the destination server. You can also select just specific packages to mirror."
  , "It is also possible to run the mirror in a continuous mode, giving you"
  , "nearly-live mirroring.\n"
  ]

helpExampleStr :: String
helpExampleStr = unlines
  [ "\nExample:"
  , "  Suppose we have:"
  , "  - source server: hackage.haskell.org"
  , "  - dest server:   localhost:8080"
  , "  Uploading packages almost always requires authentication, so suppose we have"
  , "  a user account for our mirror client with username 'foo' and password 'bar'."
  , "  We include the authentication details into the destination URL:"
  , "    http://foo:bar@localhost:8080/"
  , "  To test that it is working without actually syncing a Gb of data from"
  , "  hackage.haskell.org, we will specify to mirror only the 'zlib' package."
  , "  So overall we run:"
  , "    hackage-mirror ./mirror.cfg zlib"
  , "  This will synchronise all versions of the 'zlib' package and then exit."
  ]
