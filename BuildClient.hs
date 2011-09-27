{-# LANGUAGE PatternGuards #-}
module Main where

import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client
import Distribution.Server.Util.Merge
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set as S

import System.Environment
import System.Exit
import System.FilePath
import System.Directory
import System.Console.GetOpt


data BuildOpts = BuildOpts {
                    srcURI       :: URI,
                    stateDir     :: FilePath,
                    selectedPkgs :: [PackageId]
                  }

main :: IO ()
main = topHandler $ do
    args <- getArgs
    (verbosity, opts) <- validateOpts args

    buildOnce verbosity opts


buildOnce :: Verbosity -> BuildOpts -> IO ()
buildOnce verbosity opts = do
    createDirectoryIfMissing False (stateDir opts)

    already_built <- readBuiltCache (stateDir opts)

    built_ok <- httpSession verbosity $ do
      index <- downloadIndex (srcURI opts) cacheFile
      
      let to_build = [pkg_id | PkgIndexInfo pkg_id _ _ _ <- index, pkg_id `S.notMember` already_built]
      ioAction $ notice verbosity $ show (length to_build) ++ " packages to build."
      
      setAuthorityGen $ provideAuthInfo (srcURI opts) $ extractURICredentials (srcURI opts)
      filterM (buildPackage verbosity opts) to_build
   
    writeBuiltCache (stateDir opts) (S.fromList built_ok `S.union` already_built)
  where
    cacheFile = stateDir opts </> cacheFileName
    cacheFileName | URI { uriAuthority = Just auth } <- srcURI opts
                  = makeValid (uriRegName auth ++ uriPort auth)
                  | otherwise
                  = error $ "unexpected URI " ++ show (srcURI opts)

readBuiltCache :: FilePath -> IO (S.Set PackageId)
readBuiltCache cache_dir = do
    pkgstrs <- liftM lines $ readFile (cache_dir </> "built")
    case validatePackageIds pkgstrs of
        Left err   -> die err
        Right pkgs -> return (S.fromList pkgs)

writeBuiltCache :: FilePath -> S.Set PackageId -> IO ()
writeBuiltCache cache_dir pkgs = writeFile (cache_dir </> "built") $ unlines $ map show $ S.toList pkgs


-- Returns True if we uploaded a build report and documentation for the package successfully
buildPackage :: Verbosity -> BuildOpts -> PackageId -> HttpSession Bool
buildPackage verbosity opts pkg_id = do
    out "Not implemented yet!"
    return False


-------------------------
-- Command line handling
-------------------------

data BuildFlags = BuildFlags {
    flagCacheDir  :: Maybe FilePath,
    flagVerbosity :: Verbosity,
    flagHelp      :: Bool
}

emptyBuildFlags :: BuildFlags
emptyBuildFlags = BuildFlags Nothing normal False

buildFlagDescrs :: [OptDescr (BuildFlags -> BuildFlags)]
buildFlagDescrs =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { flagHelp = True }))
      "Show this help text"

  , Option ['v'] []
      (NoArg (\opts -> opts { flagVerbosity = moreVerbose (flagVerbosity opts) }))
      "Verbose mode (can be listed multiple times e.g. -vv)"

  , Option [] ["cache-dir"]
      (ReqArg (\dir opts -> opts { flagCacheDir = Just dir }) "DIR")
      "Where to put files during building"
  ]

validateOpts :: [String] -> IO (Verbosity, BuildOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute buildFlagDescrs args
        flags = accum flags0 emptyBuildFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (from:pkgstrs) -> case validateHackageURI from of
        Left err -> die err
        Right fromURI -> do
          pkgs <- case validatePackageIds pkgstrs of
            Left err   -> die err
            Right pkgs -> return pkgs

          return $ (,) (flagVerbosity flags) BuildOpts {
            srcURI       = fromURI,
            stateDir     = fromMaybe "build-cache" (flagCacheDir flags),
            selectedPkgs = pkgs
          }

      _ -> do putStrLn "expected a URL: the Hackage server to build for"
              printUsage

  where
    printUsage  = do
      putStrLn $ usageInfo usageHeader buildFlagDescrs
      exitSuccess
    usageHeader = "Usage: hackage-build URL [packages] [options]\nOptions:"
    printErrors errs = do
      putStrLn $ concat errs ++ "Try --help."
      exitFailure

    accum flags = foldr (flip (.)) id flags
