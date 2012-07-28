{-# LANGUAGE PatternGuards #-}
module Main where

import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client
import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils

import Data.Maybe
import Data.IORef
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set as S

import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar

import System.Environment
import System.Exit
import System.FilePath
import System.Directory
import System.Console.GetOpt
import System.Process
import System.IO.Error


-- FIXMEs:
--  * Sandbox user-supplied code (i.e. the whole 'cabal install' invocation)


data BuildOpts = BuildOpts {
                    srcURI       :: URI,
                    username     :: String,
                    password     :: String,
                    stateDir     :: FilePath,
                    selectedPkgs :: [PackageId]
                  }

srcName :: BuildOpts -> String
srcName opts = fromMaybe (show (srcURI opts)) (uriHostName (srcURI opts))


main :: IO ()
main = topHandler $ do
    args <- getArgs
    (verbosity, opts) <- validateOpts args

    buildOnce verbosity opts


buildOnce :: Verbosity -> BuildOpts -> IO ()
buildOnce verbosity opts = do
    (does_not_have_docs, mark_as_having_docs, persist) <- mkPackageHasDocs opts

    createDirectoryIfMissing False cacheDir
    prepareBuildPackages verbosity opts

    flip finally persist $ httpSession verbosity $ do
        -- Make sure we authenticate to Hackage
        setAuthorityGen $ provideAuthInfo (srcURI opts) $ Just (username opts, password opts)

        -- Find those files *not* marked as having documentation in our cache
        index <- downloadIndex (srcURI opts) cacheDir
        to_build <- filterM does_not_have_docs
                            [pkg_id | PkgIndexInfo pkg_id _ _ _ <- index
                            , null (selectedPkgs opts) ||
                              pkg_id `elem` selectedPkgs opts]
        liftIO $ notice verbosity $ show (length to_build) ++ " packages to build documentation for."
      
        -- Try to build each of them, uploading the documentation and
        -- build reports along the way. We mark each package as having
        -- documentation in the cache even if the build fails because
        -- we don't want to keep continually trying to build a failing
        -- package!
        forM_ to_build $ \pkg_id -> do
            buildPackage verbosity opts pkg_id
            liftIO $ mark_as_having_docs pkg_id
  where
    cacheDir = stateDir opts </> cacheDirName
    cacheDirName | URI { uriAuthority = Just auth } <- srcURI opts
                 = makeValid (uriRegName auth ++ uriPort auth)
                 | otherwise
                 = error $ "unexpected URI " ++ show (srcURI opts)

-- Builds a little memoised function that can tell us whether a
-- particular package already has documentation
mkPackageHasDocs :: BuildOpts
                 -> IO (PackageId -> HttpSession Bool,
                        PackageId -> IO (),
                        IO ())
mkPackageHasDocs opts = do
    init_already_built <- readBuiltCache (stateDir opts)
    cache_var <- newIORef init_already_built

    let mark_as_having_docs pkg_id = atomicModifyIORef cache_var $ \already_built -> (S.insert pkg_id already_built, ())
        does_not_have_docs pkg_id = do
            has_docs <- liftIO $ liftM (pkg_id `S.member`) $ readIORef cache_var
            if has_docs
             then return False
             else do
              has_no_docs <- liftM isNothing $ requestGET' (srcURI opts <//> "package" </> display pkg_id </> "doc")
              unless has_no_docs $ liftIO $ mark_as_having_docs pkg_id
              return has_no_docs
        persist = readIORef cache_var >>= writeBuiltCache (stateDir opts)

    return (does_not_have_docs, mark_as_having_docs, persist)
  where
    readBuiltCache :: FilePath -> IO (S.Set PackageId)
    readBuiltCache cache_dir = do
        pkgstrs <- handleDoesNotExist (return []) $ liftM lines $ readFile (cache_dir </> "built")
        case validatePackageIds pkgstrs of
            Left theError -> die theError
            Right pkgs -> return (S.fromList pkgs)
    
    writeBuiltCache :: FilePath -> S.Set PackageId -> IO ()
    writeBuiltCache cache_dir pkgs = writeFile (cache_dir </> "built") $ unlines $ map display $ S.toList pkgs


prepareBuildPackages :: Verbosity -> BuildOpts -> IO ()
prepareBuildPackages verbosity opts = withCurrentDirectory (stateDir opts) $ do
    writeFile "config" $ unlines [
        "remote-repo: " ++ srcName opts ++ ":" ++ show (srcURI opts <//> "packages" </> "archive"),
        "remote-repo-cache: " ++ stateDir opts </> "packages",
        "library-for-ghci: False",
        "package-db: " ++ stateDir opts </> "local.conf.d",
        "documentation: True",
        "remote-build-reporting: detailed",
        -- Used for the "upload" commands only, not "report" :-(
        "username: " ++ username opts,
        "password: " ++ password opts
      ]

    -- Create cache for the empty configuration directory
    local_conf_d_exists <- doesDirectoryExist "local.conf.d"
    unless local_conf_d_exists $ do
        createDirectory "local.conf.d"
        let packageConf = stateDir opts </> "local.conf.d"
        ph <- runProcess "ghc-pkg"
                         ["recache", "--package-conf=" ++ packageConf]
                         Nothing Nothing Nothing Nothing Nothing
        -- TODO: Why do we ignore the exit code here?
        _ <- waitForProcess ph
        return ()
    
    update_ec <- cabal verbosity opts "update" []
    unless (update_ec == ExitSuccess) $
        die "Could not 'cabal update' from specified server"


buildPackage :: Verbosity -> BuildOpts -> PackageId -> HttpSession ()
buildPackage verbosity opts pkg_id = do
    -- The documentation is installed within the stateDir because we
    -- set a prefix while installing
    let doc_root = stateDir opts </> "share" </> "doc"
        doc_dir      = doc_root </> display pkg_id </> "html"
        temp_doc_dir = doc_root </> display pkg_id </> display (pkgName pkg_id)
        --versionless_pkg_url = srcURI opts <//> "package" </> "$pkg"
        pkg_url = srcURI opts <//> "package" </> "$pkg-$version"

    mb_docs <- liftIO $ withCurrentDirectory (stateDir opts) $ do
        -- Let's not clean in between installations. This should save us
        -- some download/installation time for packages that are
        -- depended on by a lot of things, at the cost of some disk
        -- space.
        -- handleDoesNotExist (return ()) $ removeDirectoryRecursive "packages"
        -- handleDoesNotExist (return ()) (removeDirectoryRecursive "local.conf.d")
        -- createDirectoryIfMissing False "local.conf.d"

        -- We CANNOT build from an unpacked directory, because Cabal
        -- only generates build reports if you are building from a
        -- tarball that was verifiably downloaded from the server
        -- TODO: Why do we ignore the result code here?
        void $ cabal verbosity opts "install"
                     ["--enable-documentation",
                      "--enable-tests",
                      -- We know where this documentation will
                      -- eventually be hosted, bake that in.
                      -- The wiki claims we shouldn't include the
                      -- version in the hyperlinks so we don't have
                      -- to rehaddock some package when the dependent
                      -- packages get updated. However, this is NOT
                      -- what the Hackage v1 did, so ignore that:
                      "--haddock-html-location=" ++ show (pkg_url <//> "doc"),
                      -- Give the user a choice between themes:
                      "--haddock-option=--built-in-themes",
                      -- Link "Contents" to the package page:
                      "--haddock-contents-location=" ++ show pkg_url,
                      -- Link to colourised source code:
                      "--haddock-hyperlink-source",
                      "--prefix=" ++ stateDir opts,
                      display pkg_id]

        -- Submit a report even if installation/tests failed: all
        -- interesting data points!
        report_ec <- cabal verbosity opts "report"
                           ["--username", username opts,
                            "--password", password opts]

        -- Delete reports after submission because we don't want to
        -- submit them *again* in the future
        when (report_ec == ExitSuccess) $ do
            -- This seems like a bit of a mess: some data goes into the
            -- user directory:
            dotCabal <- getAppUserDataDirectory "cabal"
            handleDoesNotExist (return ()) $ removeDirectoryRecursive (dotCabal </> "reports" </> srcName opts)
            handleDoesNotExist (return ()) $ removeDirectoryRecursive (dotCabal </> "logs")
            -- Other data goes into a local file storing build reports:
            let simple_report_log = stateDir opts </> "packages" </> srcName opts </> "build-reports.log"
            handleDoesNotExist (return ()) $ removeFile simple_report_log
      
        docs_generated <- doesDirectoryExist doc_dir
        if docs_generated
            then do
                notice verbosity $ "Docs generated for " ++ display pkg_id
                liftM Just $
                    -- We need the files in the documentation .tar.gz
                    -- to have paths like foo/index.html
                    -- Unfortunately, on disk they have paths like
                    -- foo-x.y.z/html/index.html. This hack resolves
                    -- the problem:
                    bracket_ (renameDirectory doc_dir      temp_doc_dir)
                             (renameDirectory temp_doc_dir doc_dir)
                             (tarGzDirectory temp_doc_dir)
            else return Nothing

    -- Submit the generated docs, if possible
    case mb_docs of
        Nothing       -> return ()
        Just docs_tgz -> do
            requestPUT (srcURI opts <//> "package" </> display pkg_id </> "doc") "application/x-gzip" docs_tgz

cabal :: Verbosity -> BuildOpts -> String -> [String] -> IO ExitCode
cabal verbosity opts cmd args = do
    cwd' <- getCurrentDirectory
    let all_args = ("--config-file=" ++ stateDir opts </> "config"):cmd:args
    notice verbosity ("In " ++ cwd' ++ ":\n" ++
                      showCommandForUser "cabal" all_args)
    ph <- runProcess "cabal" all_args (Just cwd') Nothing Nothing Nothing Nothing
    waitForProcess ph

tarGzDirectory :: FilePath -> IO BS.ByteString
tarGzDirectory dir = do
    res <- liftM (GZip.compress . Tar.write) $
               Tar.pack containing_dir [nested_dir]
    -- This seq is extremely important! Tar.pack is lazy, scanning
    -- directories as entries are demanded.
    -- This interacts very badly with the renameDirectory stuff with
    -- which tarGzDirectory gets wrapped.
    BS.length res `seq` return res
  where (containing_dir, nested_dir) = splitFileName dir

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory cwd1 act
    = bracket (do cwd2 <- getCurrentDirectory
                  setCurrentDirectory cwd1
                  return cwd2)
              setCurrentDirectory
              (const act)


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
        Left theError -> die theError
        Right fromURI -> do
          pkgs <- case validatePackageIds pkgstrs of
            Left theError -> die theError
            Right pkgs -> return pkgs

          (usrnme, psswrd) <- case extractURICredentials fromURI of
            Nothing    -> die "No Hackage server username/password"
            Just creds -> return creds

          -- Ensure we store the absolute state_dir, because we might
          -- change the CWD later and we don't want the stateDir to be
          -- invalidated by such a change
          --
          -- We have to ensure the directory exists before we do
          -- canonicalizePath, or otherwise we get an exception if it
          -- does not yet exist
          let rel_state_dir = fromMaybe "build-cache" (flagCacheDir flags)
          createDirectoryIfMissing False rel_state_dir
          abs_state_dir <- canonicalizePath rel_state_dir

          return $ (,) (flagVerbosity flags) BuildOpts {
            srcURI       = removeURICredentials fromURI,
            username     = usrnme,
            password     = psswrd,
            stateDir     = abs_state_dir,
            selectedPkgs = pkgs
          }

      _ -> do putStrLn "expected a URL: the Hackage server to build for"
              printUsage

  where
    printUsage = do
        putStrLn $ usageInfo usageHeader buildFlagDescrs
        exitSuccess
    usageHeader = "Usage: hackage-build URL [packages] [options]\nOptions:"
    printErrors errs = do
        putStrLn $ concat errs ++ "Try --help."
        exitFailure

    accum flags = foldr (flip (.)) id flags


handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist handler act
    = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                 (\() -> handler)
                 act

