{-# LANGUAGE PatternGuards #-}
module Main where

import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client
import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (intercalate)

import Data.List
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


data Mode = Help [String]
          | Init String
          | Stats
          | Build [PackageId]

data BuildOpts = BuildOpts {
                     bo_verbosity :: Verbosity,
                     bo_stateDir  :: FilePath
                 }

data BuildConfig = BuildConfig {
                       bc_srcURI   :: URI,
                       bc_username :: String,
                       bc_password :: String
                   }

srcName :: BuildConfig -> String
srcName config = fromMaybe (show (bc_srcURI config))
                           (uriHostName (bc_srcURI config))

installDirectory :: BuildOpts -> FilePath
installDirectory bo = bo_stateDir bo </> "inst"

main :: IO ()
main = topHandler $ do
    args <- getArgs
    (mode, opts) <- validateOpts args

    case mode of
        Help strs ->
            do let usageHeader = intercalate "\n" [
                       "Usage: hackage-build init URL [options]",
                       "Usage: hackage-build build [packages] [options]",
                       "Options:"]
               mapM_ putStrLn $ strs
               putStrLn $ usageInfo usageHeader buildFlagDescrs
               unless (null strs) exitFailure
        Init uri -> initialise opts uri
        Stats ->
            do stateDir <- canonicalizePath $ bo_stateDir opts
               let opts' = opts {
                               bo_stateDir = stateDir
                           }
               stats opts'
        Build pkgs ->
            do stateDir <- canonicalizePath $ bo_stateDir opts
               let opts' = opts {
                               bo_stateDir = stateDir
                           }
               buildOnce opts' pkgs


initialise :: BuildOpts -> String -> IO ()
initialise opts uriStr
    = do putStrLn "Enter hackage username"
         username <- getLine
         putStrLn "Enter hackage password"
         password <- getLine
         createDirectoryIfMissing False $ bo_stateDir opts
         -- [Note: Show/Read URI]
         -- Ideally we'd just be showing a BuildConfig, but URI doesn't
         -- have Show/Read, so that doesn't work. So instead, we write
         -- out a tuple containing the uri as a string, and parse it
         -- each time we read it.
         writeFile (configFile opts) (show (uriStr, username, password))

readConfig :: BuildOpts -> IO BuildConfig
readConfig opts = do xs <- readFile $ configFile opts
                     case reads xs of
                         [((uriStr, username, password), "")] ->
                             case validateHackageURI uriStr of
                             -- Shouldn't happen: We check that this
                             -- returns Right when we create the
                             -- config file. See [Note: Show/Read URI].
                             Left theError -> die theError
                             Right uri ->
                                 return $ BuildConfig {
                                              bc_srcURI   = uri,
                                              bc_username = username,
                                              bc_password = password
                                          }
                         _ ->
                             die "Can't parse config file"

configFile :: BuildOpts -> FilePath
configFile opts = bo_stateDir opts </> "hd-config"

data StatResult = AllHaveDocs
                | MostRecentHasDocs
                | SomeHaveDocs
                | NoneHaveDocs
    deriving Eq

stats :: BuildOpts -> IO ()
stats opts = do
    config <- readConfig opts
    let verbosity = bo_verbosity opts
        cacheDir = bo_stateDir opts </> cacheDirName
        cacheDirName | URI { uriAuthority = Just auth } <- bc_srcURI config
                     = makeValid (uriRegName auth ++ uriPort auth)
                     | otherwise
                     = error $ "unexpected URI " ++ show (bc_srcURI config)

    notice verbosity "Initialising"

    createDirectoryIfMissing False cacheDir

    httpSession verbosity $ do
        liftIO $ notice verbosity "Getting index"
        index <- downloadIndex (bc_srcURI config) cacheDir
        let pkgIds = [ pkg_id | PkgIndexInfo pkg_id _ _ _ <- index ]
            checkHasDocs pkgId = do hasDocs <- liftM isJust $ requestGET' (bc_srcURI config <//> "package" </> display pkgId </> "doc/")
                                    return (pkgId, hasDocs)
        liftIO $ notice verbosity "Checking which packages have docs"
        pkgIdsHaveDocs <- mapM checkHasDocs pkgIds
        liftIO $ putStrLn ("Total package versions with docs: " ++
                           show (length (filter snd pkgIdsHaveDocs)))
        let byPackage = map (sortBy (flip (comparing (pkgVersion . fst))))
                      $ groupBy (equating  (pkgName . fst))
                      $ sortBy  (comparing (pkgName . fst)) pkgIdsHaveDocs
            categorise xs@(x : _)
             | and (map snd xs) = AllHaveDocs
             | snd x            = MostRecentHasDocs
             | or  (map snd xs) = SomeHaveDocs
             | otherwise        = NoneHaveDocs
            categorise [] = error "categorise: Can't happen: []"
            categorised = map categorise byPackage
            count c = show (length (filter (c ==) categorised))
            printTable xss
                = let col1Width = maximum (map (length . head) xss)
                      f [x, y] = replicate (col1Width - length x) ' ' ++ x
                              ++ "   " ++ y
                      f _ = error "printTable: Can't happen"
                  in mapM_ (putStrLn . f) xss

        liftIO $ printTable [["Number of packages",    "Category"],
                             [count AllHaveDocs,       "all have docs"],
                             [count MostRecentHasDocs, "most recent has docs"],
                             [count SomeHaveDocs,      "some have docs"],
                             [count NoneHaveDocs,      "none have docs"]]

buildOnce :: BuildOpts -> [PackageId] -> IO ()
buildOnce opts pkgs = do
    config <- readConfig opts
    let verbosity = bo_verbosity opts
        cacheDir = bo_stateDir opts </> cacheDirName
        cacheDirName | URI { uriAuthority = Just auth } <- bc_srcURI config
                     = makeValid (uriRegName auth ++ uriPort auth)
                     | otherwise
                     = error $ "unexpected URI " ++ show (bc_srcURI config)

    notice verbosity "Initialising"
    (does_not_have_docs, mark_as_having_docs, persist) <- mkPackageHasDocs opts config

    createDirectoryIfMissing False cacheDir
    prepareBuildPackages opts config

    flip finally persist $ httpSession verbosity $ do
        -- Make sure we authenticate to Hackage
        setAuthorityGen $ provideAuthInfo (bc_srcURI config)
                        $ Just (bc_username config, bc_password config)

        -- Find those files *not* marked as having documentation in our cache
        liftIO $ notice verbosity "Getting index"
        index <- downloadIndex (bc_srcURI config) cacheDir
        liftIO $ notice verbosity "Checking for packages without docs"
        to_build <- filterM does_not_have_docs
                            [ pkg_id | PkgIndexInfo pkg_id _ _ _ <- index
                            , null pkgs || pkg_id `elem` pkgs]
        liftIO $ notice verbosity $ show (length to_build) ++ " packages to build documentation for."

        -- Try to build each of them, uploading the documentation and
        -- build reports along the way. We mark each package as having
        -- documentation in the cache even if the build fails because
        -- we don't want to keep continually trying to build a failing
        -- package!
        forM_ to_build $ \pkg_id -> do
            buildPackage verbosity opts config pkg_id
            liftIO $ mark_as_having_docs pkg_id

-- Builds a little memoised function that can tell us whether a
-- particular package already has documentation
mkPackageHasDocs :: BuildOpts -> BuildConfig
                 -> IO (PackageId -> HttpSession Bool,
                        PackageId -> IO (),
                        IO ())
mkPackageHasDocs opts config = do
    init_already_built <- readBuiltCache (bo_stateDir opts)
    cache_var <- newIORef init_already_built

    let mark_as_having_docs pkg_id = atomicModifyIORef cache_var $ \already_built -> (S.insert pkg_id already_built, ())
        does_not_have_docs pkg_id = do
            has_docs <- liftIO $ liftM (pkg_id `S.member`) $ readIORef cache_var
            if has_docs
             then return False
             else do
              has_no_docs <- liftM isNothing $ requestGET' (bc_srcURI config <//> "package" </> display pkg_id </> "doc/")
              unless has_no_docs $ liftIO $ mark_as_having_docs pkg_id
              return has_no_docs
        persist = readIORef cache_var >>= writeBuiltCache (bo_stateDir opts)

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


prepareBuildPackages :: BuildOpts -> BuildConfig -> IO ()
prepareBuildPackages opts config
 = withCurrentDirectory (bo_stateDir opts) $ do
    writeFile "cabal-config" $ unlines [
        "remote-repo: " ++ srcName config ++ ":" ++ show (bc_srcURI config <//> "packages" </> "archive"),
        "remote-repo-cache: " ++ installDirectory opts </> "packages",
        "library-for-ghci: False",
        "package-db: " ++ installDirectory opts </> "local.conf.d",
        "documentation: True",
        "remote-build-reporting: detailed",
        -- TODO: These are currently only used for the "upload" commands
        --       only, not "report"
        "username: " ++ bc_username config,
        "password: " ++ bc_password config
      ]

buildPackage :: Verbosity -> BuildOpts -> BuildConfig -> PackageId
             -> HttpSession ()
buildPackage verbosity opts config pkg_id = do
    liftIO $ do notice verbosity ("Building " ++ display pkg_id)
                handleDoesNotExist (return ()) $
                    removeDirectoryRecursive $ installDirectory opts
                createDirectory $ installDirectory opts

                -- Create cache for the empty configuration directory
                let packageConf = installDirectory opts </> "local.conf.d"
                local_conf_d_exists <- doesDirectoryExist packageConf
                unless local_conf_d_exists $ do
                    createDirectory packageConf
                    ph <- runProcess "ghc-pkg"
                                     ["recache",
                                      "--package-conf=" ++ packageConf]
                                     Nothing Nothing Nothing Nothing Nothing
                    -- TODO: Why do we ignore the exit code here?
                    _ <- waitForProcess ph
                    return ()

                -- We don't really want to be running "cabal update"
                -- every time, but the index file and package cache
                -- end up in the same place, so when re remove the
                -- latter we also remove the former
                update_ec <- cabal opts "update" []
                unless (update_ec == ExitSuccess) $
                    die "Could not 'cabal update' from specified server"

    -- The documentation is installed within the stateDir because we
    -- set a prefix while installing
    let doc_root = installDirectory opts </> "share" </> "doc"
        doc_dir      = doc_root </> display pkg_id </> "html"
        temp_doc_dir = doc_root </> display pkg_id </> display (pkgName pkg_id)
        --versionless_pkg_url = srcURI opts <//> "package" </> "$pkg"
        pkg_url = bc_srcURI config <//> "package" </> "$pkg-$version"

    mb_docs <- liftIO $ withCurrentDirectory (installDirectory opts) $ do
        -- We CANNOT build from an unpacked directory, because Cabal
        -- only generates build reports if you are building from a
        -- tarball that was verifiably downloaded from the server
        -- TODO: Why do we ignore the result code here?
        void $ cabal opts "install"
                     ["--enable-documentation",
                      "--enable-tests",
                      -- We don't want packages installed in the user
                      -- package.conf to affect things. In particular,
                      -- we don't want doc building to fail because
                      -- "packages are likely to be broken by the reinstalls"
                      "--ghc-pkg-option", "--no-user-package-conf",
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
                      "--prefix=" ++ installDirectory opts,
                      display pkg_id]

        -- Submit a report even if installation/tests failed: all
        -- interesting data points!
        report_ec <- cabal opts "report"
                           ["--username", bc_username config,
                            "--password", bc_password config]

        -- Delete reports after submission because we don't want to
        -- submit them *again* in the future
        when (report_ec == ExitSuccess) $ do
            -- This seems like a bit of a mess: some data goes into the
            -- user directory:
            dotCabal <- getAppUserDataDirectory "cabal"
            handleDoesNotExist (return ()) $ removeDirectoryRecursive (dotCabal </> "reports" </> srcName config)
            handleDoesNotExist (return ()) $ removeDirectoryRecursive (dotCabal </> "logs")
            -- Other data goes into a local file storing build reports:
            let simple_report_log = installDirectory opts </> "packages" </> srcName config </> "build-reports.log"
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
            requestPUT (bc_srcURI config <//> "package" </> display pkg_id </> "doc") "application/x-gzip" docs_tgz

cabal :: BuildOpts -> String -> [String] -> IO ExitCode
cabal opts cmd args = do
    cwd' <- getCurrentDirectory
    let verbosity = bo_verbosity opts
        cabalConfigFile = bo_stateDir opts </> "cabal-config"
        verbosityArgs = if verbosity == silent
                        then ["-v0"]
                        else []
        all_args = ("--config-file=" ++ cabalConfigFile)
                 : cmd
                 : verbosityArgs
                ++ args
    notice verbosity ("In " ++ cwd' ++ ":\n" ++
                      showCommandForUser "cabal" all_args)
    ph <- runProcess "cabal" all_args (Just cwd')
                     Nothing Nothing Nothing Nothing
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

  , Option ['s'] []
      (NoArg (\opts -> opts { flagVerbosity = silent }))
      "Silent mode"

  , Option ['v'] []
      (NoArg (\opts -> opts { flagVerbosity = moreVerbose (flagVerbosity opts) }))
      "Verbose mode (can be listed multiple times e.g. -vv)"

  , Option [] ["cache-dir"]
      (ReqArg (\dir opts -> opts { flagCacheDir = Just dir }) "DIR")
      "Where to put files during building"
  ]

validateOpts :: [String] -> IO (Mode, BuildOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute buildFlagDescrs args
        flags = accum flags0 emptyBuildFlags

        stateDir = fromMaybe "build-cache" (flagCacheDir flags)

        opts = BuildOpts {
                   bo_verbosity = flagVerbosity flags,
                   bo_stateDir  = stateDir
               }

        mode = case args' of
               _ | flagHelp flags -> Help []
                 | not (null errs) -> Help errs
               ["init", uri] ->
                   -- We don't actually want the URI at this point
                   -- (see [Note: Show/Read URI])
                   case validateHackageURI uri of
                   Left  theError -> Help [theError]
                   Right _        -> Init uri
               "init" : _ ->
                   Help ["init takes a single argument (repo URL)"]
               ["stats"] ->
                   Stats
               "stats" : _ ->
                   Help ["stats takes no arguments"]
               "build" : pkgstrs ->
                   case validatePackageIds pkgstrs of
                   Left  theError -> Help [theError]
                   Right pkgs     -> Build pkgs
               cmd : _ -> Help ["Unrecognised command: " ++ show cmd]
               [] -> Help []

    -- Ensure we store the absolute state_dir, because we might
    -- change the CWD later and we don't want the stateDir to be
    -- invalidated by such a change
    --
    -- We have to ensure the directory exists before we do
    -- canonicalizePath, or otherwise we get an exception if it
    -- does not yet exist

    return (mode, opts)

  where

    accum flags = foldr (flip (.)) id flags


handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist handler act
    = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                 (\() -> handler)
                 act

