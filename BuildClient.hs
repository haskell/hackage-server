{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client
import Distribution.Client.Cron (cron, rethrowSignalsAsExceptions, Signal(..))

import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (intercalate)

import Data.List
import Data.Maybe
import Data.IORef
import Data.Time
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BS
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

import Data.Aeson (eitherDecode)

data Mode = Help [String]
          | Init String
          | Stats
          | Build [PackageId]

data BuildOpts = BuildOpts {
                     bo_verbosity  :: Verbosity,
                     bo_runTime    :: Maybe NominalDiffTime,
                     bo_stateDir   :: FilePath,
                     bo_force      :: Bool,
                     bo_continuous :: Maybe Int
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
    rethrowSignalsAsExceptions [SIGTERM, SIGKILL]
    args <- getArgs
    (mode, opts) <- validateOpts args

    case mode of
        Help strs ->
            do let usageHeader = intercalate "\n" [
                       "Usage: hackage-build init URL [options]",
                       "       hackage-build build [packages] [options]",
                       "       hackage-build stats",
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
               case bo_continuous opts' of
                 Nothing ->
                   buildOnce opts' pkgs
                 Just interval -> do
                   cron (bo_verbosity opts')
                        interval
                        (const (buildOnce opts' pkgs))
                        ()

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

data StatResult = AllVersionsBuiltOk
                | AllVersionsAttempted
                | NoneBuilt
                | SomeBuiltOk
                | SomeFailed
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
    (didFail, _, _)  <- mkPackageFailed opts

    httpSession verbosity $ do
      liftIO $ notice verbosity "Getting index"
      infoStats verbosity (Just statsFile) =<< getDocumentationStats config didFail
  where
    statsFile = bo_stateDir opts </> "stats"

infoStats :: MonadIO m => Verbosity -> Maybe FilePath -> [(PackageIdentifier, HasDocs)] -> m ()
infoStats verbosity mDetailedStats pkgIdsHaveDocs = liftIO $ do
    nfo $ "There are "
       ++ show (length byPackage)
       ++ " packages with a total of "
       ++ show (length pkgIdsHaveDocs)
       ++ " package versions"
    nfo $ "So far we have built or attempted to built "
       ++ show (length (filter ((/= DocsNotBuilt) . snd) pkgIdsHaveDocs))
       ++ " packages; only "
       ++ show (length (filter ((== DocsNotBuilt) . snd) pkgIdsHaveDocs))
       ++ " left!"

    nfo "Considering the most recent version only:"
    nfo . printTable . indent $ [
        [show (length mostRecentBuilt)   , "built succesfully"]
      , [show (length mostRecentFailed)  , "failed to build"]
      , [show (length mostRecentNotBuilt), "not yet built"]
      ]

    nfo "Considering all versions:"
    nfo . printTable . indent $ [
        [count AllVersionsBuiltOk,   "all versions built successfully"]
      , [count AllVersionsAttempted, "attempted to build all versions, but some failed"]
      , [count SomeBuiltOk,          "not all versions built yet, but those that did were ok"]
      , [count SomeFailed,           "not all versions built yet, and some failures"]
      , [count NoneBuilt,            "no versions built yet"]
      ]

    case mDetailedStats of
      Nothing        -> return ()
      Just statsFile -> do
        writeFile statsFile $ printTable (["Package", "Version", "Has docs?"] : formattedStats)
        notice verbosity $ "Detailed statistics written to " ++ statsFile
  where
    -- | We avoid 'info' here because it re-wraps the text
    nfo :: String -> IO ()
    nfo str = when (verbosity >= verbose) $ putStrLn str

    byPackage :: [[(PackageId, HasDocs)]]
    byPackage = map (sortBy (flip (comparing (pkgVersion . fst))))
              $ groupBy (equating  (pkgName . fst))
              $ sortBy  (comparing (pkgName . fst)) pkgIdsHaveDocs

    mostRecentBuilt, mostRecentFailed, mostRecentNotBuilt :: [[(PackageId, HasDocs)]]
    mostRecentBuilt    = filter ((== HasDocs)      . snd . head) byPackage
    mostRecentFailed   = filter ((== DocsFailed)   . snd . head) byPackage
    mostRecentNotBuilt = filter ((== DocsNotBuilt) . snd . head) byPackage

    categorise :: [(PackageId, HasDocs)] -> StatResult
    categorise ps
      | all (== HasDocs)      hd = AllVersionsBuiltOk
      | all (/= DocsNotBuilt) hd = AllVersionsAttempted
      | all (== DocsNotBuilt) hd = NoneBuilt
      | all (/= DocsFailed)   hd = SomeBuiltOk
      | otherwise                = SomeFailed
      where
        hd = map snd ps

    categorised :: [StatResult]
    categorised = map categorise byPackage

    count :: StatResult -> String
    count c = show (length (filter (c ==) categorised))

    formatVersion :: (PackageId, HasDocs) -> [String]
    formatVersion (version, hasDocs) =
      [ display (pkgVersion version)
      , show hasDocs
      ]

    formatPkg :: [(PackageId, HasDocs)] -> [[String]]
    formatPkg ((firstVersion, firstHasDocs) : otherVersions) =
        (display (pkgName firstVersion) : formatVersion (firstVersion, firstHasDocs))
      : (indent (map formatVersion otherVersions))
    formatPkg _ = error "formatPkg: cannot happen"

    formattedStats :: [[String]]
    formattedStats = concatMap formatPkg byPackage

    indent :: [[String]] -> [[String]]
    indent = map ("  " :)

-- | Formats a 2D table so that everything is nicely aligned
--
-- NOTE: Expects the same number of columns in every row!
printTable :: [[String]] -> String
printTable xss = intercalate "\n"
               . map (intercalate " ")
               . map padCols
               $ xss
  where
    colWidths :: [[Int]]
    colWidths = map (map length) $ xss

    maxColWidths :: [Int]
    maxColWidths = foldr1 (\xs ys -> map (uncurry max) (zip xs ys)) colWidths

    padCols :: [String] -> [String]
    padCols cols = map (uncurry padTo) (zip maxColWidths cols)

    padTo :: Int -> String -> String
    padTo len str = str ++ replicate (len - length str) ' '

data HasDocs = HasDocs | DocsNotBuilt | DocsFailed
  deriving (Eq, Show)

getDocumentationStats :: BuildConfig
                      -> (PackageId -> IO Bool)
                      -> HttpSession [(PackageIdentifier, HasDocs)]
getDocumentationStats config didFail = do
    mJSON <- requestGET' uri
    case mJSON of
      Nothing   -> fail $ "Could not download " ++ show uri
      Just json -> case eitherDecode json of
                     Left e    -> fail $ "Could not decode " ++ show uri ++ ": " ++ e
                     Right val -> liftIO $ mapM aux val
  where
    uri = bc_srcURI config <//> "packages" </> "docs.json"

    aux :: (String, Bool) -> IO (PackageIdentifier, HasDocs)
    aux (pkgId, docsBuilt) = do
      let pkgId' = fromJust (simpleParse pkgId)
      if docsBuilt
        then return (pkgId', HasDocs)
        else do failed <- didFail pkgId'
                if failed then return (pkgId', DocsFailed)
                          else return (pkgId', DocsNotBuilt)

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
    (has_failed, mark_as_failed, persist_failed) <- mkPackageFailed opts

    createDirectoryIfMissing False cacheDir

    configFileExists <- doesFileExist (bo_stateDir opts </> "cabal-config")
    unless configFileExists $ prepareBuildPackages opts config

    flip finally persist_failed $ do
        pkgIdsHaveDocs <- httpSession verbosity $
          getDocumentationStats config has_failed
        infoStats verbosity Nothing pkgIdsHaveDocs

        -- First build all of the latest versions of each package
        -- Then go back and build all the older versions
        -- NOTE: assumes all these lists are non-empty
        let latestFirst :: [[(PackageId, a)]] -> [(PackageId, a)]
            latestFirst ids = map head ids ++ concatMap tail ids

        -- Find those files *not* marked as having documentation in our cache
        let toBuild = map fst
                    . filter shouldBuild
                    . latestFirst
                    . map (sortBy (flip (comparing (pkgVersion . fst))))
                    . groupBy (equating  (pkgName . fst))
                    . sortBy  (comparing (pkgName . fst))
                    $ pkgIdsHaveDocs

        liftIO $ notice verbosity $ show (length toBuild) ++ " package(s) to build"

        -- Try to build each of them, uploading the documentation and
        -- build reports along the way. We mark each package as having
        -- documentation in the cache even if the build fails because
        -- we don't want to keep continually trying to build a failing
        -- package!
        startTime <- liftIO $ getCurrentTime

        let go [] = return ()
            go (pkg_id : toBuild') = do
              failed <- liftIO $ has_failed pkg_id
              if failed
                then liftIO . notice verbosity $ "Skipping " ++ display pkg_id
                                              ++ " because it failed to built previously"
                else do
                  mTgz <- buildPackage verbosity opts config pkg_id
                  case mTgz of
                    Nothing ->
                      liftIO $ mark_as_failed pkg_id
                    Just docs_tgz -> httpSession verbosity $ do
                      -- Make sure we authenticate to Hackage
                      setAuthorityGen $ provideAuthInfo (bc_srcURI config)
                                      $ Just (bc_username config, bc_password config)
                      requestPUT (bc_srcURI config <//> "package" </> display pkg_id </> "docs") "application/x-tar" (Just "gzip") docs_tgz

              -- We don't check the runtime until we've actually tried
              -- to build a doc, so as to ensure we make progress.
              outOfTime <- case bo_runTime opts of
                  Nothing -> return False
                  Just d  -> liftIO $ do
                    currentTime <- getCurrentTime
                    return $ (currentTime `diffUTCTime` startTime) > d

              if outOfTime then return ()
                           else go toBuild'

        go toBuild
  where
    shouldBuild :: (PackageIdentifier, HasDocs) -> Bool
    shouldBuild (_,     HasDocs)      = forceBuild
    shouldBuild (_,     DocsFailed)   = False
    shouldBuild (pkgId, DocsNotBuilt) = buildingAll || pkgId `elem` pkgs

    buildingAll = null pkgs
    forceBuild  = bo_force opts && not buildingAll

-- Builds a little memoised function that can tell us whether a
-- particular package failed to build its documentation
mkPackageFailed :: BuildOpts
                -> IO (PackageId -> IO Bool, PackageId -> IO (), IO ())
mkPackageFailed opts = do
    init_failed <- readFailedCache (bo_stateDir opts)
    cache_var   <- newIORef init_failed

    let mark_as_failed pkg_id = atomicModifyIORef cache_var $ \already_failed ->
                                  (S.insert pkg_id already_failed, ())
        has_failed     pkg_id = liftM (pkg_id `S.member`) $ readIORef cache_var
        persist               = readIORef cache_var >>= writeFailedCache (bo_stateDir opts)

    return (has_failed, mark_as_failed, persist)
  where
    readFailedCache :: FilePath -> IO (S.Set PackageId)
    readFailedCache cache_dir = do
        pkgstrs <- handleDoesNotExist (return []) $ liftM lines $ readFile (cache_dir </> "failed")
        case validatePackageIds pkgstrs of
            Left theError -> die theError
            Right pkgs -> return (S.fromList pkgs)

    writeFailedCache :: FilePath -> S.Set PackageId -> IO ()
    writeFailedCache cache_dir pkgs =
      writeFile (cache_dir </> "failed") $ unlines $ map display $ S.toList pkgs

prepareBuildPackages :: BuildOpts -> BuildConfig -> IO ()
prepareBuildPackages opts config
 = withCurrentDirectory (bo_stateDir opts) $ do
    writeFile "cabal-config" $ unlines [
        "remote-repo: " ++ srcName config ++ ":" ++ show (bc_srcURI config <//> "packages" </> "archive"),
        "remote-repo-cache: " ++ installDirectory opts </> "packages",
        "logs-dir: " ++ installDirectory opts </> "logs",
        "library-for-ghci: False",
        "package-db: " ++ installDirectory opts </> "local.conf.d",
        "documentation: True",
        "remote-build-reporting: detailed",
        -- TODO: These are currently only used for the "upload" commands
        --       only, not "report"
        "username: " ++ bc_username config,
        "password: " ++ bc_password config
      ]

-- | Build documentation and return @(Just tgz)@ for the built tgz file
-- on success, or @Nothing@ otherwise.
buildPackage :: MonadIO m
             => Verbosity -> BuildOpts -> BuildConfig -> PackageId -> m (Maybe BS.ByteString)
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
        temp_doc_dir = doc_root </> display pkg_id </> display pkg_id ++ "-docs"
        --versionless_pkg_url = srcURI opts <//> "package" </> "$pkg"
        pkg_url = bc_srcURI config <//> "package" </> "$pkg-$version"

    liftIO $ withCurrentDirectory (installDirectory opts) $ do
        -- We CANNOT build from an unpacked directory, because Cabal
        -- only generates build reports if you are building from a
        -- tarball that was verifiably downloaded from the server

        -- We ignore the result of calling @cabal install@ because
        -- @cabal install@ succeeds even if the documentation fails to build.
        void $ cabal opts "install"
                     ["--enable-documentation",
                      -- We only care about docs, so we want to build as
                      -- quickly as possible, and hence turn
                      -- optimisation off. Also explicitly pass -O0 as a
                      -- GHC option, in case it overrides a .cabal
                      -- setting or anything
                      "--disable-optimization", "--ghc-option", "-O0",
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

        docs_generated <- liftM2 (&&) (doesDirectoryExist doc_dir)
                                      (doesFileExist (doc_dir </> "doc-index.html"))
        if docs_generated
            then do
                notice verbosity $ "Docs generated for " ++ display pkg_id
                liftM Just $
                    -- We need the files in the documentation .tar.gz
                    -- to have paths like foo-x.y.z-docs/index.html
                    -- Unfortunately, on disk they have paths like
                    -- foo-x.y.z/html/index.html. This hack resolves
                    -- the problem:
                    bracket_ (renameDirectory doc_dir      temp_doc_dir)
                             (renameDirectory temp_doc_dir doc_dir)
                             (tarGzDirectory temp_doc_dir)
            else do
              notice verbosity $ "Docs for " ++ display pkg_id ++ " failed to build"
              return Nothing

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
    notice verbosity $ "In " ++ cwd' ++ ":\n" ++ unwords ("cabal":all_args)
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
    flagCacheDir   :: Maybe FilePath,
    flagVerbosity  :: Verbosity,
    flagRunTime    :: Maybe NominalDiffTime,
    flagHelp       :: Bool,
    flagForce      :: Bool,
    flagContinuous :: Bool,
    flagInterval   :: Maybe String
}

emptyBuildFlags :: BuildFlags
emptyBuildFlags = BuildFlags {
    flagCacheDir   = Nothing
  , flagVerbosity  = normal
  , flagRunTime    = Nothing
  , flagHelp       = False
  , flagForce      = False
  , flagContinuous = False
  , flagInterval   = Nothing
  }

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

  , Option [] ["run-time"]
      (ReqArg (\mins opts -> case reads mins of
                             [(mins', "")] -> opts { flagRunTime = Just (fromInteger mins' * 60) }
                             _ -> error "Can't parse minutes") "MINS")
      "Limit the running time of the build client"

  , Option [] ["cache-dir"]
      (ReqArg (\dir opts -> opts { flagCacheDir = Just dir }) "DIR")
      "Where to put files during building"

  , Option [] ["force"]
      (NoArg (\opts -> opts { flagForce = True }))
      "Force rebuilding (of specified packages)"

  , Option [] ["continuous"]
      (NoArg (\opts -> opts { flagContinuous = True }))
      "Mirror continuously rather than just once."

  , Option [] ["interval"]
      (ReqArg (\int opts -> opts { flagInterval = Just int }) "MIN")
      "Set the mirroring interval in minutes (default 30)"
  ]

validateOpts :: [String] -> IO (Mode, BuildOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute buildFlagDescrs args
        flags = accum flags0 emptyBuildFlags

        stateDir = fromMaybe "build-cache" (flagCacheDir flags)

        opts = BuildOpts {
                   bo_verbosity  = flagVerbosity flags,
                   bo_runTime    = flagRunTime flags,
                   bo_stateDir   = stateDir,
                   bo_force      = flagForce flags,
                   bo_continuous = case (flagContinuous flags, flagInterval flags) of
                                     (True, Just i)  -> Just (read i)
                                     (True, Nothing) -> Just 30 -- default interval
                                     (False, _)      -> Nothing
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

