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
import Distribution.Version (Version)

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
import System.IO
import System.IO.Error

import Data.Aeson (eitherDecode)

data Mode = Help [String]
          | Init String [String]
          | Stats
          | Build [PackageId]

data BuildOpts = BuildOpts {
                     bo_verbosity  :: Verbosity,
                     bo_runTime    :: Maybe NominalDiffTime,
                     bo_stateDir   :: FilePath,
                     bo_continuous :: Maybe Int,
                     bo_keepGoing  :: Bool
                 }

data BuildConfig = BuildConfig {
                       bc_srcURI   :: URI,
                       bc_auxURIs  :: [URI],
                       bc_username :: String,
                       bc_password :: String
                   }

srcName :: URI -> String
srcName uri = fromMaybe (show uri) (uriHostName uri)

installDirectory :: BuildOpts -> FilePath
installDirectory bo = bo_stateDir bo </> "inst"

main :: IO ()
main = topHandler $ do
    rethrowSignalsAsExceptions [SIGABRT, SIGINT, SIGQUIT, SIGTERM]
    hSetBuffering stdout LineBuffering
    args <- getArgs
    (mode, opts) <- validateOpts args

    case mode of
        Help strs ->
            do let usageHeader = intercalate "\n" [
                       "Usage: hackage-build init URL [auxiliary URLs] [options]",
                       "       hackage-build build [packages] [options]",
                       "       hackage-build stats",
                       "Options:"]
               mapM_ putStrLn $ strs
               putStrLn $ usageInfo usageHeader buildFlagDescrs
               unless (null strs) exitFailure
        Init uri auxUris -> initialise opts uri auxUris
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

initialise :: BuildOpts -> String -> [String] -> IO ()
initialise opts uriStr auxUrisStrs
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
         writeFile (configFile opts) (show (uriStr, auxUrisStrs, username, password))

readConfig :: BuildOpts -> IO BuildConfig
readConfig opts = do xs <- readFile $ configFile opts
                     case reads xs of
                         [((uriStr, auxUriStrs, username, password), "")] ->
                             case mapM validateHackageURI (uriStr : auxUriStrs) of
                             -- Shouldn't happen: We check that this
                             -- returns Right when we create the
                             -- config file. See [Note: Show/Read URI].
                               Left theError -> die theError
                               Right (uri : auxUris) ->
                                   return $ BuildConfig {
                                                bc_srcURI   = uri,
                                                bc_auxURIs  = auxUris,
                                                bc_username = username,
                                                bc_password = password
                                            }
                               Right _ -> error "The impossible happened"
                         _ ->
                             die "Can't parse config file (maybe re-run \"hackage-build init\")"

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

infoStats :: MonadIO m => Verbosity -> Maybe FilePath -> [DocInfo] -> m ()
infoStats verbosity mDetailedStats pkgIdsHaveDocs = liftIO $ do
    nfo $ "There are "
       ++ show (length byPackage)
       ++ " packages with a total of "
       ++ show (length pkgIdsHaveDocs)
       ++ " package versions"
    nfo $ "So far we have built or attempted to built "
       ++ show (length (filter ((/= DocsNotBuilt) . docInfoHasDocs) pkgIdsHaveDocs))
       ++ " packages; only "
       ++ show (length (filter ((== DocsNotBuilt) . docInfoHasDocs) pkgIdsHaveDocs))
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

    byPackage :: [[DocInfo]]
    byPackage = map (sortBy (flip (comparing docInfoPackageVersion)))
              $ groupBy (equating  docInfoPackageName)
              $ sortBy  (comparing docInfoPackageName) pkgIdsHaveDocs

    mostRecentBuilt, mostRecentFailed, mostRecentNotBuilt :: [[DocInfo]]
    mostRecentBuilt    = filter ((== HasDocs)      . docInfoHasDocs . head) byPackage
    mostRecentFailed   = filter ((== DocsFailed)   . docInfoHasDocs . head) byPackage
    mostRecentNotBuilt = filter ((== DocsNotBuilt) . docInfoHasDocs . head) byPackage

    categorise :: [DocInfo] -> StatResult
    categorise ps
      | all (== HasDocs)      hd = AllVersionsBuiltOk
      | all (/= DocsNotBuilt) hd = AllVersionsAttempted
      | all (== DocsNotBuilt) hd = NoneBuilt
      | all (/= DocsFailed)   hd = SomeBuiltOk
      | otherwise                = SomeFailed
      where
        hd = map docInfoHasDocs ps

    categorised :: [StatResult]
    categorised = map categorise byPackage

    count :: StatResult -> String
    count c = show (length (filter (c ==) categorised))

    formatPkg :: [DocInfo] -> [[String]]
    formatPkg = map $ \docInfo -> [
                          display (docInfoPackageName docInfo)
                        , display (docInfoPackageVersion docInfo)
                        , show (docInfoHasDocs docInfo)
                        ]

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

data DocInfo = DocInfo {
    docInfoPackage     :: PackageIdentifier
  , docInfoHasDocs     :: HasDocs
  , docInfoIsCandidate :: Bool
  }

docInfoPackageName :: DocInfo -> PackageName
docInfoPackageName = pkgName . docInfoPackage

docInfoPackageVersion :: DocInfo -> Version
docInfoPackageVersion = pkgVersion . docInfoPackage

docInfoBaseURI :: BuildConfig -> DocInfo -> URI
docInfoBaseURI config docInfo =
  if not (docInfoIsCandidate docInfo)
    then bc_srcURI config <//> "package" </> display (docInfoPackage docInfo)
    else bc_srcURI config <//> "package" </> display (docInfoPackage docInfo) </> "candidate"

docInfoDocsURI :: BuildConfig -> DocInfo -> URI
docInfoDocsURI config docInfo = docInfoBaseURI config docInfo <//> "docs"

docInfoTarGzURI :: BuildConfig -> DocInfo -> URI
docInfoTarGzURI config docInfo = docInfoBaseURI config docInfo <//> display (docInfoPackage docInfo) <.> "tar.gz"

getDocumentationStats :: BuildConfig
                      -> (PackageId -> IO Bool)
                      -> HttpSession [DocInfo]
getDocumentationStats config didFail = do
    mPackages   <- liftM eitherDecode `liftM` requestGET' packagesUri
    mCandidates <- liftM eitherDecode `liftM` requestGET' candidatesUri

    case (mPackages, mCandidates) of
      -- Download failure
      (Nothing, _) -> fail $ "Could not download " ++ show packagesUri
      (_, Nothing) -> fail $ "Could not download " ++ show candidatesUri
      -- Decoding failure
      (Just (Left e), _) -> fail $ "Could not decode " ++ show packagesUri   ++ ": " ++ e
      (_, Just (Left e)) -> fail $ "Could not decode " ++ show candidatesUri ++ ": " ++ e
      -- Success
      (Just (Right packages), Just (Right candidates)) -> do
        packages'   <- liftIO $ mapM checkFailed packages
        candidates' <- liftIO $ mapM checkFailed candidates
        return $ map (setIsCandidate False) packages'
              ++ map (setIsCandidate True)  candidates'
  where
    packagesUri   = bc_srcURI config <//> "packages" </> "docs.json"
    candidatesUri = bc_srcURI config <//> "packages" </> "candidates" </> "docs.json"

    checkFailed :: (String, Bool) -> IO (PackageIdentifier, HasDocs)
    checkFailed (pkgId, docsBuilt) = do
      let pkgId' = fromJust (simpleParse pkgId)
      if docsBuilt
        then return (pkgId', HasDocs)
        else do failed <- didFail pkgId'
                if failed then return (pkgId', DocsFailed)
                          else return (pkgId', DocsNotBuilt)

    setIsCandidate :: Bool -> (PackageIdentifier, HasDocs) -> DocInfo
    setIsCandidate isCandidate (pId, hasDocs) = DocInfo {
        docInfoPackage     = pId
      , docInfoHasDocs     = hasDocs
      , docInfoIsCandidate = isCandidate
      }

buildOnce :: BuildOpts -> [PackageId] -> IO ()
buildOnce opts pkgs = keepGoing $ do
    config <- readConfig opts
    let cacheDir = bo_stateDir opts </> cacheDirName
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
        let latestFirst :: [[DocInfo]] -> [DocInfo]
            latestFirst ids = map head ids ++ concatMap tail ids

        -- Find those files *not* marked as having documentation in our cache
        let toBuild :: [DocInfo]
            toBuild = filter shouldBuild
                    . latestFirst
                    . map (sortBy (flip (comparing docInfoPackageVersion)))
                    . groupBy (equating  docInfoPackageName)
                    . sortBy  (comparing docInfoPackageName)
                    $ pkgIdsHaveDocs

        liftIO $ notice verbosity $ show (length toBuild) ++ " package(s) to build"

        -- Try to build each of them, uploading the documentation and
        -- build reports along the way. We mark each package as having
        -- documentation in the cache even if the build fails because
        -- we don't want to keep continually trying to build a failing
        -- package!
        startTime <- liftIO $ getCurrentTime

        let go :: [DocInfo] -> IO ()
            go [] = return ()
            go (docInfo : toBuild') = do
              mTgz <- buildPackage verbosity opts config docInfo
              case mTgz of
                Nothing ->
                  liftIO $ mark_as_failed (docInfoPackage docInfo)
                Just docs_tgz -> httpSession verbosity $ do
                  -- Make sure we authenticate to Hackage
                  setAuthorityGen $ provideAuthInfo (bc_srcURI config)
                                  $ Just (bc_username config, bc_password config)
                  requestPUT (docInfoDocsURI config docInfo) "application/x-tar" (Just "gzip") docs_tgz

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
    shouldBuild :: DocInfo -> Bool
    shouldBuild docInfo = case docInfoHasDocs docInfo of
      DocsNotBuilt -> docInfoPackage docInfo `elem` pkgs || null pkgs
      _            -> docInfoPackage docInfo `elem` pkgs

    keepGoing :: IO () -> IO ()
    keepGoing act
      | bo_keepGoing opts = Control.Exception.catch act showExceptionAsWarning
      | otherwise         = act

    showExceptionAsWarning :: SomeException -> IO ()
    showExceptionAsWarning e = do
      warn verbosity (show e)
      notice verbosity "Abandoning this build attempt."

    verbosity = bo_verbosity opts

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
    writeFile "cabal-config" . unlines $
      remoteRepos ++ [
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
 where
  remoteRepo :: URI -> String
  remoteRepo uri = "remote-repo: " ++ srcName uri ++ ":" ++ show uri

  remoteRepos :: [String]
  remoteRepos = map remoteRepo (bc_srcURI config : bc_auxURIs config)

-- | Build documentation and return @(Just tgz)@ for the built tgz file
-- on success, or @Nothing@ otherwise.
buildPackage :: MonadIO m
             => Verbosity -> BuildOpts -> BuildConfig
             -> DocInfo
             -> m (Maybe BS.ByteString)
buildPackage verbosity opts config docInfo = do
    liftIO $ do notice verbosity ("Building " ++ display (docInfoPackage docInfo))
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
        doc_dir      = doc_root </> display (docInfoPackage docInfo)
        doc_dir_html = doc_dir </> "html"
        temp_doc_dir = doc_root </> display (docInfoPackage docInfo) </> display (docInfoPackage docInfo) ++ "-docs"
        pkg_url      = "/package" </> "$pkg-$version"

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
                      "--haddock-html-location=" ++ pkg_url </> "docs",
                      -- Give the user a choice between themes:
                      "--haddock-option=--built-in-themes",
                      -- Link "Contents" to the package page:
                      "--haddock-contents-location=" ++ pkg_url,
                      -- Link to colourised source code:
                      "--haddock-hyperlink-source",
                      -- The docdir can differ between Cabal versions
                      "--docdir=" ++ doc_dir,
                      "--prefix=" ++ installDirectory opts,
                      -- For candidates we need to use the full URL, because
                      -- otherwise cabal-install will not find the package.
                      -- For regular packages however we need to use just the
                      -- package name, otherwise cabal-install will not
                      -- generate a report
                      if docInfoIsCandidate docInfo
                        then show (docInfoTarGzURI config docInfo)
                        else display (docInfoPackage docInfo)
                      ]

        -- Remove reports for dependencies (so that we only submit the report
        -- for the package we are currently building)
        dotCabal <- getAppUserDataDirectory "cabal"
        let reportsDir = dotCabal </> "reports" </> srcName (bc_srcURI config)
        handleDoesNotExist (return ()) $ withRecursiveContents_ reportsDir $ \path ->
           unless ((display (docInfoPackage docInfo) ++ ".log") `isSuffixOf` path) $
             removeFile path

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
            handleDoesNotExist (return ()) $ removeDirectoryRecursive reportsDir

            -- Other data goes into a local file storing build reports:
            let simple_report_log = installDirectory opts
                                </> "packages"
                                </> srcName (bc_srcURI config)
                                </> "build-reports.log"
            handleDoesNotExist (return ()) $ removeFile simple_report_log

        docs_generated <- fmap and $ sequence [
            doesDirectoryExist doc_dir_html,
            doesFileExist (doc_dir_html </> "doc-index.html"),
            doesFileExist (doc_dir_html </> display (docInfoPackageName docInfo) <.> "haddock")]
        if docs_generated
            then do
                notice verbosity $ "Docs generated for " ++ display (docInfoPackage docInfo)
                liftM Just $
                    -- We need the files in the documentation .tar.gz
                    -- to have paths like foo-x.y.z-docs/index.html
                    -- Unfortunately, on disk they have paths like
                    -- foo-x.y.z/html/index.html. This hack resolves
                    -- the problem:
                    bracket_ (renameDirectory doc_dir_html temp_doc_dir)
                             (renameDirectory temp_doc_dir doc_dir_html)
                             (tarGzDirectory temp_doc_dir)
            else do
              notice verbosity $ "Docs for " ++ display (docInfoPackage docInfo) ++ " failed to build"
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
    flagKeepGoing  :: Bool,
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
  , flagKeepGoing  = False
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

  , Option [] ["continuous"]
      (NoArg (\opts -> opts { flagContinuous = True }))
      "Build continuously rather than just once"

  , Option [] ["keep-going"]
      (NoArg (\opts -> opts { flagKeepGoing = True }))
      "Keep going after errors"

  , Option [] ["interval"]
      (ReqArg (\int opts -> opts { flagInterval = Just int }) "MIN")
      "Set the building interval in minutes (default 30)"
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
                   bo_continuous = case (flagContinuous flags, flagInterval flags) of
                                     (True, Just i)  -> Just (read i)
                                     (True, Nothing) -> Just 30 -- default interval
                                     (False, _)      -> Nothing,
                   bo_keepGoing  = flagKeepGoing flags
               }

        mode = case args' of
               _ | flagHelp flags -> Help []
                 | not (null errs) -> Help errs
               "init" : uri : auxUris ->
                   -- We don't actually want the URI at this point
                   -- (see [Note: Show/Read URI])
                   case mapM validateHackageURI (uri : auxUris) of
                     Left  theError -> Help [theError]
                     Right _        -> Init uri auxUris
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

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

handleDoesNotExist :: IO a -> IO a -> IO a
handleDoesNotExist handler act
    = handleJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                 (\() -> handler)
                 act

withRecursiveContents :: FilePath -> (FilePath -> IO a) -> IO [a]
withRecursiveContents topdir act = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  ass <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then withRecursiveContents path act
      else return `liftM` act path
  return (concat ass)

withRecursiveContents_ :: FilePath -> (FilePath -> IO ()) -> IO ()
withRecursiveContents_ fp = void . withRecursiveContents fp
