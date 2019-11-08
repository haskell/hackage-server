{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Network.HTTP hiding (password)
import Network.Browser
import Network.URI (URI(..), parseRelativeReference, relativeTo)

import Distribution.Client
import Distribution.Client.Cron (cron, rethrowSignalsAsExceptions,
                                 Signal(..), ReceivedSignal(..))
import qualified Distribution.Client.Index as Index

import Distribution.Package
import Distribution.Text
import qualified Text.PrettyPrint          as Disp
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (intercalate)
import Distribution.Version (Version, nullVersion)

import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import Data.IORef
import Data.Time
import Control.Applicative as App
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M

import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import System.Environment
import System.Exit(exitFailure, ExitCode(..))
import System.FilePath
import System.Directory (canonicalizePath, createDirectoryIfMissing,
                         doesFileExist, doesDirectoryExist, getDirectoryContents,
                         renameFile, removeFile, getAppUserDataDirectory,
                         createDirectory, removeDirectoryRecursive,
                         createDirectoryIfMissing)
import System.Console.GetOpt
import System.Process
import System.IO
import Control.Concurrent

import Paths_hackage_server (version)


import Data.Aeson (eitherDecode)

data Mode = Help [String]
          | Init URI [URI]
          | Stats
          | Build [PackageId]

data BuildOpts = BuildOpts {
                     bo_verbosity  :: Verbosity,
                     bo_runTime    :: Maybe NominalDiffTime,
                     bo_stateDir   :: FilePath,
                     bo_continuous :: Maybe Int,
                     bo_keepGoing  :: Bool,
                     bo_dryRun     :: Bool,
                     bo_prune      :: Bool,
                     bo_username   :: Maybe String,
                     bo_password   :: Maybe String,
                     bo_buildAttempts :: Int,
                     -- ^ how many times to attempt to rebuild a failing package
                     bo_buildOrder :: BuildOrder
                 }

data BuildConfig = BuildConfig {
                       bc_srcURI   :: URI,
                       bc_auxURIs  :: [URI],
                       bc_username :: String,
                       bc_password :: String
                   }

data BuildOrder = LatestVersionFirst
                  -- ^ First build all of the latest versions of each package
                  -- Then go back and build all the older versions
                | MostRecentlyUploadedFirst
                  -- ^ Build in order of upload date.

srcName :: URI -> String
srcName uri = fromMaybe (show uri) (uriHostName uri)

installDirectory :: BuildOpts -> FilePath
installDirectory bo = bo_stateDir bo </> "tmp-install"

resultsDirectory :: BuildOpts -> FilePath
resultsDirectory bo = bo_stateDir bo </> "results"

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


---------------------------------
-- Initialisation & config file
--

initialise :: BuildOpts -> URI -> [URI] -> IO ()
initialise opts uri auxUris
    = do username <- readMissingOpt "Enter hackage username" (bo_username opts)
         password <- readMissingOpt "Enter hackage password" (bo_password opts)
         let config = BuildConfig {
                        bc_srcURI   = uri,
                        bc_auxURIs  = auxUris,
                        bc_username = username,
                        bc_password = password
                      }
         createDirectoryIfMissing False $ bo_stateDir opts
         createDirectoryIfMissing False $ resultsDirectory opts
         writeConfig opts config
         writeCabalConfig opts config
  where
    readMissingOpt prompt = maybe (putStrLn prompt >> getLine) return

-- | Parse the @00-index.cache@ file of the available package repositories.
parseRepositoryIndices :: Verbosity -> IO (M.Map PackageIdentifier Tar.EpochTime)
parseRepositoryIndices verbosity = do
    cabalDir <- getAppUserDataDirectory "cabal/packages"
    cacheDirs <- listDirectory cabalDir
    indexFiles <- filterM doesFileExist $ map (\dir -> cabalDir </> dir </> "01-index.tar") cacheDirs
    M.unions <$> mapM readIndex indexFiles
  where
    readIndex fname = do
        bs <- BS.readFile fname
        let mkPkg pkg entry = (pkg, Tar.entryTime entry)
        case Index.read mkPkg (".cabal" `isSuffixOf`) bs of
          Left msg -> do warn verbosity $ "failed to read package index "++show fname++": "++msg
                         return M.empty
          Right pkgs -> return $ M.fromList pkgs

    -- stolen from directory-1.2.5
    listDirectory :: FilePath -> IO [FilePath]
    listDirectory path =
      (filter f) <$> (getDirectoryContents path)
      where f filename = filename /= "." && filename /= ".."


writeConfig :: BuildOpts -> BuildConfig -> IO ()
writeConfig opts BuildConfig {
                   bc_srcURI   = uri,
                   bc_auxURIs  = auxUris,
                   bc_username = username,
                   bc_password = password
                 } =
    -- [Note: Show/Read URI]
    -- Ideally we'd just be showing a BuildConfig, but URI doesn't
    -- have Show/Read, so that doesn't work. So instead, we write
    -- out a tuple containing the uri as a string, and parse it
    -- each time we read it.
    let confStr = show (show uri, map show auxUris, username, password) in
    writeFile (configFile opts) confStr

readConfig :: BuildOpts -> IO BuildConfig
readConfig opts = do
    xs <- readFile $ configFile opts
    case reads xs of
      [((uriStr, auxUriStrs, username, password), _)] ->
         case mapM validateHackageURI (uriStr : auxUriStrs) of
         -- Shouldn't happen: We check that this
         -- returns Right when we create the
         -- config file. See [Note: Show/Read URI].
           Left theError -> dieNoVerbosity theError
           Right (uri : auxUris) ->
               return $ BuildConfig {
                            bc_srcURI   = uri,
                            bc_auxURIs  = auxUris,
                            bc_username = username,
                            bc_password = password
                        }
           Right _ -> error "The impossible happened"
      _ ->
         dieNoVerbosity "Can't parse config file (maybe re-run \"hackage-build init\")"

configFile :: BuildOpts -> FilePath
configFile opts = bo_stateDir opts </> "hackage-build-config"

writeCabalConfig :: BuildOpts -> BuildConfig -> IO ()
writeCabalConfig opts config = do
    let tarballsDir  = bo_stateDir opts </> "cached-tarballs"
    writeFile (bo_stateDir opts </> "cabal-config") . unlines $
        [ "remote-repo: " ++ srcName uri ++ ":" ++ show uri
        | uri <- bc_srcURI config : bc_auxURIs config ]
     ++ [ "remote-repo-cache: " ++ tarballsDir ]
    createDirectoryIfMissing False tarballsDir


----------------------
-- Displaying status
--

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

    notice verbosity "Initialising"

    (didFail, _, _)  <- mkPackageFailed opts

    pkgIdsHaveDocs <- getDocumentationStats verbosity config didFail
    infoStats verbosity (Just statsFile) pkgIdsHaveDocs
  where
    statsFile = bo_stateDir opts </> "stats"

infoStats :: Verbosity -> Maybe FilePath -> [DocInfo] -> IO ()
infoStats verbosity mDetailedStats pkgIdsHaveDocs = do
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
      Nothing        -> App.pure ()
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

docInfoReports :: BuildConfig -> DocInfo -> URI
docInfoReports config docInfo = docInfoBaseURI config docInfo <//> "reports/"

getDocumentationStats :: Verbosity
                      -> BuildConfig
                      -> (PackageId -> IO Bool)
                      -> IO [DocInfo]
getDocumentationStats verbosity config didFail = do
    notice verbosity "Downloading documentation index"
    httpSession verbosity "hackage-build" version $ do
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


----------------------
-- Building packages
--

buildOnce :: BuildOpts -> [PackageId] -> IO ()
buildOnce opts pkgs = keepGoing $ do
    config <- readConfig opts

    notice verbosity "Initialising"
    (has_failed, mark_as_failed, persist_failed) <- mkPackageFailed opts

    flip finally persist_failed $ do
        updatePackageIndex
        -- Due to caching sometimes the package repository state may lag behind the
        -- documentation index. Consequently, we make sure that the packages we are
        -- going to build actually appear in the repository before building. See
        -- #543.
        repoIndex <- parseRepositoryIndices verbosity

        pkgIdsHaveDocs <- getDocumentationStats verbosity config has_failed
        infoStats verbosity Nothing pkgIdsHaveDocs
        threadDelay (10^(7::Int))

        let orderBuilds :: BuildOrder -> [DocInfo] -> [DocInfo]
            orderBuilds LatestVersionFirst =
                  latestFirst
                . map (sortBy (flip (comparing docInfoPackageVersion)))
                . groupBy (equating  docInfoPackageName)
                . sortBy  (comparing docInfoPackageName)
              where
                -- NOTE: assumes all these lists are non-empty
                latestFirst :: [[DocInfo]] -> [DocInfo]
                latestFirst ids = map head ids ++ concatMap tail ids

            orderBuilds MostRecentlyUploadedFirst =
                  map snd
                . sortBy (comparing fst)
                . mapMaybe (\pkg -> fmap (\uploadTime -> (Down uploadTime, pkg)) (M.lookup (docInfoPackage pkg) repoIndex))


        -- Find those files *not* marked as having documentation in our cache
        let toBuild :: [DocInfo]
            toBuild = filter shouldBuild
                    . filter (flip M.member repoIndex . docInfoPackage)
                    . orderBuilds (bo_buildOrder opts)
                    $ pkgIdsHaveDocs

        notice verbosity $ show (length toBuild) ++ " package(s) to build"

        -- Try to build each of them, uploading the documentation and
        -- build reports along the way. We mark each package as having
        -- documentation in the cache even if the build fails because
        -- we don't want to keep continually trying to build a failing
        -- package!
        startTime <- getCurrentTime

        let go :: [DocInfo] -> IO ()
            go [] = return ()
            go (docInfo : toBuild') = do
              (mTgz, mRpt, logfile) <- buildPackage verbosity opts config docInfo
              let installOk = fmap ("install-outcome: InstallOk" `isInfixOf`) mRpt == Just True
              case mTgz of
                Nothing -> do
                     mark_as_failed (docInfoPackage docInfo)
                     -- When it installed ok, but there's no docs, that means it is exe only.
                     -- This marks it "really failed" in such a case to stop retries.
                     when installOk . replicateM_ 4 $ mark_as_failed (docInfoPackage docInfo)
                Just _  -> return ()
              case mRpt of
                Just _  | bo_dryRun opts -> return ()
                Just report -> uploadResults verbosity config docInfo
                                              mTgz report logfile
                _           -> return ()

              -- We don't check the runtime until we've actually tried
              -- to build a doc, so as to ensure we make progress.
              outOfTime <- case bo_runTime opts of
                  Nothing -> return False
                  Just d  -> do
                    currentTime <- getCurrentTime
                    return $ (currentTime `diffUTCTime` startTime) > d

              if outOfTime then return ()
                           else go toBuild'

        go toBuild
  where
    shouldBuild :: DocInfo -> Bool
    shouldBuild docInfo =
        case docInfoHasDocs docInfo of
          DocsNotBuilt -> null pkgs || is_selected
          _            -> is_selected -- rebuild package if the user explicitly requested it
      where
        is_selected = any (isSelectedPackage pkgid) pkgs
        pkgid = docInfoPackage docInfo

    -- do versionless matching if no version was given
    isSelectedPackage pkgid pkgid'@(PackageIdentifier _ v)
        | nullVersion == v =
        packageName pkgid == packageName pkgid'
    isSelectedPackage pkgid pkgid' =
        pkgid == pkgid'

    keepGoing :: IO () -> IO ()
    keepGoing act
      | bo_keepGoing opts = Control.Exception.catch act showExceptionAsWarning
      | otherwise         = act

    showExceptionAsWarning :: SomeException -> IO ()
    showExceptionAsWarning e
      -- except for signals telling us to really stop
      | Just (ReceivedSignal {}) <- fromException e
      = throwIO e

      | Just UserInterrupt <- fromException e
      = throwIO e

      | otherwise
      = do warn verbosity (show e)
           notice verbosity "Abandoning this build attempt."

    verbosity = bo_verbosity opts

    updatePackageIndex = do
      update_ec <- cabal opts "update" [] Nothing
      unless (update_ec == ExitSuccess) $
          dieNoVerbosity "Could not 'cabal update' from specified server"

-- | Builds a little memoised function that can tell us whether a
-- particular package failed to build its documentation, a function to mark a
-- package as having failed, and a function to write the final failed list back
-- to disk.
mkPackageFailed :: BuildOpts
                -> IO (PackageId -> IO Bool, PackageId -> IO (), IO ())
mkPackageFailed opts = do
    init_failed <- readFailedCache (bo_stateDir opts)
    cache_var   <- newIORef init_failed

    let mark_as_failed pkg_id = atomicModifyIORef cache_var $ \already_failed ->
                                  (M.insertWith (+) pkg_id 1 already_failed, ())
        has_failed     pkg_id = f <$> readIORef cache_var
          where f cache = M.findWithDefault 0 pkg_id cache > bo_buildAttempts opts
        persist               = readIORef cache_var >>= writeFailedCache (bo_stateDir opts)

    return (has_failed, mark_as_failed, persist)
  where
    readFailedCache :: FilePath -> IO (M.Map PackageId Int)
    readFailedCache cache_dir = do
        pkgstrs <- handleDoesNotExist [] $ liftM lines $ readFile (cache_dir </> "failed")
        let (pkgids, attempts) = unzip $ map (parseLine . words) pkgstrs
               where
                 parseLine [pkg_id] = (pkg_id, 1)
                 parseLine [pkg_id, attempts']
                   | [(n,_)] <- reads attempts' = (pkg_id, n)
                   | otherwise                  = (pkg_id, 1)
                 parseLine other = error $ "failed to parse failed list line: "++show other
        case validatePackageIds pkgids of
            Left theError -> dieNoVerbosity theError
            Right pkgs -> return (M.fromList $ zip pkgs attempts)

    writeFailedCache :: FilePath -> M.Map PackageId Int -> IO ()
    writeFailedCache cache_dir pkgs =
      writeUTF8File (cache_dir </> "failed")
      $ unlines
      $ map (\(pkgid,n) -> show $ (Disp.text $ show pkgid) Disp.<+> Disp.int n)
      $ M.assocs pkgs


-- | Build documentation and return @(Just tgz)@ for the built tgz file
-- on success, or @Nothing@ otherwise.
buildPackage :: Verbosity -> BuildOpts -> BuildConfig
             -> DocInfo
             -> IO (Maybe FilePath, Maybe FilePath, FilePath)
buildPackage verbosity opts config docInfo = do
    let pkgid = docInfoPackage docInfo
    notice verbosity ("Building " ++ display pkgid)
    handleDoesNotExist () $
        removeDirectoryRecursive $ installDirectory opts
    createDirectory $ installDirectory opts

    -- Create the local package db
    let packageDb = installDirectory opts </> "packages.db"
    -- TODO: use Distribution.Simple.Program.HcPkg
    ph <- runProcess "ghc-pkg"
                     ["init", packageDb]
                     Nothing Nothing Nothing Nothing Nothing
    init_ec <- waitForProcess ph
    unless (init_ec == ExitSuccess) $
        dieNoVerbosity $ "Could not initialise the package db " ++ packageDb

    -- The documentation is installed within the stateDir because we
    -- set a prefix while installing
    let doc_root     = installDirectory opts </> "haddocks"
        doc_dir_tmpl = doc_root </> "$pkgid-docs"
        doc_dir_pkg  = doc_root </> display pkgid ++ "-docs"
--        doc_dir_html = doc_dir </> "html"
--        deps_doc_dir = doc_dir </> "deps"
--        temp_doc_dir = doc_dir </> display (docInfoPackage docInfo) ++ "-docs"
        pkg_url      = "/package" </> "$pkg-$version"
        pkg_flags    =
            ["--enable-documentation",
             "--htmldir=" ++ doc_dir_tmpl,
             -- We only care about docs, so we want to build as
             -- quickly as possible, and hence turn
             -- optimisation off. Also explicitly pass -O0 as a
             -- GHC option, in case it overrides a .cabal
             -- setting or anything
             "--disable-optimization", "--ghc-option", "-O0",
             "--disable-library-for-ghci",
             -- We don't want packages installed in the user
             -- package.conf to affect things. In particular,
             -- we don't want doc building to fail because
             -- "packages are likely to be broken by the reinstalls"
             "--package-db=clear", "--package-db=global",
             "--package-db=" ++ packageDb,
             -- Always build the package, even when it's been built
             -- before. This lets us regenerate documentation when
             -- dependencies are updated.
             "--reinstall", "--force-reinstalls",
             -- We know where this documentation will
             -- eventually be hosted, bake that in.
             -- The wiki claims we shouldn't include the
             -- version in the hyperlinks so we don't have
             -- to rehaddock some package when the dependent
             -- packages get updated. However, this is NOT
             -- what the Hackage v1 did, so ignore that:
             "--haddock-html-location=" ++ pkg_url </> "docs",
             -- Link "Contents" to the package page:
             "--haddock-contents-location=" ++ pkg_url,
             -- Link to colourised source code:
             "--haddock-hyperlink-source",
             "--prefix=" ++ installDirectory opts,
             "--build-summary=" ++ installDirectory opts </> "reports" </> "$pkgid.report",
             "--report-planning-failure",
             -- We want both html documentation and hoogle database generated
             "--haddock-html",
             "--haddock-hoogle",
             -- Generate the quickjump index files
             "--haddock-option=--quickjump",
             -- For candidates we need to use the full URL, because
             -- otherwise cabal-install will not find the package.
             -- For regular packages however we need to use just the
             -- package name, otherwise cabal-install will not
             -- generate a report
             if docInfoIsCandidate docInfo
               then show (docInfoTarGzURI config docInfo)
               else display pkgid
             ]

    -- The installDirectory is purely temporary, while the resultsDirectory is
    -- more persistent. We will grab various outputs from the tmp dir and stash
    -- them for safe keeping (for later upload or manual inspection) in the
    -- results dir.
    let resultDir         = resultsDirectory opts
        resultLogFile     = resultDir </> display pkgid <.> "log"
        resultReportFile  = resultDir </> display pkgid <.> "report"
        resultDocsTarball = resultDir </> (display pkgid ++ "-docs") <.> "tar.gz"

    buildLogHnd <- openFile resultLogFile WriteMode

    -- We ignore the result of calling @cabal install@ because
    -- @cabal install@ succeeds even if the documentation fails to build.
    void $ cabal opts "install" pkg_flags (Just buildLogHnd)

    -- Grab the report for the package we want. Stash it for safe keeping.
    report <- handleDoesNotExist Nothing $ do
                renameFile (installDirectory opts </> "reports"
                                </> display pkgid <.> "report")
                           resultReportFile
                appendFile resultReportFile "\ndoc-builder: True"
                return (Just resultReportFile)

    docs_generated <- fmap and $ sequence [
        doesDirectoryExist doc_dir_pkg,
        doesFileExist (doc_dir_pkg </> "doc-index.html"),
        doesFileExist (doc_dir_pkg </> display (docInfoPackageName docInfo) <.> "haddock")]
    docs <- if docs_generated
              then do
                when (bo_prune opts) (pruneHaddockFiles doc_dir_pkg)
                try (tarGzDirectory doc_dir_pkg) >>= either
                  (\(e :: SomeException) -> print e >> return Nothing)
                  (\x -> BS.writeFile resultDocsTarball x >> return (Just resultDocsTarball))
              else return Nothing

    notice verbosity $ unlines
      [ "Build results for " ++ display pkgid ++ ":"
      , fromMaybe "no report" report
      , fromMaybe "no docs" docs
      , resultLogFile
      ]

    return (docs, report, resultLogFile)


cabal :: BuildOpts -> String -> [String] -> Maybe Handle -> IO ExitCode
cabal opts cmd args moutput = do
    let verbosity = bo_verbosity opts
        cabalConfigFile = bo_stateDir opts </> "cabal-config"
        verbosityArgs = if verbosity == silent
                        then ["-v0"]
                        else []
        all_args = ("--config-file=" ++ cabalConfigFile)
                 : cmd
                 : verbosityArgs
                ++ args
    info verbosity $ unwords ("cabal":all_args)
    ph <- runProcess "cabal" all_args Nothing
                     Nothing Nothing moutput moutput
    waitForProcess ph

pruneHaddockFiles :: FilePath -> IO ()
pruneHaddockFiles dir = do
    -- Hackage doesn't support the haddock frames view, so remove it
    -- both visually (no frames link) and save space too.
    files <- getDirectoryContents dir
    sequence_
      [ removeFile (dir </> file)
      | file <- files
      , unwantedFile file ]
    hackJsUtils
  where
    unwantedFile file
      | "frames.html" == file             = True
      | "mini_" `isPrefixOf` file         = True
        -- The .haddock file is haddock-version specific
        -- so it is not useful to make available for download
      | ".haddock" <- takeExtension file  = True
      | otherwise                         = False

    -- The "Frames" link is added by the JS, just comment it out.
    hackJsUtils = do
      content <- readFile (dir </> "haddock-util.js")
      _ <- evaluate (length content)
      writeFile (dir </> "haddock-util.js") (munge content)
      where
        munge = unlines
              . map removeAddMenuItem
              . lines
        removeAddMenuItem l | (sp, l') <- span (==' ') l
                            , "addMenuItem" `isPrefixOf` l'
                            = sp ++ "//" ++ l'
        removeAddMenuItem l = l


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

uploadResults :: Verbosity -> BuildConfig -> DocInfo
              -> Maybe FilePath -> FilePath -> FilePath -> IO ()
uploadResults verbosity config docInfo
              mdocsTarballFile buildReportFile buildLogFile =
    httpSession verbosity "hackage-build" version $ do
      -- Make sure we authenticate to Hackage
      setAuthorityGen (provideAuthInfo (bc_srcURI config)
                                       (Just (bc_username config, bc_password config)))
      case mdocsTarballFile of
        Nothing              -> return ()
        Just docsTarballFile ->
          putDocsTarball config docInfo docsTarballFile

      buildId <- postBuildReport config docInfo buildReportFile
      putBuildLog buildId buildLogFile

putDocsTarball :: BuildConfig -> DocInfo -> FilePath -> HttpSession ()
putDocsTarball config docInfo docsTarballFile =
    requestPUTFile (docInfoDocsURI config docInfo)
      "application/x-tar" (Just "gzip") docsTarballFile

type BuildReportId = URI

postBuildReport :: BuildConfig -> DocInfo -> FilePath -> HttpSession BuildReportId
postBuildReport config docInfo reportFile = do
    let uri = docInfoReports config docInfo
    body <- liftIO $ BS.readFile reportFile
    setAllowRedirects False
    (_, response) <- request Request {
      rqURI     = uri,
      rqMethod  = POST,
      rqHeaders = [Header HdrContentType   ("text/plain"),
                   Header HdrContentLength (show (BS.length body)),
                   Header HdrAccept        ("text/plain")],
      rqBody    = body
    }
    case rspCode response of
      --TODO: fix server to not do give 303, 201 is more appropriate
      (3,0,3) | [Just buildId] <- [ do rel <- parseRelativeReference location
                                       return $ relativeTo rel uri
                                  | Header HdrLocation location <- rspHeaders response ]
                -> return buildId
      _         -> do checkStatus uri response
                      fail "Unexpected response from server."

putBuildLog :: BuildReportId -> FilePath -> HttpSession ()
putBuildLog reportId buildLogFile = do
    body <- liftIO $ BS.readFile buildLogFile
    let uri = reportId <//> "log"
    setAllowRedirects False
    (_, response) <- request Request {
        rqURI     = uri,
        rqMethod  = PUT,
        rqHeaders = [Header HdrContentType   ("text/plain"),
                     Header HdrContentLength (show (BS.length body)),
                     Header HdrAccept        ("text/plain")],
        rqBody    = body
      }
    case rspCode response of
      --TODO: fix server to not to give 303, 201 is more appropriate
      (3,0,3)   -> return ()
      _         -> checkStatus uri response


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
    flagDryRun     :: Bool,
    flagInterval   :: Maybe String,
    flagPrune      :: Bool,
    flagUsername   :: Maybe String,
    flagPassword   :: Maybe String,
    flagBuildAttempts :: Maybe Int,
    flagBuildOrder :: Maybe BuildOrder
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
  , flagDryRun     = False
  , flagInterval   = Nothing
  , flagPrune      = False
  , flagUsername   = Nothing
  , flagPassword   = Nothing
  , flagBuildAttempts = Nothing
  , flagBuildOrder = Nothing
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

  , Option [] ["dry-run"]
      (NoArg (\opts -> opts { flagDryRun = True }))
      "Don't record results or upload"

  , Option [] ["interval"]
      (ReqArg (\int opts -> opts { flagInterval = Just int }) "MIN")
      "Set the building interval in minutes (default 30)"

  , Option [] ["prune-haddock-files"]
      (NoArg (\opts -> opts { flagPrune = True }))
      "Remove unnecessary haddock files (frames, .haddock file)"

  , Option [] ["init-username"]
      (ReqArg (\uname opts -> opts { flagUsername = Just uname }) "USERNAME")
      "The Hackage user to run the build as (used with init)"

  , Option [] ["init-password"]
      (ReqArg (\passwd opts -> opts { flagPassword = Just passwd }) "PASSWORD")
      "The password of the Hackage user to run the build as (used with init)"

  , Option [] ["build-attempts"]
      (ReqArg (\attempts opts -> case reads attempts of
                                 [(attempts', "")] -> opts { flagBuildAttempts = Just attempts' }
                                 _ -> error "Can't parse attempt count") "ATTEMPTS")
      "How many times to attempt to build a package before giving up"

  , Option [] ["build-order"]
     (ReqArg (\order opts -> let set o = opts { flagBuildOrder = Just o }
                             in case order of
                                "latest-version-first" -> set LatestVersionFirst
                                "recent-uploads-first" -> set MostRecentlyUploadedFirst
                                _                      -> error "Can't parse build order") "ORDER")
     "What order should packages be built in? (latest-version-first or recent-uploads-first)"
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
                   bo_keepGoing  = flagKeepGoing flags,
                   bo_dryRun     = flagDryRun flags,
                   bo_prune      = flagPrune flags,
                   bo_username   = flagUsername flags,
                   bo_password   = flagPassword flags,
                   bo_buildAttempts = fromMaybe 3 $ flagBuildAttempts flags,
                   bo_buildOrder = fromMaybe LatestVersionFirst $ flagBuildOrder flags
               }

        mode = case args' of
               _ | flagHelp flags -> Help []
                 | not (null errs) -> Help errs
               "init" : uriStr : auxUriStrs ->
                   -- We don't actually want the URI at this point
                   -- (see [Note: Show/Read URI])
                   case mapM validateHackageURI (uriStr : auxUriStrs) of
                     Left  theError      -> Help [theError]
                     Right (uri:auxUris) -> Init uri auxUris
                     Right _             -> error "impossible"
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
