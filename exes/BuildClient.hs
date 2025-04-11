{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.URI (URI(..))
import Distribution.Client
import Distribution.Client.Cron (cron, rethrowSignalsAsExceptions,
                                 Signal(..), ReceivedSignal(..))
import qualified Distribution.Client.Index as Index
import qualified Distribution.Server.Features.BuildReports.BuildReport as BR

import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (intercalate)
import Distribution.Version (Version)

import Data.Either
import Data.List
import Data.Maybe
import Data.Ord (Down(..))
import Data.Time
import Control.Applicative as App
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8      as BSS
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Map                   as M

import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import System.Environment
import System.Exit(exitFailure, ExitCode(..))
import System.FilePath
import System.Directory (canonicalizePath, createDirectoryIfMissing,
                         doesFileExist, doesDirectoryExist, getDirectoryContents,
                         renameFile, removeFile,
                         createDirectory, removeDirectoryRecursive,
                         createDirectoryIfMissing, makeAbsolute)
import System.Console.GetOpt
import System.Process
import System.IO
import Control.Concurrent

import Paths_hackage_server (version)


import Data.Aeson (eitherDecode, encode, parseJSON)
import Data.Aeson.Types (parseEither)

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
                     bo_buildOlderGHC :: Bool,
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
               mapM_ putStrLn strs
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
parseRepositoryIndices :: BuildOpts -> Verbosity -> IO (M.Map PackageIdentifier Tar.EpochTime)
parseRepositoryIndices opts verbosity = do
    cacheDirs <- listDirectory cabalDir
    indexFiles <- catMaybes <$> mapM findIdx cacheDirs
    M.unions <$> mapM readIndex indexFiles
  where
    cabalDir = bo_stateDir opts </> "cached-tarballs"
    findIdx dir = do
       let index01 = cabalDir </> dir </> "01-index.tar"
           index00 = cabalDir </> dir </> "00-index.tar"
       b <- doesFileExist index01
       if b
         then return (Just index01)
         else do
           b2 <- doesFileExist index00
           if b2
             then return (Just index00)
             else return Nothing
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
    createDirectoryIfMissing False tarballsDir

    -- Because we call runProcess with installDirectory as the cwd,
    -- this relative path won't be valid when cabal is running.
    -- An absolute path remains valid independently of cwd.
    absTarballsDir <- makeAbsolute tarballsDir

    writeFile (bo_stateDir opts </> "cabal-config") . unlines $
        [ "remote-repo: " ++ srcName uri ++ ":" ++ show uri
        | uri <- bc_srcURI config : bc_auxURIs config ]
     ++ [ "remote-repo-cache: " ++ absTarballsDir ]


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

    pkgIdsHaveDocs <- getDocumentationStats verbosity opts config Nothing
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
               . map unwords
               . map padCols
               $ xss
  where
    colWidths :: [[Int]]
    colWidths = map (map length) xss

    maxColWidths :: [Int]
    maxColWidths = map maximum (transpose colWidths)

    padCols :: [String] -> [String]
    padCols cols = zipWith padTo maxColWidths cols

    padTo :: Int -> String -> String
    padTo len str = str ++ replicate (len - length str) ' '

data HasDocs = HasDocs | DocsNotBuilt | DocsFailed
  deriving (Eq, Show)

data DocInfo = DocInfo {
    docInfoPackage     :: PackageIdentifier
  , docInfoHasDocs     :: HasDocs
  , docInfoIsCandidate :: Bool
  , docInfoRunTests    :: Bool
  }
  deriving Show

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

parseJsonStats :: BS.ByteString -> ([String],[BR.PkgDetails])
parseJsonStats = either (\x -> ([x],[])) (partitionEithers . map (parseEither parseJSON)) . eitherDecode

getDocumentationStats :: Verbosity
                      -> BuildOpts
                      -> BuildConfig
                      -> Maybe [PackageId]
                      -> IO [DocInfo]
getDocumentationStats verbosity opts config pkgs = do
    notice verbosity "Downloading documentation index"
    httpSession verbosity "hackage-build" version $ do
      curGhcVersion <- liftIO $ case bo_buildOlderGHC opts of
                                  True -> getGHCversion
                                  False -> return Nothing
      mPackages   <- fmap parseJsonStats <$> requestGET' (packagesUri False curGhcVersion)
      mCandidates <- fmap parseJsonStats <$> requestGET' (packagesUri True curGhcVersion)
      liftIO $ print curGhcVersion
      case (mPackages, mCandidates) of
        -- Download failure
        (Nothing, _) -> fail $ "Could not download " ++ show (packagesUri False curGhcVersion)
        (_, Nothing) -> fail $ "Could not download " ++ show (packagesUri True curGhcVersion)
        -- Success
        (Just (perrs, packages), Just (cerrs, candidates)) -> do
          liftIO . when (not . null $ perrs) . putStrLn $ "failed package json parses: " ++ show perrs
          liftIO . when (not . null $ cerrs) . putStrLn $ "failed candidate json parses: " ++ show cerrs
          let packages'   = map checkFailed packages
              candidates' = map checkFailed candidates
          return $ map (setIsCandidate False) packages'
                ++ map (setIsCandidate True)  candidates'
  where
    -- curGhcVersion = "f"
    getGHCversion :: IO (Maybe String)
    getGHCversion = do
      let dirloc = (bo_stateDir opts) </> "ghc" <.> "log"
      moutput <- openFile dirloc ReadWriteMode
      ph <- runProcess "ghc" ["--info"] Nothing
                    Nothing Nothing (Just moutput) Nothing
      waitForProcess ph
      hClose moutput
      handler <- openFile dirloc ReadWriteMode
      contents <- hGetContents handler
      let res      = read contents :: [(String, String)]
          version' = lookup "Project version" res
      return version'

    getQry :: [PackageIdentifier] -> String
    getQry [] = ""
    getQry [pkg] = display pkg
    getQry (x:y) = (display x) ++ "," ++ getQry y

    packagesUri :: Bool -> Maybe String -> URI
    packagesUri isCandidate curGhcVersion= do
      addEnd pkgs curGhcVersion $ addCandi isCandidate getURI
      where
        getURI = bc_srcURI config <//> "packages"
        addCandi True  uri  = uri <//> "candidates"
        addCandi False uri  = uri
        addEnd _            (Just a) uri = uri <//> "docs.json" ++ "?ghcid=" ++ a
        addEnd (Just [])    Nothing  uri = uri <//> "docs.json" ++ "?doc=false&fail=" ++ (show $ bo_buildAttempts opts)
        addEnd (Just pkgs') Nothing  uri = uri <//> "docs.json" ++ "?pkgs=" ++ (getQry pkgs')
        addEnd Nothing      Nothing  uri = uri <//> "docs.json"

    checkFailed :: BR.PkgDetails -> (PackageIdentifier, HasDocs, Bool)
    checkFailed pkgDetails =
      let pkgId = BR.pkid pkgDetails
          hasDocs = case (BR.docs pkgDetails, BR.failCnt pkgDetails) of
            (True , _)                        -> HasDocs
            (False, Just BR.BuildOK)          -> DocsFailed
            (False, Just (BR.BuildFailCnt a))
                | a >= bo_buildAttempts opts  -> DocsFailed
            (False, _)                        -> DocsNotBuilt
      in  (pkgId, hasDocs, fromMaybe True $ BR.runTests pkgDetails)

    setIsCandidate :: Bool -> (PackageIdentifier, HasDocs, Bool) -> DocInfo
    setIsCandidate isCandidate (pId, hasDocs, runTests) = DocInfo {
        docInfoPackage     = pId
      , docInfoHasDocs     = hasDocs
      , docInfoIsCandidate = isCandidate
      , docInfoRunTests    = runTests
      }


----------------------
-- Building packages
--

buildOnce :: BuildOpts -> [PackageId] -> IO ()
buildOnce opts pkgs = keepGoing $ do
    config <- readConfig opts
    notice verbosity "Initialising"

    handleDoesNotExist () $
        removeDirectoryRecursive $ installDirectory opts

    updatePackageIndex
    -- Due to caching sometimes the package repository state may lag behind the
    -- documentation index. Consequently, we make sure that the packages we are
    -- going to build actually appear in the repository before building. See
    -- #543.
    repoIndex <- parseRepositoryIndices opts verbosity

    pkgIdsHaveDocs <- getDocumentationStats verbosity opts config (Just pkgs)
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


    let toBuild :: [DocInfo]
        toBuild = filter (flip M.member repoIndex . docInfoPackage)
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
          processPkg verbosity opts config docInfo

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

-- Takes a single Package, process it and uploads result
processPkg :: Verbosity -> BuildOpts -> BuildConfig
             -> DocInfo -> IO ()
processPkg verbosity opts config docInfo = do
    prepareTempBuildDir
    (mTgz, mRpt, logfile)   <- buildPackage verbosity opts config docInfo
    buildReport             <- mapM readFile mRpt
    let installOk = fmap ("install-outcome: InstallOk" `isInfixOf`) buildReport == Just True

    -- Run Tests if installOk, Run coverage is Tests runs
    (testOutcome, hpcLoc, testfile)   <- case installOk && docInfoRunTests docInfo of
      True  -> testPackage verbosity opts docInfo
      False -> return (Nothing, Nothing, Nothing)
    coverageFile <- mapM (coveragePackage verbosity opts docInfo) hpcLoc

    -- Modify test-outcome and rewrite report file.
    mapM_ (setTestStatus mRpt buildReport) testOutcome

    case bo_dryRun opts of
      True -> return ()
      False -> uploadResults verbosity config docInfo
                                    mTgz mRpt logfile testfile coverageFile installOk
  where
    prepareTempBuildDir :: IO ()
    prepareTempBuildDir = do
      handleDoesNotExist () $
        removeDirectoryRecursive $ installDirectory opts
      createDirectory $ installDirectory opts
      createDirectoryIfMissing True $ resultsDirectory opts
      notice verbosity $ "Writing cabal.project for " ++ display (docInfoPackage docInfo)
      let projectFile = installDirectory opts </> "cabal.project"
      cabal opts "unpack" [show (docInfoTarGzURI config docInfo)] Nothing
      writeFile projectFile $ "packages: */*.cabal" -- ++ show (docInfoTarGzURI config docInfo)

    setTestOutcome :: String -> [String] -> [String]
    setTestOutcome _ []                  = []
    setTestOutcome a (xs:xt)
      | "tests-outcome: " `isPrefixOf` xs = ("tests-outcome: " ++ a) : xt
      | otherwise                         = xs : setTestOutcome a xt

    rewriteRpt:: Maybe FilePath -> Maybe String -> IO ()
    rewriteRpt (Just loc) (Just cnt) = do
      writeFile (loc <.> "temp") cnt
      renameFile (loc <.> "temp") loc
    rewriteRpt _ _ = do return ()

    setTestStatus :: Maybe FilePath -> Maybe String -> String -> IO ()
    setTestStatus mRpt buildReport testOutcome = do
        let buildReport' = fmap (unlines.setTestOutcome testOutcome) $ fmap lines buildReport
        rewriteRpt mRpt buildReport'

coveragePackage :: Verbosity -> BuildOpts -> DocInfo -> FilePath -> IO FilePath
coveragePackage verbosity opts docInfo loc = do
  let pkgid = docInfoPackage docInfo
      dir = takeDirectory loc
      mixLoc = dir </> ".." </> ".." </> "mix" </> display pkgid
      tixLoc = dir </> ".." </> ".." </> "tix" </> display pkgid </> display pkgid <.> "tix"
      all_args = ["report", tixLoc, "--hpcdir=" ++ mixLoc]
      coverageFile = (resultsDirectory opts) </> display pkgid <.> "coverage"
  buildCovg <- openFile coverageFile WriteMode

  ph <- runProcess "hpc" all_args Nothing
                        Nothing Nothing (Just buildCovg) (Just buildCovg)
  waitForProcess ph
  notice verbosity $ unlines
      [ "Code coverage results for " ++ display pkgid ++ ":"
      , coverageFile
      ]
  return coverageFile


testPackage :: Verbosity -> BuildOpts -> DocInfo -> IO (Maybe String, Maybe FilePath, Maybe FilePath)
testPackage verbosity opts docInfo = do
  let pkgid = docInfoPackage docInfo
      testLogFile = (installDirectory opts) </> display pkgid <.> "test"
      testReportFile = (installDirectory opts) </> "reports" </> display pkgid <.> "test"
      testResultFile = (resultsDirectory opts) </> display pkgid <.> "test"
      pkg_flags =
        ["all",
         "--enable-coverage",
         "--test-log=" ++ testReportFile,
         "--test-show-details=never",
         "--disable-optimization"]
  notice verbosity ("Testing " ++ display pkgid)

  buildLogHnd <- openFile testLogFile WriteMode

  void $ cabal opts "v2-test" pkg_flags (Just buildLogHnd)
  testLog <- readFile testLogFile

  let covgInd = elemIndex "Package coverage report written to" $ lines testLog
      hpcLoc = fmap (\x -> (lines testLog)!!(x+1)) covgInd

  testOutcome <- case ("Test suite" `isInfixOf` testLog) && (": PASS" `isInfixOf` testLog) of
        True  -> return (Just "Ok")
        False -> case ("Test suite" `isInfixOf` testLog) && (": FAIL" `isInfixOf` testLog) of
            True  -> return (Just "Failed")
            False -> return Nothing
  renameFile testLogFile testResultFile

  notice verbosity $ unlines
      [ "Test results for " ++ display pkgid ++ ":"
      , testResultFile
      ]
  return (testOutcome, hpcLoc, Just testResultFile)


-- | Build documentation and return @(Just tgz)@ for the built tgz file
-- on success, or @Nothing@ otherwise.
buildPackage :: Verbosity -> BuildOpts -> BuildConfig
             -> DocInfo
             -> IO (Maybe FilePath, Maybe FilePath, FilePath)
buildPackage verbosity opts config docInfo = do
    let pkgid = docInfoPackage docInfo
    notice verbosity ("Building " ++ display pkgid)


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
    void $ cabal opts "v1-install" pkg_flags (Just buildLogHnd)

    -- Grab the report for the package we want. Stash it for safe keeping.
    report <- handleDoesNotExist Nothing $ do
                renameFile (installDirectory opts </> "reports"
                                </> display pkgid <.> "report")
                           resultReportFile
                appendFile resultReportFile "doc-builder: True\n"
                -- TODO add real time
                appendFile resultReportFile "time:\n"
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
    createDirectoryIfMissing False $ installDirectory opts
    ph <- runProcess "cabal" all_args (Just $ installDirectory opts)
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

uploadResults :: Verbosity -> BuildConfig -> DocInfo -> Maybe FilePath
                    -> Maybe FilePath -> FilePath -> Maybe FilePath -> Maybe FilePath -> Bool -> IO ()
uploadResults verbosity config docInfo
              mdocsTarballFile buildReportFile buildLogFile testLogFile coverageFile installOk =
    httpSession verbosity "hackage-build" version $ do
      case mdocsTarballFile of
        Nothing              -> return ()
        Just docsTarballFile ->
          putDocsTarball config docInfo docsTarballFile

      putBuildFiles config docInfo buildReportFile buildLogFile testLogFile coverageFile installOk

withAuth :: BuildConfig -> Request -> Request
withAuth config req =
    noRedirects $ applyBasicAuth (BSS.pack $ bc_username config) (BSS.pack $ bc_password config) req

putDocsTarball :: BuildConfig -> DocInfo -> FilePath -> HttpSession ()
putDocsTarball config docInfo docsTarballFile = do
    body <- liftIO $ BS.readFile docsTarballFile
    req <- withAuth config <$> mkUploadRequest "PUT" uri mimetype mEncoding [] body
    runRequest req $ \rsp -> do
        rsp' <- responseReadBSL rsp
        checkStatus uri rsp'
  where
    uri = docInfoDocsURI config docInfo
    mimetype = "application/x-tar"
    mEncoding = Just "gzip"

putBuildFiles :: BuildConfig -> DocInfo -> Maybe FilePath
                    -> FilePath -> Maybe FilePath -> Maybe FilePath -> Bool -> HttpSession ()
putBuildFiles config docInfo reportFile buildLogFile testLogFile coverageFile installOk = do
    reportContent   <- liftIO $ traverse readFile reportFile
    logContent      <- liftIO $ readFile buildLogFile
    testContent     <- liftIO $ traverse readFile testLogFile
    coverageContent <- liftIO $ traverse readFile coverageFile
    let uri   = docInfoReports config docInfo
        body  = encode $ BR.BuildFiles reportContent (Just logContent) testContent coverageContent (not installOk)
    let headers = [ (hAccept, BSS.pack "application/json") ]
    req <- withAuth config <$> mkUploadRequest (BSS.pack "PUT") uri "application/json" Nothing headers body
    runRequest req $ \rsp -> do
        case statusCode $ responseStatus rsp of
          --TODO: fix server to not do give 303, 201 is more appropriate
          303 -> return ()
          _   -> do rsp' <- responseReadBSL rsp
                    checkStatus uri rsp'
                    fail "Unexpected response from server."


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
    flagBuildOlderGHC :: Bool,
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
  , flagBuildOlderGHC = False
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

  , Option [] ["build-older-ghc"]
      (NoArg (\opts -> opts { flagBuildOlderGHC = True }))
      "Build packages that were previously built with an older version of GHC"
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
                   bo_buildOlderGHC = flagBuildOlderGHC flags,
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
