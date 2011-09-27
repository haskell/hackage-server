{-# LANGUAGE PatternGuards #-}
module Main where

import Network.HTTP
import Network.Browser
import Network.URI (URI(..), URIAuth(..), parseURI, escapeURIString)

import Distribution.Server.LegacyImport.UploadLog as UploadLog (read, Entry(..))
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import Distribution.Server.Util.Index as PackageIndex (read)
import Distribution.Server.Util.Merge
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import System.Environment
import System.IO
import System.IO.Error
import System.Exit
import System.FilePath
import System.Directory
import System.Console.GetOpt
import qualified System.FilePath.Posix as Posix


data MirrorOpts = MirrorOpts {
                    srcURI       :: URI,
                    dstURI       :: URI,
                    stateDir     :: FilePath,
                    selectedPkgs :: [PackageId]
                  }

data PkgMirrorInfo = PkgMirrorInfo
                       PackageId
                       (Maybe UTCTime)
                       (Maybe UserName)
                       (Maybe UserId)
  deriving Show

main :: IO ()
main = topHandler $ do
  args <- getArgs
  (verbosity, opts) <- validateOpts args

  mirrorOnce verbosity opts


mirrorOnce :: Verbosity -> MirrorOpts -> IO ()
mirrorOnce verbosity opts = do

    let srcCacheFile = stateDir opts </> mkCacheFileName (srcURI opts)
        dstCacheFile = stateDir opts </> mkCacheFileName (dstURI opts)
    when (srcCacheFile == dstCacheFile) $
      die "source and destination cache files clash"

    createDirectoryIfMissing False (stateDir opts)

    httpSession verbosity $ do

      srcIndex <- downloadIndex (srcURI opts) srcCacheFile
      dstIndex <- downloadIndex (dstURI opts) dstCacheFile

      let pkgsMissingFromDest = diffIndex srcIndex dstIndex
          pkgsToMirror
            | null (selectedPkgs opts) = pkgsMissingFromDest
            | otherwise                = subsetIndex (selectedPkgs opts)
                                                     pkgsMissingFromDest

      ioAction $ notice verbosity $ show (length pkgsToMirror) ++ " packages to mirror."

      mirrorPackages verbosity opts pkgsToMirror

  where
    mkCacheFileName :: URI -> FilePath
    mkCacheFileName URI { uriAuthority = Just auth }
                        = makeValid (uriRegName auth ++ uriPort auth)
    mkCacheFileName uri = error $ "unexpected URI " ++ show uri


diffIndex :: [PkgMirrorInfo] -> [PkgMirrorInfo] -> [PkgMirrorInfo]
diffIndex as bs =
    [ pkg | OnlyInLeft pkg <- mergeBy (comparing mirrorPkgId)
                                       (sortBy (comparing mirrorPkgId) as)
                                       (sortBy (comparing mirrorPkgId) bs) ]
  where
    mirrorPkgId (PkgMirrorInfo pkgid _ _ _) = pkgid

subsetIndex :: [PackageId] -> [PkgMirrorInfo] -> [PkgMirrorInfo]
subsetIndex pkgids =
    filter (\(PkgMirrorInfo pkgid' _ _ _) -> anyMatchPackage pkgids pkgid')
  where
    anyMatchPackage :: [PackageId] -> PackageId -> Bool
    anyMatchPackage pkgids pkgid' =
      any (\pkgid -> matchPackage pkgid pkgid') pkgids

    matchPackage :: PackageId -> PackageId -> Bool
    matchPackage (PackageIdentifier name  (Version [] _))
                 (PackageIdentifier name' _             ) = name == name'
    matchPackage pkgid pkgid' = pkgid == pkgid'


mirrorPackages :: Verbosity -> MirrorOpts -> [PkgMirrorInfo] -> HttpSession ()
mirrorPackages verbosity opts pkgsToMirror = do

    let credentials = extractCredentials (dstURI opts)
    setAuthorityGen (provideAuthInfo credentials)

    sequence_
      [ do let srcBasedir
                 | isOldHackageURI (srcURI opts) = "packages" </> "archive"
                 | otherwise                     = "packages"
               srcPkgFile = display (packageName pkgid) </>
                            display (packageVersion pkgid) </>
                            display pkgid <.> "tar.gz"
               src     = srcURI   opts <//> srcBasedir </> srcPkgFile
               dst     = dstURI   opts <//> "package" </> display pkgid
               pkgfile = stateDir opts </>  display pkgid <.> "tar.gz"

           ioAction $ notice verbosity $ "\nmirroring " ++ display pkgid
           ok <-     downloadFile' src         pkgfile
           when ok $ putPackage    dst pkginfo pkgfile

      | pkginfo@(PkgMirrorInfo pkgid _ _ _) <- pkgsToMirror ]

  where
    provideAuthInfo :: Maybe (String, String) -> URI -> String -> IO (Maybe (String, String))
    provideAuthInfo credentials = \uri _realm -> do
      if hostName uri == hostName (dstURI opts) then return credentials
                                                else return Nothing

    hostName = fmap uriRegName . uriAuthority

    extractCredentials uri
      | Just authority <- uriAuthority uri
      , (username, ':':passwd0) <- break (==':') (uriUserInfo authority)
      , let passwd = takeWhile (/='@') passwd0
      , not (null username)
      , not (null passwd)
      = Just (username, passwd)
    extractCredentials _ = Nothing

putPackage :: URI -> PkgMirrorInfo -> FilePath -> HttpSession ()
putPackage baseURI (PkgMirrorInfo pkgid mtime muname muid) pkgFile = do
    putPackageTarball

{-  case mtime of
      Nothing   -> return ()
      Just time -> putPackageUploadTime -}
  where
    putPackageTarball = do
      pkgContent <- ioAction $ BS.readFile pkgFile
      let pkgURI = baseURI <//> display pkgid <.> "tar.gz"
      requestPUT pkgURI "application/x-gzip" pkgContent
{-
    putPackageUploadTime time = do
      (_, rsp) <- request (requestPUT pkgURI "text/plain" timeStr)
      where
        timeStr = formatTime defaultTimeLocale "%c" time
    putPackageUploadUser = do
      where
        nameStr = display uname
-}


----------------------------------------------------
-- Fetching info from source and destination servers
----------------------------------------------------

downloadIndex :: URI -> FilePath -> HttpSession [PkgMirrorInfo]
downloadIndex uri | isOldHackageURI uri = downloadOldIndex uri
                  | otherwise           = downloadNewIndex uri
  where

isOldHackageURI :: URI -> Bool
isOldHackageURI uri
  | Just auth <- uriAuthority uri = uriRegName auth == "hackage.haskell.org"
  | otherwise                     = False


downloadOldIndex :: URI -> FilePath -> HttpSession [PkgMirrorInfo]
downloadOldIndex uri cacheFile = do

    downloadFile indexURI indexFile
    downloadFile logURI logFile

    ioAction $ do

      pkgids <- withFile indexFile ReadMode $ \hnd -> do
        content <- BS.hGetContents hnd
        case PackageIndex.read (\pkgid _ -> pkgid) (GZip.decompress content) of
          Left  err  -> die $ "Error parsing index at " ++ show uri ++ ": " ++ err
          Right pkgs -> return pkgs

      log <- withFile logFile ReadMode $ \hnd -> do
        content <- hGetContents hnd
        case UploadLog.read content of
          Right log -> return log
          Left  err -> die $ "Error parsing log at " ++ show uri ++ ": " ++ err

      return (mergeLogInfo pkgids log)

  where
    indexURI  = uri <//> "packages" </> "archive" </> "00-index.tar.gz"
    indexFile = cacheFile <.> "tar.gz"

    logURI    = uri <//> "packages" </> "archive" </> "log"
    logFile   = cacheFile <.> "log"

    mergeLogInfo pkgids log =
        catMaybes
      . map selectDetails
      $ mergeBy (\pkgid entry -> compare pkgid (entryPkgId entry))
                (sort pkgids)
                ( map (maximumBy (comparing entryTime))
                . groupBy (equating  entryPkgId)
                . sortBy  (comparing entryPkgId)
                $ log )

    selectDetails (OnlyInRight _)     = Nothing
    selectDetails (OnlyInLeft  pkgid) =
      Just $ PkgMirrorInfo pkgid Nothing Nothing Nothing
    selectDetails (InBoth pkgid (UploadLog.Entry time uname _)) =
      Just $ PkgMirrorInfo pkgid (Just time) (Just uname) Nothing

    entryPkgId (Entry _ _ pkgid) = pkgid
    entryTime  (Entry time _ _)  = time


downloadNewIndex :: URI -> FilePath -> HttpSession [PkgMirrorInfo]
downloadNewIndex uri cacheFile = do
    downloadFile indexURI indexFile
    ioAction $ withFile indexFile ReadMode $ \hnd -> do
      content <- BS.hGetContents hnd
      case PackageIndex.read selectDetails (GZip.decompress content) of
        Left err   -> error $ "Error parsing index at " ++ show uri ++ ": " ++ err
        Right pkgs -> return pkgs

  where
    indexURI  = uri <//> "packages/00-index.tar.gz"
    indexFile = cacheFile <.> "tar.gz"

    selectDetails :: PackageId -> Tar.Entry -> PkgMirrorInfo
    selectDetails pkgid entry =
        PkgMirrorInfo
          pkgid
          (Just time)
          (if null username then Nothing else Just (UserName username))
          (if userid == 0   then Nothing else Just (UserId userid))
      where
        time     = epochTimeToUTC (Tar.entryTime entry)
        username = Tar.ownerName (Tar.entryOwnership entry)
        userid   = Tar.ownerId   (Tar.entryOwnership entry)

        epochTimeToUTC :: Tar.EpochTime -> UTCTime
        epochTimeToUTC = posixSecondsToUTCTime . realToFrac


-------------------------
-- HTTP utilities
-------------------------

infixr 5 <//>

(<//>) :: URI -> FilePath -> URI
uri <//> path = uri { uriPath = Posix.addTrailingPathSeparator (uriPath uri)
                                Posix.</> path }


type HttpSession a = BrowserAction (HandleStream ByteString) a

httpSession :: Verbosity -> HttpSession a -> IO a
httpSession verbosity action =
    browse $ do
      setUserAgent "hackage-mirror"
      setErrHandler die
      setOutHandler (debug verbosity)
      setAllowBasicAuth True
      setCheckForProxy True
      action

downloadFile :: URI -> FilePath -> HttpSession ()
downloadFile uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  let etagFile = file <.> "etag"
  metag <- ioAction $ catchJustDoesNotExistError
                        (Just <$> readFile etagFile)
                        (\_ -> return Nothing)
  case metag of
    Just etag -> do
      let headers = [mkHeader HdrIfNoneMatch (quote etag)]
      (_, rsp) <- request (Request uri GET headers BS.empty)
      case rspCode rsp of
        (3,0,4) -> out $ file ++ " unchanged with ETag " ++ etag
        (2,0,0) -> ioAction $ writeDowloadedFileAndEtag rsp
        _       -> err (showFailure uri rsp)

    Nothing -> do
      (_, rsp) <- request (Request uri GET [] BS.empty)
      case rspCode rsp of
        (2,0,0) -> ioAction $ writeDowloadedFileAndEtag rsp
        _       -> err (showFailure uri rsp)

  where
    writeDowloadedFileAndEtag rsp = do
      BS.writeFile file (rspBody rsp)
      setETag file (unquote <$> findHeader HdrETag rsp)

getETag :: FilePath -> IO (Maybe String)
getETag file =
    catchJustDoesNotExistError
      (Just <$> readFile (file </> ".etag"))
      (\_ -> return Nothing)

setETag :: FilePath -> Maybe String -> IO ()
setETag file Nothing     = catchJustDoesNotExistError
                             (removeFile (file <.> "etag"))
                             (\_ -> return ())
setETag file (Just etag) = writeFile  (file <.> "etag") etag

catchJustDoesNotExistError =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)

quote   s = '"' : s ++ ['"']

unquote ('"':s) = go s
  where
    go []       = []
    go ('"':[]) = []
    go (c:cs)   = c : go cs
unquote     s   = s

-- AAARG! total lack of exception handling in HTTP monad!
downloadFile' :: URI -> FilePath -> HttpSession Bool
downloadFile' uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  mcontent <- requestGET' uri
  case mcontent of
    Nothing      -> do out $ "404 " ++ show uri
                       return False

    Just content -> do ioAction $ BS.writeFile file content
                       return True

requestGET :: URI -> HttpSession ByteString
requestGET uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    checkStatus uri rsp
    return (rspBody rsp)
  where
    headers = []

-- Really annoying!
requestGET' :: URI -> HttpSession (Maybe ByteString)
requestGET' uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    case rspCode rsp of
      (4,0,4) -> return Nothing
      _       -> do checkStatus uri rsp
                    return (Just (rspBody rsp))
  where
    headers = []


requestPUT :: URI -> String -> ByteString -> HttpSession ()
requestPUT uri mimetype body = do
    (_, rsp) <- request (Request uri PUT headers body)
    checkStatus uri rsp
  where
    headers = [ Header HdrContentLength (show (BS.length body))
              , Header HdrContentType mimetype ]


checkStatus :: URI -> Response ByteString -> HttpSession ()
checkStatus uri rsp = case rspCode rsp of
  (2,0,0) -> return ()
  (4,0,0) -> ioAction (warn normal (showFailure uri rsp)) >> return ()
  code    -> err (showFailure uri rsp)

showFailure uri rsp =
    show (rspCode rsp) ++ " " ++ rspReason rsp ++ show uri
 ++ case lookupHeader HdrContentType (rspHeaders rsp) of
      Just mimetype | "text/plain" `isPrefixOf` mimetype
                   -> '\n' : BS.unpack (rspBody rsp)
      _            -> ""

-------------------------
-- Command line handling
-------------------------

data MirrorFlags = MirrorFlags {
    flagCacheDir  :: Maybe FilePath,
    flagVerbosity :: Verbosity,
    flagHelp      :: Bool
}

emptyMirrorFlags :: MirrorFlags
emptyMirrorFlags = MirrorFlags Nothing normal False

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
      "Where to put files during mirroring"
  ]

validateURI :: String -> Either String URI
validateURI str = case parseURI str of
  Nothing                          -> Left ("invalid URL " ++ str)
  Just uri
    | uriScheme uri /= "http:"     -> Left ("only http URLs are supported " ++ str)
    | isNothing (uriAuthority uri) -> Left ("server name required in URL " ++ str)
    | otherwise                    -> Right uri

validateOpts :: [String] -> IO (Verbosity, MirrorOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute mirrorFlagDescrs args
        flags = accum flags0 emptyMirrorFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (from:to:pkgstrs) -> case (validateURI from, validateURI to) of
        (Left err, _) -> die err
        (_, Left err) -> die err
        (Right fromURI, Right toURI) -> do
          let pkgstrs' = [ (pkgstr, simpleParse pkgstr) | pkgstr <- pkgstrs ]
              pkgs     = [ pkgid  | (_, Just pkgid)   <- pkgstrs' ]
              errs     = [ pkgstr | (pkgstr, Nothing) <- pkgstrs' ]
          case errs of
            (err:_) -> die $ "'" ++ err ++ "' is not a valid package name or id"
            _       -> return ()

          return $ (,) (flagVerbosity flags) MirrorOpts {
            srcURI       = fromURI,
            dstURI       = toURI,
            stateDir     = fromMaybe "mirror-cache" (flagCacheDir flags),
            selectedPkgs = pkgs
          }

      _ -> do putStrLn $ "Expected two URLs, a source and destination.\n"
                      ++ "See hackage-mirror --help for details and an example."
              exitFailure

  where
    printUsage = do
      putStrLn $ usageInfo usageHeader mirrorFlagDescrs ++ helpExampleStr
      exitSuccess
    usageHeader = helpDescrStr
               ++ "Usage: hackage-mirror fromURL toURL [packages] [options]\n"
               ++ "Options:"
    printErrors errs = do
      putStrLn $ concat errs ++ "Try --help."
      exitFailure

    accum flags = foldr (flip (.)) id flags

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
  , "    hackage-mirror http://hackage.haskell.org/ \\"
  , "                   http://foo:bar@localhost:8080/  zlib"
  , "  This will synchronise all versions of the 'zlib' package and then exit."
  ]
