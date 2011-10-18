{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, FlexibleInstances #-}
module Main where

import Network.HTTP
import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client (validateHackageURI, validatePackageIds, PkgIndexInfo(..))
import Distribution.Server.LegacyImport.UploadLog as UploadLog (read, Entry(..))
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import Distribution.Server.Util.Index as PackageIndex (read)
import Distribution.Server.Util.Merge
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils hiding (warn)

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IORef

import Control.Exception
import Control.Concurrent
import Data.Time
import Data.Time.Clock.POSIX
import System.Locale
import System.Random
import System.Environment
import System.IO
import System.IO.Error hiding (catch)
import System.Exit
import System.FilePath
import System.Directory
import System.Console.GetOpt
import qualified System.FilePath.Posix as Posix
import Prelude hiding (catch)

import Paths_hackage_server (version)


data MirrorOpts = MirrorOpts {
                    srcURI       :: URI,
                    dstURI       :: URI,
                    stateDir     :: FilePath,
                    selectedPkgs :: [PackageId],
                    continuous   :: Maybe Int -- if so, interval in minutes
                  }

data MirrorEnv = MirrorEnv {
                   srcCacheDir :: FilePath,
                   dstCacheDir :: FilePath,
                   missingPkgsFile      :: FilePath,
                   unmirrorablePkgsFile :: FilePath
                 }
  deriving Show

data MirrorState = MirrorState {
                     missingPkgs          :: Set PackageId,
                     unmirrorablePkgs     :: Set PackageId
                   }
  deriving (Eq, Show)


main :: IO ()
main = toplevelHandler $ do
  args <- getArgs
  (verbosity, opts) <- validateOpts args

  (env, st) <- mirrorInit verbosity opts

  case continuous opts of
    Nothing       -> mirrorOneShot verbosity opts env st
    Just interval -> cron verbosity interval
                          (mirrorIteration verbosity opts env) st


mirrorInit :: Verbosity -> MirrorOpts -> IO (MirrorEnv, MirrorState)
mirrorInit verbosity opts = do

    let srcName     = mkCacheFileName (srcURI opts)
        dstName     = mkCacheFileName (dstURI opts)
        srcCacheDir = stateDir opts </> srcName
        dstCacheDir = stateDir opts </> dstName

    when (srcCacheDir == dstCacheDir) $
      die "source and destination cache files clash"

    when (continuous opts == Just 0) $
      warn verbosity "A sync interval of zero is a seriously bad idea!"

    when (isOldHackageURI (srcURI opts)
       && maybe False (<30) (continuous opts)) $
       die $ "Please don't hit the central hackage.haskell.org "
          ++ "more frequently than every 30 minutes."

    createDirectoryIfMissing False (stateDir opts)
    createDirectoryIfMissing False srcCacheDir
    createDirectoryIfMissing False dstCacheDir

    let missingPkgsFile      = stateDir opts
                           </> "missing-" ++ srcName
        -- being unmirrorable is a function of the combination of the source and
        -- destination, so the cache file uses both names.
        unmirrorablePkgsFile = stateDir opts
                           </> "unmirrorable-" ++ srcName ++ "-" ++ dstName
    missingPkgs      <- readPkgProblemFile missingPkgsFile
    unmirrorablePkgs <- readPkgProblemFile unmirrorablePkgsFile

    return (MirrorEnv srcCacheDir dstCacheDir
                      missingPkgsFile unmirrorablePkgsFile
           ,MirrorState missingPkgs unmirrorablePkgs)
  where
    mkCacheFileName :: URI -> FilePath
    mkCacheFileName URI { uriAuthority = Just auth }
                        = makeValid (uriRegName auth ++ uriPort auth)
    mkCacheFileName uri = error $ "unexpected URI " ++ show uri


readPkgProblemFile :: FilePath -> IO (Set PackageId)
readPkgProblemFile file = do
  exists <- doesFileExist file
  if exists
    then evaluate . Set.fromList
                  . catMaybes . map simpleParse . lines
                =<< readFile file
    else return Set.empty

writePkgProblemFile :: FilePath -> Set PackageId -> IO ()
writePkgProblemFile file =
  writeFile file . unlines . map display . Set.toList


cron :: Verbosity -> Int -> (a -> IO a) -> a -> IO ()
cron verbosity interval action x = do
    x' <- action x

    interval' <- pertabate interval
    logNextSyncMessage interval'
    wait interval'
    cron verbosity interval action x'

  where
    -- to stop all mirror clients hitting the server at exactly the same time
    -- we randomly adjust the wait time by +/- 10%
    pertabate i = let deviation = i `div` 10
                   in randomRIO (i + deviation, i - deviation)

    -- Annoyingly, threadDelay takes an Int number of microseconds, so we cannot
    -- wait much longer than an hour. So have to wait repeatedly. Sigh.
    wait minutes | minutes > 60 = do threadDelay (60 * 60 * 1000000)
                                     wait (minutes - 60)
                 | otherwise    = threadDelay (minutes * 60 * 1000000)

    logNextSyncMessage minutes = do
      now       <- getCurrentTime
      tz        <- getCurrentTimeZone
      let nextSync = addUTCTime (fromIntegral (60 * minutes)) now
      notice verbosity $
          "Next sync will be in " ++ show minutes ++ " minutes, at "
       ++ formatTime defaultTimeLocale "%R %Z" (utcToZonedTime tz nextSync)


---------------------------
-- Main mirroring logic
---------------------------

mirrorOneShot :: Verbosity -> MirrorOpts -> MirrorEnv -> MirrorState -> IO ()
mirrorOneShot verbosity opts env st = do

    (merr, _) <- mirrorOnce verbosity False opts env st

    case merr of
      Nothing  -> return ()
      Just err -> fail (formatMirrorError err)


mirrorIteration :: Verbosity -> MirrorOpts -> MirrorEnv
                -> MirrorState -> IO MirrorState
mirrorIteration verbosity opts env st = do

    (merr, st') <- mirrorOnce verbosity True opts env st

    when (st' /= st) $
      savePackagesState env st'

    case merr of
      Nothing          -> return ()
      Just Interrupted -> throw UserInterrupt
      Just err         -> do
        warn verbosity (formatMirrorError err)
        notice verbosity "Abandoning this mirroring attempt."
    return st'


savePackagesState :: MirrorEnv  -> MirrorState -> IO ()
savePackagesState (MirrorEnv _ _ missingPkgsFile unmirrorablePkgsFile)
                  (MirrorState missingPkgs unmirrorablePkgs) = do
  writePkgProblemFile missingPkgsFile      missingPkgs
  writePkgProblemFile unmirrorablePkgsFile unmirrorablePkgs


mirrorOnce :: Verbosity -> Bool -> MirrorOpts -> MirrorEnv
           -> MirrorState -> IO (Maybe MirrorError, MirrorState)
mirrorOnce verbosity keepGoing opts
           (MirrorEnv srcCacheDir dstCacheDir missingPkgsFile unmirrorablePkgsFile)
           st@(MirrorState missingPkgs unmirrorablePkgs) =

    mirrorSession verbosity keepGoing st $ do

      srcIndex <- downloadIndex (srcURI opts) srcCacheDir
      dstIndex <- downloadIndex (dstURI opts) dstCacheDir

      let pkgsMissingFromDest = diffIndex srcIndex dstIndex
          pkgsToMirror
            | null (selectedPkgs opts) = pkgsMissingFromDest
            | otherwise                = subsetIndex (selectedPkgs opts)
                                                     pkgsMissingFromDest
          pkgsToMirror' = filter (\(PkgIndexInfo pkg _ _ _) ->
                                     pkg `Set.notMember` missingPkgs
                                  && pkg `Set.notMember` unmirrorablePkgs )
                                 pkgsToMirror
          mirrorCount   = length pkgsToMirror'
          ignoreCount   = length pkgsToMirror - mirrorCount

      liftIO $ notice verbosity $
          show mirrorCount ++ " packages to mirror."
       ++ if ignoreCount == 0 then ""
            else " Ignoring " ++ show ignoreCount
              ++ " package(s) that cannot be mirrored\n(for details see "
              ++ missingPkgsFile ++ " and " ++ unmirrorablePkgsFile ++ ")"

      mirrorPackages verbosity opts pkgsToMirror'

  where
    mirrorSession :: Verbosity -> Bool -> MirrorState
                  -> MirrorSession a -> IO (Maybe MirrorError, MirrorState)
    mirrorSession verbosity keepGoing st action =
      liftM (\(eerr, st') -> (either Just (const Nothing) eerr,
                              fromErrorState st')) $
      runMirrorSession verbosity keepGoing (toErrorState st) $ do
        browserAction $ do
          setUserAgent  ("hackage-mirror/" ++ display version)
          setErrHandler (warn  verbosity)
          setOutHandler (debug verbosity)
          setAllowBasicAuth True
          setCheckForProxy True
        action


diffIndex :: [PkgIndexInfo] -> [PkgIndexInfo] -> [PkgIndexInfo]
diffIndex as bs =
    [ pkg | OnlyInLeft pkg <- mergeBy (comparing mirrorPkgId)
                                       (sortBy (comparing mirrorPkgId) as)
                                       (sortBy (comparing mirrorPkgId) bs) ]
  where
    mirrorPkgId (PkgIndexInfo pkgid _ _ _) = pkgid

subsetIndex :: [PackageId] -> [PkgIndexInfo] -> [PkgIndexInfo]
subsetIndex pkgids =
    filter (\(PkgIndexInfo pkgid' _ _ _) -> anyMatchPackage pkgids pkgid')
  where
    anyMatchPackage :: [PackageId] -> PackageId -> Bool
    anyMatchPackage pkgids pkgid' =
      any (\pkgid -> matchPackage pkgid pkgid') pkgids

    matchPackage :: PackageId -> PackageId -> Bool
    matchPackage (PackageIdentifier name  (Version [] _))
                 (PackageIdentifier name' _             ) = name == name'
    matchPackage pkgid pkgid' = pkgid == pkgid'


mirrorPackages :: Verbosity -> MirrorOpts -> [PkgIndexInfo] -> MirrorSession ()
mirrorPackages verbosity opts pkgsToMirror = do

    let credentials = extractCredentials (dstURI opts)
    browserAction $ setAuthorityGen (provideAuthInfo credentials)

    mapM_ mirrorPackage pkgsToMirror

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

    mirrorPackage pkginfo@(PkgIndexInfo pkgid _ _ _) = do
      let srcBasedir
             | isOldHackageURI (srcURI opts) = "packages" </> "archive"
             | otherwise                     = "packages"
          srcPkgFile = display (packageName pkgid) </>
                       display (packageVersion pkgid) </>
                       display pkgid <.> "tar.gz"
          src     = srcURI   opts <//> srcBasedir </> srcPkgFile
          dst     = dstURI   opts <//> "package" </> display pkgid
          pkgfile = stateDir opts </>  display pkgid <.> "tar.gz"
          --TODO: pass env and srcCacheDir instead of opts

      liftIO $ notice verbosity $ "mirroring " ++ display pkgid
      rsp <- browserAction $ downloadFile' src pkgfile
      case rsp of
        Just err -> notifyResponse (GetPackageFailed err pkgid)

        Nothing  -> do notifyResponse GetPackageOk
                       putPackage dst pkginfo pkgfile



putPackage :: URI -> PkgIndexInfo -> FilePath -> MirrorSession ()
putPackage baseURI (PkgIndexInfo pkgid mtime muname _muid) pkgFile = do
    putPackageTarball
    maybe (return ()) putPackageUploadTime mtime
    maybe (return ()) putPackageUploader muname
  where
    pkgURI = baseURI <//> display pkgid <.> "tar.gz"

    putPackageTarball = do
      pkgContent <- liftIO $ BS.readFile pkgFile
      rsp <- browserAction $ requestPUT pkgURI "application/x-gzip" pkgContent
      case rsp of
        Just err -> notifyResponse (PutPackageFailed err pkgid)

        Nothing  -> do notifyResponse PutPackageOk
                       liftIO $ removeFile pkgFile
      --TODO: think about in what situations we delete the file
      -- and if we should actually cache it if we don't sucessfully upload.

      -- TODO: perhaps we shouldn't report failure for the whole package if
      -- we fail to set the upload time/uploader

    toBS = BS.pack . toUTF8

    putPackageUploadTime time = do
      let timeStr = formatTime defaultTimeLocale "%c" time
      rsp <- browserAction $ requestPUT (pkgURI <//> "upload-time") "text/plain" (toBS timeStr)
      case rsp of
        Just err -> notifyResponse (PutPackageFailed err pkgid)
        Nothing  -> notifyResponse PutPackageOk

    putPackageUploader uname = do
      let nameStr = display uname
      rsp <- browserAction $ requestPUT (pkgURI <//> "uploader") "text/plain" (toBS nameStr)
      case rsp of
        Just err -> notifyResponse (PutPackageFailed err pkgid)
        Nothing  -> notifyResponse PutPackageOk

-----------------------------
-- Error handling strategy
-----------------------------

-- There's two general classes of errors we have to consider:
--  - systematic errors that apply to all packages
--  - errors that only apply to individual packages
--
-- For the first class we want to fail and not continue mirroring.
--
-- For the second class we want to keep going and try mirroring other packages.
-- In addition we want to remember the packages involved persistently so that
-- we don't keep trying again and again to mirror packages that simply cannot
-- be mirrored.
--
-- We want the mirroring to be robust. So we don't want to have temporary or
-- individual package errors make us fail overall.
--
-- To add further complication, it can be hard to distinguish a systematic
-- error from an individual per-package error. Our strategy may have to be
-- complex and based on counting various events to make best guesses.
--
-- Since we expect our error handling stategy we try to separate it from the
-- main mirroring logic.
--
-- We make a new monad to hold the plumbing for our strategy. We make it to
-- be a state monad so that we can accumulating info about non-fatal errors in
-- individual packages. We make it an error monad to deal with fatal errors.
--
-- We layer state and error on top of the HTTP Browser monad. The layering
-- order of the state vs error is such that errors do not roll back the changed
-- state. This is because we want (at least the option) to keep the info about
-- individual packages when we encounter a systematic error.
--

newtype MirrorSession a
      = MirrorSession {
          unMirror :: ErrorT MirrorError
                        (ReaderT (Verbosity, Bool, IORef ErrorState)
                          (BrowserAction (HandleStream ByteString)))
                        a
        }
  deriving (Functor, Monad, MonadIO,
            MonadError MirrorError)

instance MonadReader (Verbosity, Bool) MirrorSession where
  ask       = MirrorSession (fmap (\(x,y,_) -> (x,y)) ask)
  local f m = MirrorSession (local f' (unMirror m))
                where
                  f' (x,y,z) = (x',y',z) where (x',y') = f (x,y)

instance MonadState ErrorState MirrorSession where
  get   = MirrorSession (ask >>= \(_,_,stRef) -> liftIO (readIORef stRef))
  put x = MirrorSession (ask >>= \(_,_,stRef) -> liftIO (writeIORef stRef x))

browserAction :: BrowserAction (HandleStream ByteString) a -> MirrorSession a
browserAction = MirrorSession . lift . lift

runMirrorSession :: Verbosity -> Bool -> ErrorState -> MirrorSession a
                 -> IO (Either MirrorError a, ErrorState)
runMirrorSession verbosity keepGoing st (MirrorSession m) = do
  stRef <- newIORef st
  result <- browse (runReaderT (runErrorT m) (verbosity, keepGoing, stRef))
              `catch` (return . Left . MirrorIOError)
              `catch` \ae -> case ae of
                               UserInterrupt -> return (Left Interrupted)
                               _             -> throw ae

  st' <- readIORef stRef
  return (result, st')


-- So that's the plumbing. The actual policy/strategy is implemented by an
-- action in the monad where we inform it of the interesting events, which is
-- basically all the gets and puts, the packages and their http response codes.
-- This policy action behaves like a state machine action, updating the monad
-- state and if it decides it has to fail hard, by making use of error monad.

data MirrorError = MirrorGeneralError String
                 | MirrorIOError IOError
                 | GetEntityError Entity ErrorResponse
                 | ParseEntityError Entity URI String
                 | PutPackageError PackageId ErrorResponse
                 | Interrupted
  deriving Show

data Entity = EntityIndex | EntityLog | EntityPackage PackageId
  deriving Show

instance Error MirrorError where
  strMsg = MirrorGeneralError

formatMirrorError :: MirrorError -> String
formatMirrorError (MirrorGeneralError msg)  = msg
formatMirrorError (MirrorIOError      ioe)  = formatIOError ioe
formatMirrorError (GetEntityError entity rsp) =
    "Failed to download " ++ formatEntity entity
 ++ ",\n  " ++ formatErrorResponse rsp
formatMirrorError (ParseEntityError entity uri err) =
    "Error parsing " ++ formatEntity entity
 ++ " at " ++ show uri ++ ": " ++ err
formatMirrorError (PutPackageError pkgid rsp) =
    "Failed to upload package "  ++ display pkgid
 ++ ",\n  " ++ formatErrorResponse rsp
formatMirrorError Interrupted = error "formatMirrorError: Interrupted"

formatEntity :: Entity -> String
formatEntity EntityIndex           = "the package index"
formatEntity EntityLog             = "the package upload log"
formatEntity (EntityPackage pkgid) = "package " ++ display pkgid

data ErrorState = ErrorState {
                    missing      :: Set PackageId,
                    unmirrorable :: Set PackageId
                  }
  deriving Show

toErrorState :: MirrorState -> ErrorState
toErrorState (MirrorState missing unmirrorable) =
  ErrorState missing unmirrorable

fromErrorState :: ErrorState -> MirrorState
fromErrorState (ErrorState missing unmirrorable) =
  MirrorState missing unmirrorable

data MirrorEvent = GetIndexOk
                 | GetPackageOk | GetPackageFailed ErrorResponse PackageId
                 | PutPackageOk | PutPackageFailed ErrorResponse PackageId

notifyResponse :: MirrorEvent -> MirrorSession ()
notifyResponse e = do
    st <- get
    (verbosity, keepGoing) <- ask
    st' <- handleEvent verbosity keepGoing e st
    put st'
  where
    handleEvent _ False e st = case e of
      GetIndexOk   -> return st
      GetPackageOk -> return st
      PutPackageOk -> return st
      GetPackageFailed rsp pkgid ->
        throwError (GetEntityError (EntityPackage pkgid) rsp)
      PutPackageFailed rsp pkgid ->
        throwError (PutPackageError pkgid rsp)

    handleEvent verbosity True e st = case e of
      GetIndexOk   -> return st
      GetPackageOk -> return st
      PutPackageOk -> return st
      GetPackageFailed rsp@(ErrorResponse _ code _ _) pkgid
        | code == (4,0,4) -> do
            liftIO $ warn verbosity $
              formatMirrorError (GetEntityError (EntityPackage pkgid) rsp)
            return st {
              missing = Set.insert pkgid (missing st)
            }
        | otherwise -> throwError (GetEntityError (EntityPackage pkgid) rsp)

      PutPackageFailed rsp@(ErrorResponse _ code _ _) pkgid
        | code == (4,0,0) -> do
            liftIO $ warn verbosity $
              formatMirrorError (PutPackageError pkgid rsp)
            return st {
              unmirrorable = Set.insert pkgid (unmirrorable st)
            }
        | otherwise -> throwError (PutPackageError pkgid rsp)


----------------------------------------------------
-- Fetching info from source and destination servers
----------------------------------------------------

downloadIndex :: URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadIndex uri | isOldHackageURI uri = downloadOldIndex uri
                  | otherwise           = downloadNewIndex uri
  where

isOldHackageURI :: URI -> Bool
isOldHackageURI uri
  | Just auth <- uriAuthority uri = uriRegName auth == "hackage.haskell.org"
  | otherwise                     = False


downloadOldIndex :: URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadOldIndex uri cacheDir = do

    rsp <- browserAction $ downloadFile indexURI indexFile
    case rsp of
      Nothing  -> notifyResponse GetIndexOk
      Just err -> throwError (GetEntityError EntityIndex err)

    rsp <- browserAction $ downloadFile logURI logFile
    case rsp of
      Nothing  -> notifyResponse GetIndexOk
      Just err -> throwError (GetEntityError EntityLog err)

    res <- liftIO $
      withFile indexFile ReadMode $ \hnd ->
        evaluate . PackageIndex.read (\pkgid _ -> pkgid)
                 . GZip.decompress
               =<< BS.hGetContents hnd
    pkgids <- case res of
      Left  err  -> throwError (ParseEntityError EntityIndex uri err)
      Right pkgs -> return pkgs

    res <- liftIO $
      withFile logFile ReadMode $ \hnd ->
        evaluate . UploadLog.read =<< hGetContents hnd
    log <- case res of
      Left  err -> throwError (ParseEntityError EntityLog uri err)
      Right log -> return log

    return (mergeLogInfo pkgids log)

  where
    indexURI  = uri <//> "packages" </> "archive" </> "00-index.tar.gz"
    indexFile = cacheDir </> "00-index.tar.gz"

    logURI    = uri <//> "packages" </> "archive" </> "log"
    logFile   = cacheDir </> "log"

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
      Just $ PkgIndexInfo pkgid Nothing Nothing Nothing
    selectDetails (InBoth pkgid (UploadLog.Entry time uname _)) =
      Just $ PkgIndexInfo pkgid (Just time) (Just uname) Nothing

    entryPkgId (Entry _ _ pkgid) = pkgid
    entryTime  (Entry time _ _)  = time


downloadNewIndex :: URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadNewIndex uri cacheDir = do
    rsp <- browserAction $ downloadFile indexURI indexFile
    case rsp of
      Just err -> throwError (GetEntityError EntityIndex err)
      Nothing  -> do
        notifyResponse GetIndexOk
        res <- liftIO $ withFile indexFile ReadMode $ \hnd ->
                 evaluate . PackageIndex.read selectDetails
                          . GZip.decompress
                        =<< BS.hGetContents hnd
        case res of
          Left  err  -> throwError (ParseEntityError EntityIndex uri err)
          Right pkgs -> return pkgs

  where
    indexURI  = uri <//> "packages/00-index.tar.gz"
    indexFile = cacheDir </> "00-index.tar.gz"

    selectDetails :: PackageId -> Tar.Entry -> PkgIndexInfo
    selectDetails pkgid entry =
        PkgIndexInfo
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

downloadFile :: URI -> FilePath -> HttpSession (Maybe ErrorResponse)
downloadFile uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  let etagFile = file <.> "etag"
  metag <- liftIO $ catchJustDoesNotExistError
                        (Just <$> readFile etagFile)
                        (\_ -> return Nothing)
  case metag of
    Just etag -> do
      let headers = [mkHeader HdrIfNoneMatch (quote etag)]
      (_, rsp) <- request (Request uri GET headers BS.empty)
      case rspCode rsp of
        (3,0,4) -> do out $ file ++ " unchanged with ETag " ++ etag
                      return Nothing
        (2,0,0) -> do liftIO $ writeDowloadedFileAndEtag rsp
                      return Nothing
        _       -> return (Just (mkErrorResponse uri rsp))

    Nothing -> do
      (_, rsp) <- request (Request uri GET [] BS.empty)
      case rspCode rsp of
        (2,0,0) -> do liftIO $ writeDowloadedFileAndEtag rsp
                      return Nothing
        _       -> return (Just (mkErrorResponse uri rsp))

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

catchJustDoesNotExistError :: IO a -> (IOError -> IO a) -> IO a
catchJustDoesNotExistError =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)

quote :: String -> String
quote   s = '"' : s ++ ['"']

unquote :: String -> String
unquote ('"':s) = go s
  where
    go []       = []
    go ('"':[]) = []
    go (c:cs)   = c : go cs
unquote     s   = s


downloadFile' :: URI -> FilePath -> HttpSession (Maybe ErrorResponse)
downloadFile' uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  rsp <- requestGET uri
  case rsp of
    Left  err     -> return (Just err)
    Right content -> do liftIO $ BS.writeFile file content
                        --TODO: check we wrote the expected length.
                        return Nothing

requestGET :: URI -> HttpSession (Either ErrorResponse ByteString)
requestGET uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    case rspCode rsp of
      (2,0,0) -> return (Right (rspBody rsp))
      _       -> return (Left  (mkErrorResponse uri rsp))
  where
    headers = []


requestPUT :: URI -> String -> ByteString -> HttpSession (Maybe ErrorResponse)
requestPUT uri mimetype body = do
    (_, rsp) <- request (Request uri PUT headers body)
    case rspCode rsp of
      (2,_,_) -> return Nothing
      _       -> return (Just (mkErrorResponse uri rsp))
  where
    headers = [ Header HdrContentLength (show (BS.length body))
              , Header HdrContentType mimetype ]

data ErrorResponse = ErrorResponse URI ResponseCode String (Maybe String)
  deriving Show

mkErrorResponse :: URI -> Response ByteString -> ErrorResponse
mkErrorResponse uri rsp =
    ErrorResponse uri (rspCode rsp) (rspReason rsp) mBody
  where
    mBody = case lookupHeader HdrContentType (rspHeaders rsp) of
      Just mimetype | "text/plain" `isPrefixOf` mimetype
                   -> Just (BS.unpack (rspBody rsp))
      _            -> Nothing

formatErrorResponse :: ErrorResponse -> String
formatErrorResponse (ErrorResponse uri (a,b,c) reason mBody) =
    "HTTP error code " ++ show a ++ show b ++ show c
 ++ ", " ++ reason ++ "\n  " ++  show uri
 ++ maybe "" (('\n':) . unlines . map ("  "++) . lines . wrapText) mBody

-------------------------
-- Command line handling
-------------------------

data MirrorFlags = MirrorFlags {
    flagCacheDir  :: Maybe FilePath,
    flagContinuous:: Bool,
    flagInterval  :: Maybe String,
    flagVerbosity :: Verbosity,
    flagHelp      :: Bool
}

defaultMirrorFlags :: MirrorFlags
defaultMirrorFlags = MirrorFlags
                       Nothing False Nothing normal False

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
  ]

validateOpts :: [String] -> IO (Verbosity, MirrorOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute mirrorFlagDescrs args
        flags = accum flags0 defaultMirrorFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (from:to:pkgstrs) -> case (validateHackageURI from, validateHackageURI to) of
        (Left err, _) -> die err
        (_, Left err) -> die err
        (Right fromURI, Right toURI) -> case (mpkgs, minterval) of
          (Left err, _) -> die err
          (_, Left err) -> die err
          (Right pkgs, Right interval) ->
             return $ (,) (flagVerbosity flags) MirrorOpts {
                 srcURI       = fromURI,
                 dstURI       = toURI,
                 stateDir     = fromMaybe "mirror-cache" (flagCacheDir flags),
                 selectedPkgs = pkgs,
                 continuous   = if flagContinuous flags
                                  then Just interval
                                  else Nothing
               }
          where
            mpkgs     = validatePackageIds pkgstrs
            minterval = validateInterval (flagInterval flags)

      _ -> die $ "Expected two URLs, a source and destination.\n"
              ++ "See hackage-mirror --help for details and an example."

  where
    printUsage = do
      putStrLn $ usageInfo usageHeader mirrorFlagDescrs ++ helpExampleStr
      exitSuccess
    usageHeader = helpDescrStr
               ++ "Usage: hackage-mirror fromURL toURL [packages] [options]\n"
               ++ "Options:"
    printErrors errs = die $ concat errs ++ "Try --help."

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
  , "    hackage-mirror http://hackage.haskell.org/ \\"
  , "                   http://foo:bar@localhost:8080/  zlib"
  , "  This will synchronise all versions of the 'zlib' package and then exit."
  ]

toplevelHandler :: IO a -> IO a
toplevelHandler =
    handle $ \ioe -> do
      hFlush stdout
      pname <- getProgName
      hPutStrLn stderr (pname ++ ": " ++ formatIOError ioe)
      exitWith (ExitFailure 1)

formatIOError :: IOError -> String
formatIOError ioe
  | isUserError ioe = file ++ location ++ detail
  | otherwise       = show ioe
  where
    file         = case ioeGetFileName ioe of
                     Nothing   -> ""
                     Just path -> path ++ ": "
    location     = case ioeGetLocation ioe of
                    ""  -> ""
                    loc -> loc ++ ": "
    detail       = ioeGetErrorString ioe

warn :: Verbosity -> String -> IO ()
warn verbosity msg =
  when (verbosity >= normal) $
    putStrLn ("Warning: " ++ msg)
