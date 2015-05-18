{-# LANGUAGE PatternGuards, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, CPP #-}
module Main where

import Network.HTTP
import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client (validateHackageURI, validatePackageIds, PkgIndexInfo(..))
import Distribution.Client.UploadLog as UploadLog (read, Entry(..))
import Distribution.Client.Cron (cron, rethrowSignalsAsExceptions, Signal(..))
import Distribution.Server.Util.Parse (packUTF8, unpackUTF8)

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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Distribution.Server.Util.GZip as GZip
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.Set as Set
import Data.Set (Set)
import Data.IORef

import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Locale.Compat (defaultTimeLocale)
import System.Environment
import System.IO
import System.Exit(exitSuccess, exitWith, ExitCode(..))
import System.FilePath
import System.Directory
import System.Console.GetOpt
import qualified System.FilePath.Posix as Posix

#if __GLASGOW_HASKELL__ >= 706
import System.IO.Error
#else
import System.IO.Error hiding (catch)
import Prelude hiding (catch)
#endif

import Paths_hackage_server (version)

data MirrorOpts = MirrorOpts {
                    srcURI          :: URI,
                    dstURI          :: URI,
                    stateDir        :: FilePath,
                    selectedPkgs    :: [PackageId],
                    continuous      :: Maybe Int, -- if so, interval in minutes
                    mo_keepGoing    :: Bool,
                    mirrorUploaders :: Bool,
                    srcIsOldHackage :: Bool
                  }

data MirrorEnv = MirrorEnv {
                   me_srcCacheDir :: FilePath,
                   me_dstCacheDir :: FilePath,
                   me_missingPkgsFile      :: FilePath,
                   me_unmirrorablePkgsFile :: FilePath
                 }
  deriving Show

data MirrorState = MirrorState {
                     ms_missingPkgs          :: Set PackageId,
                     ms_unmirrorablePkgs     :: Set PackageId
                   }
  deriving (Eq, Show)

main :: IO ()
main = toplevelHandler $ do
  rethrowSignalsAsExceptions [SIGABRT, SIGINT, SIGQUIT, SIGTERM]
  hSetBuffering stdout LineBuffering

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

    when (isHackageURI (srcURI opts)
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


---------------------------
-- Main mirroring logic
---------------------------

mirrorOneShot :: Verbosity -> MirrorOpts -> MirrorEnv -> MirrorState -> IO ()
mirrorOneShot verbosity opts env st = do

    (merr, _) <- mirrorOnce verbosity opts env st

    case merr of
      Nothing -> return ()
      Just theError -> fail (formatMirrorError theError)


mirrorIteration :: Verbosity -> MirrorOpts -> MirrorEnv
                -> MirrorState -> IO MirrorState
mirrorIteration verbosity opts env st = do

    (merr, st') <- mirrorOnce verbosity opts { mo_keepGoing = True } env st

    when (st' /= st) $
      savePackagesState env st'

    case merr of
      Nothing          -> return ()
      Just Interrupted -> throw UserInterrupt
      Just theError         -> do
        warn verbosity (formatMirrorError theError)
        notice verbosity "Abandoning this mirroring attempt."
    return st'


savePackagesState :: MirrorEnv  -> MirrorState -> IO ()
savePackagesState (MirrorEnv _ _ missingPkgsFile unmirrorablePkgsFile)
                  (MirrorState missingPkgs unmirrorablePkgs) = do
  writePkgProblemFile missingPkgsFile      missingPkgs
  writePkgProblemFile unmirrorablePkgsFile unmirrorablePkgs


mirrorOnce :: Verbosity -> MirrorOpts -> MirrorEnv
           -> MirrorState -> IO (Maybe MirrorError, MirrorState)
mirrorOnce verbosity opts
           (MirrorEnv srcCacheDir dstCacheDir missingPkgsFile unmirrorablePkgsFile)
           st@(MirrorState missingPkgs unmirrorablePkgs) =

    mirrorSession (mo_keepGoing opts) $ do

      srcIndex <- downloadIndex (srcIsOldHackage opts) (srcURI opts) srcCacheDir
      dstIndex <- downloadIndex (srcIsOldHackage opts) (dstURI opts) dstCacheDir

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
    mirrorSession :: Bool
                  -> MirrorSession a -> IO (Maybe MirrorError, MirrorState)
    mirrorSession keepGoing action =
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
    filter (\(PkgIndexInfo pkgid' _ _ _) -> anyMatchPackage pkgid')
  where
    anyMatchPackage :: PackageId -> Bool
    anyMatchPackage pkgid' =
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
      let srcPackage = if srcIsOldHackage opts
              then "packages" </> "archive"
                    </> display (packageName pkgid)
                    </> display (packageVersion pkgid)
              else "package" </> display pkgid
          srcBase = srcURI opts <//> srcPackage
          dstBase = dstURI opts
                    <//> "package"
                    </> display pkgid

          srcTgz  = srcBase <//> display pkgid <.> "tar.gz"
          srcCab  = srcBase <//> display (packageName pkgid) <.> "cabal"

          locTgz  = stateDir opts </> display pkgid <.> "tar.gz"
          locCab  = stateDir opts </> display pkgid <.> "cabal"

      liftIO $ notice verbosity $ "mirroring " ++ display pkgid
      rsp <- browserActions [
                 downloadFile' srcCab locCab
               , downloadFile' srcTgz locTgz
               ]
      case rsp of
        Just theError ->
          notifyResponse (GetPackageFailed theError pkgid)
        Nothing -> do
          notifyResponse GetPackageOk
          liftIO $ sanitiseTarball verbosity (stateDir opts) locTgz
          putPackage (mirrorUploaders opts) dstBase pkginfo locCab locTgz

-- Some package tarballs have extraneous stuff in them that causes
-- them to fail the "tarbomb" test in the server.  This cleans them
-- up before uploading.
sanitiseTarball :: Verbosity -> FilePath -> FilePath -> IO ()
sanitiseTarball verbosity tmpdir tgzpath = do
  tgz <- BS.readFile tgzpath
  let add _ (Left e) = Left e
      add entry (Right entries) = Right (entry:entries)
      eallentries = Tar.foldEntries add (Right []) (Left . show) $
                    Tar.read (GZip.decompressNamed tgzpath tgz)
  case eallentries of
    Left e -> warn verbosity e
    Right allentries -> do
      let okentries = filter dirOK allentries
          newtgz = GZ.compress $ Tar.write $ reverse okentries
      when (length allentries /= length okentries) $
        warn verbosity $ "sanitising tarball for " ++ tgzpath
      (tmpfp, tmph) <- openTempFile tmpdir "tmp.tgz"
      hClose tmph
      BS.writeFile tmpfp newtgz
      renameFile tmpfp tgzpath
  where
    basedir = dropExtension $ takeBaseName tgzpath
    dirOK entry = case splitDirectories (Tar.entryPath entry) of
      (d:_) -> d == basedir
      _     -> False

putPackage :: Bool -> URI -> PkgIndexInfo -> FilePath -> FilePath -> MirrorSession ()
putPackage doMirrorUploaders baseURI (PkgIndexInfo pkgid mtime muname _muid) locCab locTgz = do
    cab <- liftIO $ BS.readFile locCab
    tgz <- liftIO $ BS.readFile locTgz

    rsp <- browserActions $ [
        requestPUT cabURI "text/plain" cab
      , requestPUT tgzURI "application/x-gzip" tgz
      , maybe (return Nothing) putPackageUploadTime mtime
      ] ++
      (if doMirrorUploaders
         then [maybe (return Nothing) putPackageUploader muname]
         else []
      )

    case rsp of
      Just theError ->
        notifyResponse (PutPackageFailed theError pkgid)
      Nothing -> do
        notifyResponse PutPackageOk
        liftIO $ removeFile locCab
        liftIO $ removeFile locTgz

      -- TODO: think about in what situations we delete the file
      -- and if we should actually cache it if we don't sucessfully upload.

      -- TODO: perhaps we shouldn't report failure for the whole package if
      -- we fail to set the upload time/uploader
  where
    cabURI = baseURI <//> display (packageName pkgid) <.> "cabal"
    tgzURI = baseURI <//> display pkgid               <.> "tar.gz"

    putPackageUploadTime time = do
      let timeStr = formatTime defaultTimeLocale "%c" time
      requestPUT (baseURI <//> "upload-time") "text/plain" (packUTF8 timeStr)

    putPackageUploader uname = do
      let nameStr = display uname
      requestPUT (baseURI <//> "uploader") "text/plain" (packUTF8 nameStr)

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
  deriving (Functor, Applicative, Monad, MonadIO,
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

browserActions :: [BrowserAction (HandleStream ByteString) (Maybe ErrorResponse)] -> MirrorSession (Maybe ErrorResponse)
browserActions = foldr1 maybeThen . map browserAction
  where
    -- Bind for the strange not-quite-monad where errors are returned as
    -- (Just err) and success is returned as Nothing
    maybeThen :: Monad m => m (Maybe err) -> m (Maybe err) -> m (Maybe err)
    maybeThen p q = do
      res <- p
      case res of
        Just theError -> return (Just theError)
        Nothing       -> q

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
formatMirrorError (ParseEntityError entity uri theError) =
    "Error parsing " ++ formatEntity entity
 ++ " at " ++ show uri ++ ": " ++ theError
formatMirrorError (PutPackageError pkgid rsp) =
    "Failed to upload package "  ++ display pkgid
 ++ ",\n  " ++ formatErrorResponse rsp
formatMirrorError Interrupted = error "formatMirrorError: Interrupted"

formatEntity :: Entity -> String
formatEntity EntityIndex           = "the package index"
formatEntity EntityLog             = "the package upload log"
formatEntity (EntityPackage pkgid) = "package " ++ display pkgid

data ErrorState = ErrorState {
                    es_missing      :: Set PackageId,
                    es_unmirrorable :: Set PackageId
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
    st' <- handleEvent verbosity keepGoing st
    put st'
  where
    handleEvent _ False st = case e of
      GetIndexOk   -> return st
      GetPackageOk -> return st
      PutPackageOk -> return st
      GetPackageFailed rsp pkgid ->
        throwError (GetEntityError (EntityPackage pkgid) rsp)
      PutPackageFailed rsp pkgid ->
        throwError (PutPackageError pkgid rsp)

    handleEvent verbosity True st = case e of
      GetIndexOk   -> return st
      GetPackageOk -> return st
      PutPackageOk -> return st
      GetPackageFailed rsp@(ErrorResponse _ code _ _) pkgid
        | code == (4,0,4) -> do
            liftIO $ warn verbosity $
              formatMirrorError (GetEntityError (EntityPackage pkgid) rsp)
            return st {
              es_missing = Set.insert pkgid (es_missing st)
            }
        | otherwise -> throwError (GetEntityError (EntityPackage pkgid) rsp)

      PutPackageFailed rsp@(ErrorResponse _ code _ _) pkgid
        | code == (4,0,0) || code == (4,0,4) -> do
            liftIO $ warn verbosity $
              formatMirrorError (PutPackageError pkgid rsp)
            return st {
              es_unmirrorable = Set.insert pkgid (es_unmirrorable st)
            }
        | otherwise -> throwError (PutPackageError pkgid rsp)


----------------------------------------------------
-- Fetching info from source and destination servers
----------------------------------------------------

downloadIndex :: Bool -> URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadIndex isOldHackageURI
    | isOldHackageURI = downloadOldIndex
    | otherwise       = downloadNewIndex

isHackageURI :: URI -> Bool
isHackageURI uri
  | Just auth <- uriAuthority uri = uriRegName auth == "hackage.haskell.org"
  | otherwise                     = False

downloadOldIndex :: URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadOldIndex uri cacheDir = do

    rsp1 <- browserAction $ downloadFile indexURI indexFile
    case rsp1 of
      Nothing       -> notifyResponse GetIndexOk
      Just theError -> throwError (GetEntityError EntityIndex theError)

    rsp2 <- browserAction $ downloadFile logURI logFile
    case rsp2 of
      Nothing       -> notifyResponse GetIndexOk
      Just theError -> throwError (GetEntityError EntityLog theError)

    res1 <- liftIO $
      withFile indexFile ReadMode $ \hnd ->
        evaluate . PackageIndex.read (\pkgid _ -> pkgid)
                 . GZip.decompressNamed indexFile
               =<< BS.hGetContents hnd
    pkgids <- case res1 of
      Left  theError -> throwError (ParseEntityError EntityIndex uri theError)
      Right pkgs     -> return pkgs

    res2 <- liftIO $
      withFile logFile ReadMode $ \hnd ->
        evaluate . UploadLog.read =<< hGetContents hnd
    theLog <- case res2 of
      Left  theError -> throwError (ParseEntityError EntityLog uri theError)
      Right theLog   -> return theLog

    return (mergeLogInfo pkgids theLog)

  where
    indexURI  = uri <//> "packages" </> "archive" </> "00-index.tar.gz"
    indexFile = cacheDir </> "00-index.tar.gz"

    logURI    = uri <//> "packages" </> "archive" </> "log"
    logFile   = cacheDir </> "log"

    mergeLogInfo pkgids theLog =
        catMaybes
      . map selectDetails
      $ mergeBy (\pkgid entry -> compare pkgid (entryPkgId entry))
                (sort pkgids)
                ( map (maximumBy (comparing entryTime))
                . groupBy (equating  entryPkgId)
                . sortBy  (comparing entryPkgId)
                $ theLog )

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
      Just theError -> throwError (GetEntityError EntityIndex theError)
      Nothing -> do
        notifyResponse GetIndexOk
        res <- liftIO $ withFile indexFile ReadMode $ \hnd ->
                 evaluate . PackageIndex.read selectDetails
                          . GZip.decompressNamed indexFile
                        =<< BS.hGetContents hnd
        case res of
          Left  theError -> throwError (ParseEntityError EntityIndex uri theError)
          Right pkgs     -> return pkgs

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
    Left  theError -> return (Just theError)
    Right content  -> do liftIO $ BS.writeFile file content
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
                   -> Just (unpackUTF8 (rspBody rsp))
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
    flagCacheDir        :: Maybe FilePath,
    flagContinuous      :: Bool,
    flagInterval        :: Maybe String,
    flagKeepGoing       :: Bool,
    flagMirrorUploaders :: Bool,
    flagSrcIsOldHackage :: Bool,
    flagVerbosity       :: Verbosity,
    flagHelp            :: Bool
}

defaultMirrorFlags :: MirrorFlags
defaultMirrorFlags = MirrorFlags
  { flagCacheDir        = Nothing
  , flagContinuous      = False
  , flagInterval        = Nothing
  , flagKeepGoing       = False
  , flagMirrorUploaders = False
  , flagSrcIsOldHackage = False
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

  , Option [] ["keep-going"]
      (NoArg (\opts -> opts { flagKeepGoing = True }))
      "Don't fail on mirroring errors, keep going."

  , Option [] ["mirror-uploaders"]
      (NoArg (\opts -> opts { flagMirrorUploaders = True }))
      "Mirror the original uploaders which requires that they are already registered on the target hackage."

  , Option [] ["src-is-old-hackage"]
      (NoArg (\opts -> opts { flagSrcIsOldHackage = True }))
      "Enable this when the source is the old Hackage server so that the correct URLs are used."
  ]

validateOpts :: [String] -> IO (Verbosity, MirrorOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute mirrorFlagDescrs args
        flags = accum flags0 defaultMirrorFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (configFile:pkgstrs) -> do
        mFromTo <- readConfigFile configFile
        case mFromTo of
          Left theError -> die theError
          Right (fromURI, toURI) -> case (mpkgs, minterval) of
            (Left theError, _) -> die theError
            (_, Left theError) -> die theError
            (Right pkgs, Right interval) ->
               return $ (,) (flagVerbosity flags) MirrorOpts {
                   srcURI       = fromURI,
                   dstURI       = toURI,
                   stateDir     = fromMaybe "mirror-cache" (flagCacheDir flags),
                   selectedPkgs = pkgs,
                   continuous   = if flagContinuous flags
                                    then Just interval
                                    else Nothing,
                   mo_keepGoing = flagKeepGoing flags,
                   mirrorUploaders = flagMirrorUploaders flags,
                   srcIsOldHackage = flagSrcIsOldHackage flags
                 }
          where
            mpkgs     = validatePackageIds pkgstrs
            minterval = validateInterval (flagInterval flags)

      _ -> die $ "Expected path to a config file.\n"
              ++ "See hackage-mirror --help for details and an example."

  where
    printUsage = do
      putStrLn $ usageInfo usageHeader mirrorFlagDescrs ++ helpExampleStr
      exitSuccess
    usageHeader = helpDescrStr
               ++ "Usage: hackage-mirror configFile [packages] [options]\n\n"
               ++ "configFile should be a path to a file containing two lines:\n\n"
               ++ "    fromURL\n"
               ++ "    toURL\n\n"
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

readConfigFile :: FilePath -> IO (Either String (URI, URI))
readConfigFile configFile = runErrorT $ do
    config   <- ErrorT $ tryIO $ readFile configFile
    [fr, to] <- case lines config of
                  [fr, to] -> return [fr, to]
                  _        -> fail "Invalid config file format"
    frURI    <- ErrorT . return $ validateHackageURI fr
    toURI    <- ErrorT . return $ validateHackageURI to
    return (frURI, toURI)
  where
    tryIO :: IO a -> IO (Either String a)
    tryIO io = catch (Right `liftM` io) (\e -> return . Left $ show (e :: IOException))

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
  , "  where ./mirror.cfg contains"
  , "    http://hackage.haskell.org/"
  , "    http://foo:bar@localhost:8080/"
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
