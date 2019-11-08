module Main (main) where

-- stdlib
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Version
import Network.Browser
import System.Directory
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import System.FilePath
import System.IO
import System.IO.Error
import System.Process (callCommand)
import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Set               as Set

-- Cabal
import Distribution.Package
import Distribution.Simple.Utils hiding (warn)
import Distribution.Text
import Distribution.Verbosity
import Distribution.Version

-- hackage
import Distribution.Client (PkgIndexInfo(..))
import Distribution.Client.Cron (cron, rethrowSignalsAsExceptions, Signal(..))
import Distribution.Client.Mirror.CmdLine
import Distribution.Client.Mirror.Config
import Distribution.Client.Mirror.Repo
import Distribution.Client.Mirror.Session
import Distribution.Client.Mirror.State
import Distribution.Server.Util.Merge
import Paths_hackage_server (version)
import qualified Distribution.Server.Util.GZip as GZip

-- hackage-security
import qualified Hackage.Security.Client.Repository.HttpLib as Sec

{-------------------------------------------------------------------------------
  Application entry point
-------------------------------------------------------------------------------}

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

toplevelHandler :: IO a -> IO a
toplevelHandler =
    handle $ \ioe -> do
      hFlush stdout
      pname <- getProgName
      hPutStrLn stderr (pname ++ ": " ++ formatIOError ioe)
      exitWith (ExitFailure 1)

{-------------------------------------------------------------------------------
  One-shot versus continuous mirroring
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Main mirroring logic
-------------------------------------------------------------------------------}

mirrorOnce :: Verbosity -> MirrorOpts -> MirrorEnv
           -> MirrorState -> IO (Maybe MirrorError, MirrorState)
mirrorOnce verbosity opts
           (MirrorEnv srcCacheDir dstCacheDir missingPkgsFile unmirrorablePkgsFile)
           st@(MirrorState missingPkgs unmirrorablePkgs) =

    mirrorSession (mo_keepGoing opts) $ do
      httpLib <- mirrorAskHttpLib
      liftCont (initRepos httpLib) $ \(sourceRepo, targetRepo) -> do
          srcIndex <- downloadSourceIndex sourceRepo
          dstIndex <- readCachedTargetIndex verbosity targetRepo

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

          if mirrorCount == 0
            then liftIO $ notice verbosity $ "No packages to mirror"
            else do
              liftIO $ notice verbosity $
                  show mirrorCount ++ " packages to mirror."
               ++ if ignoreCount == 0 then ""
                    else " Ignoring " ++ show ignoreCount
                      ++ " package(s) that cannot be mirrored\n(for details see "
                      ++ missingPkgsFile ++ " and " ++ unmirrorablePkgsFile ++ ")"

              mirrorPackages verbosity opts sourceRepo targetRepo pkgsToMirror'
              finalizeMirror   sourceRepo targetRepo
              cacheTargetIndex sourceRepo targetRepo

              case mirrorPostHook (mirrorConfig opts) of
                Nothing       -> return ()
                Just postHook -> liftIO $ callCommand postHook
  where
    mirrorSession :: Bool
                  -> MirrorSession a -> IO (Maybe MirrorError, MirrorState)
    mirrorSession keepGoing action =
      liftM (\(eerr, st') -> (either Just (const Nothing) eerr,
                              fromErrorState st')) $
      runMirrorSession verbosity keepGoing (toErrorState st) $ do
        browserAction $ do
          setUserAgent  ("hackage-mirror/" ++ showVersion version)
          setErrHandler (warn  verbosity)
          setOutHandler (debug verbosity)
          setAllowBasicAuth True
          setCheckForProxy True
        action

    initRepos :: Sec.HttpLib -> ((SourceRepo, TargetRepo) -> IO a) -> IO a
    initRepos httpLib callback =
       withSourceRepo verbosity
                      httpLib
                      srcCacheDir
                      (mirrorSource (mirrorConfig opts))
                      $ \sourceRepo ->
         withTargetRepo dstCacheDir
                        (mirrorTarget (mirrorConfig opts) )
                        $ \targetRepo ->
           callback (sourceRepo, targetRepo)

mirrorPackages :: Verbosity
               -> MirrorOpts
               -> SourceRepo
               -> TargetRepo
               -> [PkgIndexInfo]
               -> MirrorSession ()
mirrorPackages verbosity opts sourceRepo targetRepo pkgsToMirror = do
    authenticate targetRepo
    mapM_ (mirrorPackage verbosity opts sourceRepo targetRepo) pkgsToMirror

mirrorPackage :: Verbosity
              -> MirrorOpts
              -> SourceRepo
              -> TargetRepo
              -> PkgIndexInfo
              -> MirrorSession ()
mirrorPackage verbosity opts sourceRepo targetRepo pkginfo = do
     liftIO $ notice verbosity $ "mirroring " ++ display pkgid
     go `mirrorFinally` removeTempFiles
  where
    go :: MirrorSession ()
    go = do
      rsp <- downloadPackage sourceRepo pkgid locCab locTgz
      case rsp of
        Just theError ->
          notifyResponse (GetPackageFailed theError pkgid)
        Nothing -> do
          notifyResponse GetPackageOk
          liftIO $ sanitiseTarball verbosity (stateDir opts) locTgz
          uploadPackage targetRepo (mirrorUploaders opts) pkginfo locCab locTgz

    removeTempFiles :: MirrorSession ()
    removeTempFiles = liftIO $ handle ignoreDoesNotExist $ do
      removeFile locTgz
      removeFile locCab

    ignoreDoesNotExist :: IOException -> IO ()
    ignoreDoesNotExist ex = if isDoesNotExistError ex then return ()
                                                      else throwIO ex

    PkgIndexInfo pkgid _ _ _ = pkginfo
    locTgz = stateDir opts </> display pkgid <.> "tar.gz"
    locCab = stateDir opts </> display pkgid <.> "cabal"

{-------------------------------------------------------------------------------
  Operations on the the index
-------------------------------------------------------------------------------}

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
    matchPackage (PackageIdentifier name  v)
                 (PackageIdentifier name' _)
                 | nullVersion == v = name == name'
    matchPackage pkgid pkgid' = pkgid == pkgid'

{-------------------------------------------------------------------------------
  Auxiliary: dealing with tarballs
-------------------------------------------------------------------------------}

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
      (tmpfp, tmph) <- openBinaryTempFileWithDefaultPermissions tmpdir "tmp.tgz"
      hClose tmph
      BS.writeFile tmpfp newtgz
      renameFile tmpfp tgzpath
  where
    basedir = dropExtension $ takeBaseName tgzpath
    dirOK entry = case splitDirectories (Tar.entryPath entry) of
      (d:_) -> d == basedir
      _     -> False
