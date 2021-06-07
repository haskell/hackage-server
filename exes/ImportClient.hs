{-# LANGUAGE PatternGuards, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Network.HTTP
import Network.Browser hiding (err)
import Network.URI
         ( URI(..), URIAuth(..), parseURI, escapeURIString, isUnescapedInURI )

import qualified Codec.Archive.Tar.Entry as Tar (Entry(..), EntryContent(..))

import qualified Distribution.Client.HtPasswdDb as HtPasswdDb
import qualified Distribution.Client.UserAddressesDb as UserAddressesDb
import qualified Distribution.Client.UploadLog  as UploadLog
import qualified Distribution.Client.TagsFile   as TagsFile
import qualified Distribution.Client.DistroMap  as DistroMap
import qualified Distribution.Client.Index as PackageIndex (read)


import Distribution.Server.Users.Types (UserName(..))
import Distribution.Server.Util.Parse (packUTF8, unpackUTF8)

import Distribution.Package
         ( PackageId, PackageName, packageName, packageVersion, PackageIdentifier )
import Distribution.Version
         ( Version, versionTags )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( topHandler, die, {-warn, debug,-} wrapText )
import Distribution.Verbosity
         ( Verbosity, normal )

import Distribution.Simple.Command
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, flagToList )

import Distribution.Client.ParseApacheLogs
import qualified Distribution.Server.Util.GZip as GZip

import System.Environment
         ( getArgs, getProgName )
import Data.Time.Locale.Compat
         ( defaultTimeLocale )
import System.Exit
         ( exitWith, ExitCode(..) )
import System.IO
import System.FilePath
         ( (</>), (<.>), takeFileName, dropExtension, takeExtension )
import qualified System.FilePath.Posix as Posix
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Time (UTCTime, formatTime, getCurrentTime)
import Control.Monad
import Control.Monad.Trans
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (Value(..), toJSON, encode)
import Data.Text (Text)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import qualified Data.ByteString.Lazy as LBS

import qualified Text.CSV as CSV

import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent.Async

import qualified Paths_hackage_server as Paths (version)

--
-- The following data can be imported:
--
--  * User accounts
--    - user names
--    - htpasswd-style passwords
--    - email addresses
--
--  * Package metadata
--    - cabal files
--    - upload user
--    - upload times
--    - maintainer group
--
--  * Package tarballs
--
--  * Package deprecations
--
--  * Distribution information
--
--  * Documentation tarballs
--

-------------------------------------------------------------------------------
-- Top level command handling
--

main :: IO ()
main = topHandler $ do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case commandsRun globalCommand commands args of
      CommandHelp   help  -> printHelp help
      CommandList   opts  -> printOptionsList opts
      CommandErrors errs  -> printErrors errs
      CommandReadyToGo (globalflags, commandParse) ->
        case commandParse of
          _ | fromFlag (globalVersion globalflags) -> printVersion
          CommandHelp      help    -> printHelp help
          CommandList      opts    -> printOptionsList opts
          CommandErrors    errs    -> printErrors errs
          CommandReadyToGo action  -> action globalflags

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitWith (ExitFailure 1)
    printVersion = putStrLn $ "hackage-import " ++ display Paths.version

    commands =
      [ usersCommand         `commandAddAction` usersAction
      , metadataCommand      `commandAddAction` metadataAction
      , tarballCommand       `commandAddAction` tarballAction
      , distroCommand        `commandAddAction` distroAction
      , deprecationCommand   `commandAddAction` deprecationAction
      , docsCommand          `commandAddAction` docsAction
      , downloadCountCommand `commandAddAction` downloadCountAction
      ]


-------------------------------------------------------------------------------
-- Global command
--

data GlobalFlags = GlobalFlags {
    globalVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    globalVersion = Flag False
  }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName         = "",
    commandSynopsis     = "",
    commandUsage        = \_ ->
         "Data import client for the hackage server:\n"
      ++ "utility for importing old data into a new hackage-server\n",
    commandDescription  = Just $ \pname ->
         "For more information about a command use\n"
      ++ "  " ++ pname ++ " COMMAND --help\n\n"
      ++ "Main steps for populating a new empty server instance:\n"
      ++ concat [ "  " ++ x ++ "\n"
                | x <- ["users", "metadata", "tarballs"]],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions      = \_ ->
      [option ['V'] ["version"]
         "Print version information"
         globalVersion (\v flags -> flags { globalVersion = v })
         (noArg (Flag True))
      ]
  }


-------------------------------------------------------------------------------
-- Users command
--

data UsersFlags = UsersFlags {
    usersHtPasswd  :: Flag FilePath,
    usersUploaders :: Flag Bool,
    usersAddresses :: Flag FilePath,
    usersJobs      :: Flag String
  }

defaultUsersFlags :: UsersFlags
defaultUsersFlags = UsersFlags {
    usersHtPasswd  = NoFlag,
    usersUploaders = Flag False,
    usersAddresses = NoFlag,
    usersJobs      = NoFlag
  }

usersCommand :: CommandUI UsersFlags
usersCommand =
    (makeCommand name shortDesc longDesc defaultUsersFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS] [URI]\n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "users"
    shortDesc  = "Import user accounts and other user data"
    longDesc   = Just $ \_ ->
                  "User account names and passwords can be imported from a "
               ++ "file in the\napache 'htpasswd' format.\n"
    options _  =
      [ option [] ["htpasswd"]
          "Import an apache 'htpasswd' user account database file"
          usersHtPasswd (\v flags -> flags { usersHtPasswd = v })
          (reqArgFlag "HTPASSWD")
      , option [] ["all-uploaders"]
          "Add all new accounts to the uploaders group (use with --htpasswd)"
          usersUploaders (\v flags -> flags { usersUploaders = v })
          (noArg (Flag True))
      , option [] ["addresses"]
          "Import user email addresses"
          usersAddresses (\v flags -> flags { usersAddresses = v })
          (reqArgFlag "ADDRESSES")
      , option [] ["jobs"]
          "The level of concurrency to use when uploading"
          usersJobs (\v flags -> flags { usersJobs = v })
          (reqArgFlag "N")
      ]

usersAction :: UsersFlags -> [String] -> GlobalFlags -> IO ()
usersAction flags args _ = do
    jobs    <- validateOptsJobs (usersJobs flags)
    baseURI <- validateOptsServerURI args

    when (usersHtPasswd flags == NoFlag
       && usersAddresses flags == NoFlag) $
      die $ "specify what to import using one or more of the flags:\n"
         ++ "  --htpasswd=  --addresses="

    let makeUploader = fromFlag (usersUploaders flags)

    case usersHtPasswd flags of
      NoFlag    -> return ()
      Flag file -> importAccounts jobs file makeUploader baseURI

    case usersAddresses flags of
      NoFlag    -> return ()
      Flag file -> importAddresses jobs file baseURI

importAccounts :: Int -> FilePath -> Bool -> URI -> IO ()
importAccounts jobs htpasswdFile makeUploader baseURI = do

  htpasswdDb <- either die return . HtPasswdDb.parse
            =<< readFile htpasswdFile

  concForM_ jobs htpasswdDb $ \tasks ->
    httpSession "hackage-import" Paths.version $ do
      setAuthorityFromURI baseURI
      tasks $ \(username, mPasswdhash) ->
        putUserAccount baseURI username mPasswdhash makeUploader


putUserAccount :: URI -> UserName
               -> Maybe HtPasswdDb.HtPasswdHash -> Bool
               -> HttpSession ()
putUserAccount baseURI username mPasswdHash makeUploader = do

    do rsp <- requestPUT userURI "" LBS.empty
       case rsp of
        Nothing  -> return ()
        Just err -> fail (formatErrorResponse err)

    case mPasswdHash of
      Nothing -> return ()
      Just (HtPasswdDb.HtPasswdHash passwdHash) -> do
        rsp <- requestPUT passwdURI "text/plain" (packUTF8 passwdHash)
        case rsp of
          Nothing  -> return ()
          Just err -> fail (formatErrorResponse err)

    when makeUploader $ do
      rsp <- requestPUT userUploaderURI "" LBS.empty
      case rsp of
        Nothing  -> return ()
        Just err -> fail (formatErrorResponse err)

  where
    userURI   = baseURI <//> "user" </> display username
    passwdURI = userURI <//> "htpasswd"
    userUploaderURI = baseURI <//> "packages/uploaders/user" </> display username

importAddresses :: Int -> FilePath -> URI -> IO ()
importAddresses jobs addressesFile baseURI = do

    addressesDb <- either die return =<< UserAddressesDb.parseFile addressesFile

    concForM_ jobs addressesDb $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \(username, realname, email, timestamp, adminname) ->
          putUserDetails baseURI username realname email timestamp adminname

putUserDetails :: URI -> UserName -> Text -> Text
               -> UTCTime -> UserName -> HttpSession ()
putUserDetails baseURI username realname email timestamp adminname = do

    do rsp <- requestPUT userNameAddressURI "application/json" (encode nameAddressInfo)
       case rsp of
         Nothing  -> return ()
         Just err | isErrNotFound err
                  -> liftIO $ info $ "Ignoring address info for user "
                                  ++ display username
         Just err -> fail (formatErrorResponse err)

    do now <- liftIO getCurrentTime
       rsp <- requestPUT userAdminInfoURI "application/json" (encode (adminInfo now))
       case rsp of
         Nothing  -> return ()
         Just err | isErrNotFound err
                  -> liftIO $ info $ "Ignoring address info for user "
                                  ++ display username
         Just err -> fail (formatErrorResponse err)

  where
    userURI            = baseURI <//> "user" </> display username
    userNameAddressURI = userURI <//> "name-contact.json"
    userAdminInfoURI   = userURI <//> "admin-info.json"

    nameAddressInfo =
      object
          [ ("name",                String realname)
          , ("contactEmailAddress", String email)
          ]
    adminInfo now =
      object
          [ ("accountKind", object [("AccountKindRealUser", array [])])
          , ("notes",       toJSON (notes now))
          ]
    notes now = "Original hackage account created by "
             ++ display adminname ++ " on " ++ showUTCTime timestamp ++ "\n"
             ++ "Account created on this server by the hackage-import client on "
             ++ showUTCTime now


-------------------------------------------------------------------------------
-- Metadata command
--

data MetadataFlags = MetadataFlags {
    metadataIndex     :: Flag FilePath,
    metadataUploadLog :: Flag FilePath,
    metadataJobs      :: Flag String
  }

defaultMetadataFlags :: MetadataFlags
defaultMetadataFlags = MetadataFlags {
    metadataIndex     = NoFlag,
    metadataUploadLog = NoFlag,
    metadataJobs      = NoFlag
  }

metadataCommand :: CommandUI MetadataFlags
metadataCommand =
    (makeCommand name shortDesc longDesc defaultMetadataFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS] [URI]\n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "metadata"
    shortDesc  = "Import package metadata: cabal files and upload log"
    longDesc   = Just $ \_ ->
                  "The cabal files can be imported from hackage index file,"
               ++ "while upload details\ncan be got from the hackage log.\n"
    options _  =
      [ option [] ["index"]
          "Import all the packages from a hackage '00-index.tar' file"
          metadataIndex (\v flags -> flags { metadataIndex = v })
          (reqArgFlag "INDEX")
      , option [] ["upload-log"]
          "Import who uploaded what when, and set maintainer groups"
          metadataUploadLog (\v flags -> flags { metadataUploadLog = v })
          (reqArgFlag "LOG")
      , option [] ["jobs"]
          "The level of concurrency to use when uploading"
          metadataJobs (\v flags -> flags { metadataJobs = v })
          (reqArgFlag "N")
      ]

metadataAction :: MetadataFlags -> [String] -> GlobalFlags -> IO ()
metadataAction flags args _ = do

    jobs    <- validateOptsJobs (metadataJobs flags)
    baseURI <- validateOptsServerURI args

    when (metadataIndex flags == NoFlag
       && metadataUploadLog flags == NoFlag) $
      die $ "specify what to import using one or more of the flags:\n"
         ++ "  --index=  --upload-log="

    case metadataIndex flags of
      NoFlag    -> return ()
      Flag file -> importIndex jobs file baseURI

    case metadataUploadLog flags of
      NoFlag    -> return ()
      Flag file -> importUploadLog jobs file baseURI

importIndex :: Int -> FilePath -> URI -> IO ()
importIndex jobs indexFile baseURI = do
    info $ "Reading index file " ++ indexFile
    pkgs  <- either fail return
           . readPkgIndex
         =<< LBS.readFile indexFile

    pkgs' <- evaluate (sortBy (comparing fst) pkgs)
    info $ "Uploading..."

    concForM_ jobs pkgs' $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \ (pkgid, cabalFile) ->
          putCabalFile baseURI pkgid cabalFile


putCabalFile :: URI -> PackageId -> ByteString -> HttpSession ()
putCabalFile baseURI pkgid cabalFile = do

    rsp <- requestPUT pkgURI "text/plain" cabalFile
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgid
                      </> display (packageName pkgid) <.> "cabal"


importUploadLog :: Int -> FilePath -> URI -> IO ()
importUploadLog jobs uploadLogFile baseURI = do
    info $ "Reading log file " ++ uploadLogFile
    uploadLog <- either fail return
               . UploadLog.read
             =<< readFile uploadLogFile

    let uploadInfo     = UploadLog.collectUploadInfo uploadLog
        maintainerInfo = UploadLog.collectMaintainerInfo uploadLog

    concForM_ jobs uploadInfo $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \(pkgid, time, uname) ->
          putUploadInfo baseURI pkgid time uname

    concForM_ jobs maintainerInfo $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \(pkgname, maintainers) ->
          putMaintainersInfo baseURI pkgname maintainers


putUploadInfo :: URI -> PackageId -> UTCTime -> UserName -> HttpSession ()
putUploadInfo baseURI pkgid time uname = do

    liftIO $ info $ "setting upload info for " ++ display pkgid

    do let timeStr = showUTCTime time
       rsp <- requestPUT (pkgURI <//> "upload-time") "text/plain" (packUTF8 timeStr)
       case rsp of
         Nothing  -> return ()
         Just err | isErrNotFound err -> liftIO $ info $ "Ignoring upload log entry for package " ++ display pkgid
         Just err -> fail (formatErrorResponse err)

    do let nameStr = display uname
       rsp <- requestPUT (pkgURI <//> "uploader") "text/plain" (packUTF8 nameStr)
       case rsp of
         Nothing  -> return ()
         Just err | isErrNotFound err  -> liftIO $ info $ "Ignoring upload log entry for package " ++ display pkgid
         Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgid

putMaintainersInfo :: URI -> PackageName -> [UserName] -> HttpSession ()
putMaintainersInfo baseURI pkgname maintainers =

    -- Add to the package's maintainers group
    forM_ maintainers $ \uname -> do
      rsp <- requestPUT (pkgURI <//> "maintainers" </> "user" </> display uname) "" LBS.empty
      case rsp of
        Nothing  -> return ()
        Just err | isErrNotFound err -> liftIO $ info $ "Cannot make " ++ display uname ++ " a maintainer for package " ++ display pkgname
        Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgname


-------------------------------------------------------------------------------
-- Tarballs command
--

data TarballFlags = TarballFlags {
    tarballJobs :: Flag String
  }

defaultTarballFlags :: TarballFlags
defaultTarballFlags = TarballFlags {
    tarballJobs = NoFlag
  }

tarballCommand :: CommandUI TarballFlags
tarballCommand =
    (makeCommand name shortDesc longDesc defaultTarballFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [URI] [TARBALL]... \n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "tarball"
    shortDesc  = "Import package tarballs"
    longDesc   = Just $ \_ ->
                     "The package tarballs can be imported directly from\n"
                  ++ " local .tar.gz files.\n"
    options _  = [ option [] ["jobs"]
                   "The level of concurrency to use when uploading tarballs"
                   tarballJobs (\v flags -> flags { tarballJobs = v })
                   (reqArgFlag "N")
                 ]

tarballAction :: TarballFlags -> [String] -> GlobalFlags -> IO ()
tarballAction flags args _ = do

    jobs <- validateOptsJobs (tarballJobs flags)

    (baseURI, tarballFiles) <- validateOptsServerURI' args

    let pkgidAndTarball =
          [ (mpkgid, file)
          | file <- tarballFiles
          , let pkgidstr = (dropExtension . dropExtension . takeFileName) file
                ext      = takeExtension (dropExtension file)
                             <.> takeExtension file
                mpkgid | ext == ".tar.gz"
                       , Just pkgid <- simpleParse pkgidstr
                       , null (versionTags (packageVersion pkgid))
                       = Just pkgid
                       | otherwise
                       = Nothing
          ]

    case [ file | (Nothing, file) <- pkgidAndTarball] of
      []    -> return ()
      files -> warn normal $ "the following files will be ignored because they "
                          ++ "do not match the expected naming convention of "
                          ++ "foo-1.0.tar.gz:\n" ++ unlines files

    let pkgidAndTarball' = [ (pkgid, tarballFilePath)
                           | (Just pkgid, tarballFilePath) <- pkgidAndTarball ]

    concForM_ jobs pkgidAndTarball' $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \(pkgid, tarballFilePath) ->
          putPackageTarball baseURI pkgid tarballFilePath


putPackageTarball :: URI -> PackageId -> FilePath -> HttpSession ()
putPackageTarball baseURI pkgid tarballFilePath = do

    pkgContent <- liftIO $ LBS.readFile tarballFilePath
    rsp <- requestPUT pkgURI "application/x-gzip" pkgContent
    case rsp of
      Nothing  -> return ()
      Just err -> out (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgid </> display pkgid <.> "tar.gz"


-------------------------------------------------------------------------------
-- Deprecations command
--

data DeprecationFlags = DeprecationFlags

defaultDeprecationFlags :: DeprecationFlags
defaultDeprecationFlags = DeprecationFlags

deprecationCommand :: CommandUI DeprecationFlags
deprecationCommand =
    (makeCommand name shortDesc longDesc defaultDeprecationFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [URI] [TAGS]... \n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "deprecation"
    shortDesc  = "Import package deprecation info from old hackage files"
    longDesc   = Just $ \_ ->
                     "The old hackage server has files like alsa/0.4/tags\n"
                  ++ "and some of these tags files contain info about the \n"
                  ++ "package being deprecated, and sometimes what it is\n"
                  ++ "superceded by. The files must follow this file name\n"
                  ++ "convention so we know which package the tags apply to.\n"
    options _  = []

deprecationAction :: DeprecationFlags -> [String] -> GlobalFlags -> IO ()
deprecationAction _flags args _ = do

    (baseURI, tagFiles) <- validateOptsServerURI' args

    entries <- forM tagFiles $ \tagFile -> do
      content <- readFile tagFile
      either die return (TagsFile.read tagFile content)

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putDeprecatedInfo baseURI pkgname replacement
        | (pkgname, replacement) <- TagsFile.collectDeprecated entries ]


putDeprecatedInfo :: URI -> PackageName -> Maybe PackageName -> HttpSession ()
putDeprecatedInfo baseURI pkgname replacement = do

    rsp <- requestPUT (pkgURI <//> "deprecated.json") "application/json" (encode deprecatedInfo)
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgname

    deprecatedInfo =
      object
          [ ("is-deprecated", Bool True)
          , ("in-favour-of", array [ string $ display pkg
                                   | pkg <- maybeToList replacement ])
          ]


-------------------------------------------------------------------------------
-- Distro command
--

data DistroFlags = DistroFlags

defaultDistroFlags :: DistroFlags
defaultDistroFlags = DistroFlags

distroCommand :: CommandUI DistroFlags
distroCommand =
    (makeCommand name shortDesc longDesc defaultDistroFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [URI] [TAGS]... \n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "distro"
    shortDesc  = "Import distro info from old hackage files"
    longDesc   = Just $ \_ ->
                     "The old hackage server has files like archive/00-distromap/Debian\n"
                  ++ "which contain info about what versions of the packages \n"
                  ++ "are available in the respective distributions.\n"
                  ++ "The file name must be the distro name."
    options _  = []

distroAction :: DistroFlags -> [String] -> GlobalFlags -> IO ()
distroAction _flags args _ = do

    (baseURI, distroFiles) <- validateOptsServerURI' args

    distros <- forM distroFiles $ \distroFile -> do
      content <- readFile distroFile
      let (errs, entries) = DistroMap.read content
      mapM_ info errs
      return (takeFileName distroFile, entries)

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putDistroInfo baseURI distroname entries
        | (distroname, entries) <- distros, not (null entries) ]


putDistroInfo :: URI -> String -> [DistroMap.Entry] -> HttpSession ()
putDistroInfo baseURI distroname entries = do

    do rsp <- requestPUT distroURI "" LBS.empty
       case rsp of
         Nothing  -> return ()
         Just err -> fail (formatErrorResponse err)

    do rsp <- requestPUT (distroURI <//> "packages.csv") "text/csv" (toBS entries)
       case rsp of
         Nothing  -> return ()
         Just err -> fail (formatErrorResponse err)

  where
    distroURI = baseURI <//> "distro" </> distroname
    toBS      = packUTF8 . CSV.printCSV . DistroMap.toCSV

-------------------------------------------------------------------------------
-- Documentation tarballs command
--

data DocsFlags = DocsFlags {
    docsJobs :: Flag String
  }

defaultDocsFlags :: DocsFlags
defaultDocsFlags = DocsFlags {
    docsJobs = NoFlag
  }

docsCommand :: CommandUI DocsFlags
docsCommand =
    (makeCommand name shortDesc longDesc defaultDocsFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " [URI] [TARBALL]... \n\n"
        ++ "Flags for " ++ name ++ ":"
    }
  where
    name       = "docs"
    shortDesc  = "Import package documentation tarballs"
    longDesc   = Just $ \_ ->
                     "The package documentation can be imported directly from\n"
                  ++ " local .tar or .tar.gz files of the html bundle."
    options _  = [ option [] ["jobs"]
                   "The level of concurrency to use when uploading documentation"
                   docsJobs (\v flags -> flags { docsJobs = v })
                   (reqArgFlag "N")
                 ]

docsAction :: DocsFlags -> [String] -> GlobalFlags -> IO ()
docsAction flags args _ = do

    jobs <- validateOptsJobs (docsJobs flags)

    (baseURI, docTarballFiles) <- validateOptsServerURI' args

    let pkgidAndTarball =
          [ (mpkgid, file)
          | file <- docTarballFiles
          , let pkgidstr = (dropExtension . dropExtension. takeFileName) file
                ext      = takeExtension (dropExtension file)
                             <.> takeExtension file
                mpkgid | ext == ".tar" || ext == ".tar.gz"
                       , Just pkgid <- simpleParse pkgidstr
                       , versionTags (packageVersion pkgid) == ["docs"]
                       = Just pkgid
                       | otherwise
                       = Nothing
          ]

    case [ file | (Nothing, file) <- pkgidAndTarball ] of
      []    -> return ()
      files -> warn normal $ "the following files don't match the expected naming "
                          ++ "convention of foo-1.0-docs.tar[.gz]:\n" ++ unlines files

    let pkgidAndTarball' = [ (pkgid, tarballFilePath)
                           | (Just pkgid, tarballFilePath) <- pkgidAndTarball ]

    concForM_ jobs pkgidAndTarball' $ \tasks ->
      httpSession $ do
        setAuthorityFromURI baseURI
        tasks $ \(pkgid, tarballFilePath) ->
          putPackageDocsTarball baseURI pkgid tarballFilePath

putPackageDocsTarball :: URI -> PackageId -> FilePath -> HttpSession ()
putPackageDocsTarball baseURI pkgid tarballFilePath = do

    tarballContent <- liftIO $ LBS.readFile tarballFilePath
    let extraHeaders
          | takeExtension tarballFilePath == ".gz"
                      = [Header HdrContentEncoding "gzip"]
          | otherwise = []

    rsp <- requestPUTWithHeaders pkgDocURI "application/x-tar" extraHeaders
                                 tarballContent
    case rsp of
      Nothing  -> return ()
      Just err | isErrNotFound err
               -> liftIO $ info $ "Ignoring documentation for package "
                               ++ display pkgid
      Just err -> fail (formatErrorResponse err)

  where
    pkgDocURI = baseURI <//> "package" </> display pkgid </> "docs.tar"

-------------------------------------------------------------------------------
-- Download count command
--

data DownloadCountFlags = DownloadCountFlags

defaultDownloadCountFlags :: DownloadCountFlags
defaultDownloadCountFlags = DownloadCountFlags

downloadCountCommand :: CommandUI DownloadCountFlags
downloadCountCommand =
    (makeCommand name shortDesc longDesc defaultDownloadCountFlags options) {
      commandUsage = \pname ->
           "Usage: " ++ pname ++ " " ++ name ++ " URI [LOG.GZ]... \n\n"
        ++ "Flags for " ++ name ++ ":"
      }
  where
    name = "downloads"
    shortDesc = "Import download counts"
    longDesc  = Just $ \_ -> unlines $ [
        "Replace the on-disk download statistics with the download statistics"
      , "extracted from Apache log files (in .gz format)"
      ]
    options _ = []

downloadCountAction :: DownloadCountFlags -> [String] -> GlobalFlags -> IO ()
downloadCountAction _ args _ = do
    (baseURI, logFiles) <- validateOptsServerURI' args
    csv <- logToDownloadCounts <$> readLogs logFiles

    -- Compute before we open the connection to the server
    evaluate $ LBS.length csv

    httpSession $ do
      setAuthorityFromURI baseURI
      void $ requestPUT (baseURI <//> "packages" </> "downloads.csv") "text/csv" csv
  where
    readLogs :: [FilePath] -> IO LBS.ByteString
    readLogs paths = LBS.concat <$> mapM decompress paths

    decompress :: FilePath -> IO LBS.ByteString
    decompress path = GZip.decompressNamed path <$> LBS.readFile path

-------------------------
-- HTTP utilities
-------------------------

infixr 5 <//>

(<//>) :: URI -> FilePath -> URI
uri <//> path = uri { uriPath = Posix.addTrailingPathSeparator (uriPath uri)
                                Posix.</> escapeURIString isUnescapedInURI path }

validateHttpURI :: String -> Either String URI
validateHttpURI str = case parseURI str of
  Nothing                          -> Left ("invalid URL " ++ str)
  Just uri
    | uriScheme uri /= "http:"     -> Left ("only http URLs are supported " ++ str)
    | isNothing (uriAuthority uri) -> Left ("server name required in URL " ++ str)
    | otherwise                    -> Right uri


type HttpSession a = BrowserAction (HandleStream ByteString) a

httpSession :: HttpSession a -> IO a
httpSession action =
    browse $ do
      setUserAgent  ("hackage-import/" ++ display Paths.version)
      setErrHandler (warn  verbosity)
      setOutHandler (debug verbosity)
      setAllowBasicAuth True
      setCheckForProxy True
      action
  where
    verbosity = normal

data ErrorResponse = ErrorResponse URI ResponseCode String (Maybe String)
  deriving Show

isErrNotFound :: ErrorResponse -> Bool
isErrNotFound (ErrorResponse _ (4,0,4) _ _) = True
isErrNotFound _                             = False

-- | We can do http digest auth, however currently the Network.Browser module
-- does not cache the auth info so it has to re-auth for every single request.
-- So instead here we just make it pre-emptively supply basic auth info.
-- We assume this is fine since we're probably working with localhost anyway.
--
setAuthorityFromURI :: URI -> HttpSession ()
setAuthorityFromURI uri
    | Just (username, passwd) <- extractCredentials uri
    = addAuthority AuthBasic {
        auRealm    = "Hackage",
        auUsername = username,
        auPassword = passwd,
        auSite     = uri
      }
    | otherwise = return ()

extractCredentials :: URI -> Maybe (String, String)
extractCredentials uri
  | Just authority <- uriAuthority uri
  , (username, ':':passwd0) <- break (==':') (uriUserInfo authority)
  , let passwd = takeWhile (/='@') passwd0
  , not (null username)
  , not (null passwd)
  = Just (username, passwd)
extractCredentials _ = Nothing


requestGET :: URI -> HttpSession (Either ErrorResponse ByteString)
requestGET uri = do
    (_, rsp) <- request (Request uri GET headers LBS.empty)
    case rspCode rsp of
      (2,0,0) -> return (Right (rspBody rsp))
      _       -> return (Left  (mkErrorResponse uri rsp))
  where
    headers = []

requestPUT :: URI -> String -> ByteString -> HttpSession (Maybe ErrorResponse)
requestPUT uri mimetype body =
    let extraHeaders = [] in
    requestPUTWithHeaders uri mimetype extraHeaders body

requestPUTWithHeaders :: URI -> String -> [Header] -> ByteString
                      -> HttpSession (Maybe ErrorResponse)
requestPUTWithHeaders uri mimetype extraHeaders body = do
    (_, rsp) <- request (Request uri PUT headers body)
    case rspCode rsp of
      (2,_,_) -> return Nothing
      _       -> return (Just (mkErrorResponse uri rsp))
  where
    headers =   Header HdrContentLength (show (LBS.length body))
              : Header HdrContentType mimetype
              : extraHeaders

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


-------------------------------------------------------------------------------
-- Utils
--

showUTCTime :: UTCTime -> String
showUTCTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- option utility
reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description
           -> (a -> Flag String) -> (Flag String -> a -> a)
           -> OptDescr a
reqArgFlag ad = reqArg' ad Flag flagToList

warn :: Verbosity -> String -> IO ()
warn verbosity msg =
  when (verbosity >= normal) $
    putStrLn ("Warning: " ++ msg)

debug :: Verbosity -> String -> IO ()
debug verbosity msg =
  when (verbosity > normal) $
    putStrLn ("Debug: " ++ msg)

info :: String -> IO ()
info msg = do
  pname <- getProgName
  putStrLn (pname ++ ": " ++ msg)
  hFlush stdout

validateOptsServerURI :: [String] -> IO URI
validateOptsServerURI [server] = either die return $ validateHttpURI server
validateOptsServerURI _        = die $ "The command expects the target server "
                            ++ "URI e.g. http://admin:admin@localhost:8080/"

validateOptsServerURI' :: [String] -> IO (URI, [String])
validateOptsServerURI' (server:opts) = do uri <- either die return $ validateHttpURI server
                                          return (uri,opts)
validateOptsServerURI' _             = die $ "The command expects the target server "
                                          ++ "URI e.g. http://admin:admin@localhost:8080/"

validateOptsJobs :: Flag String -> IO Int
validateOptsJobs  NoFlag       = return 1
validateOptsJobs (Flag s)
  | [(n,"")] <- reads s
 , n >= 1 && n <= 16           = return n
validateOptsJobs (Flag s)
  | [(_ :: Int,"")] <- reads s = die "not a sensible number for --jobs"
  | otherwise                  = die "expected a number for --jobs"


-------------------------------------------------------------------------------
-- Concurrency Utils
--

concForM_ :: MonadIO m => Int -> [a] -> (((a -> m ()) -> m ()) -> IO ()) -> IO ()
concForM_ n = flip (concMapM_ n)

concMapM_ :: forall m a. MonadIO m => Int -> (((a -> m ()) -> m ()) -> IO ()) -> [a] -> IO ()
concMapM_ n action xs = do
    chan <- newChan
    writeList2Chan chan (map Just xs ++ replicate n Nothing)
    bracket
      (replicateM n (async (worker chan)))
      (mapM_ cancel)
      (\as -> waitAny as >> mapM_ wait as)
  where
    worker :: Chan (Maybe a) -> IO ()
    worker chan = action mapMTasks
      where
        mapMTasks :: MonadIO m => (a -> m ()) -> m ()
        mapMTasks process = go
          where
            go = do mx <- liftIO $ readChan chan
                    case mx of
                      Nothing -> return ()
                      Just x  -> process x >> go

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

array :: [Value] -> Value
array = Array . Vector.fromList

object :: [(Text.Text, Value)] -> Value
object = Object . HashMap.fromList

string :: String -> Value
string = String . Text.pack

-- originally provided by "Distribution.Client.PkgIndex" which got removed
-- in 4b2770aa9597ece12ec5e35b742161a30120276b

readPkgIndex :: ByteString -> Either String [(PackageIdentifier, ByteString)]
readPkgIndex = fmap extractCabalFiles
             . PackageIndex.read (,)
             . GZip.decompressNamed "<<package index>>"
  where
    extractCabalFiles entries =
      [ (pkgid, cabalFile)
      | (pkgid, Tar.Entry {
                          Tar.entryContent = Tar.NormalFile cabalFile _
                }) <- entries ]


-- legacy/compat function
-- TODO: get rid
makeCommand name shortDesc longDesc defaultInitFlags options =
    CommandUI {
      commandName         = name,
      commandSynopsis     = shortDesc,
      commandUsage        = usageAlternatives name ["[FLAGS]"],
      commandDescription  = longDesc,
      commandNotes        = Nothing,
      commandDefaultFlags = defaultInitFlags,
      commandOptions      = options
    }
