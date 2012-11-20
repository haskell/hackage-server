{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

module Main where

import Network.HTTP
import Network.Browser hiding (err)
import Network.URI (URI(..), URIAuth(..), parseURI)

import qualified Distribution.Client.HtPasswdDb as HtPasswdDb
import qualified Distribution.Client.UploadLog  as UploadLog
import qualified Distribution.Client.TagsFile   as TagsFile
import qualified Distribution.Client.DistroMap  as DistroMap
import qualified Distribution.Client.PkgIndex   as PkgIndex

import Distribution.Server.Users.Types (UserName(..))
import Distribution.Server.Framework.AuthTypes (HtPasswdHash(..))

import Distribution.Package
         ( PackageId, PackageName, packageName )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( topHandler, die, {-warn, debug,-} wrapText, toUTF8 )
import Distribution.Verbosity
         ( Verbosity, normal, verbose )

import Distribution.Simple.Command
import Distribution.Simple.Setup
         ( Flag(..), fromFlag, flagToList )

import System.Environment
         ( getArgs, getProgName )
import System.Locale
         ( defaultTimeLocale )
import System.Exit
         ( exitWith, ExitCode(..) )
import System.IO
import System.FilePath
         ( (</>), (<.>), takeFileName, dropExtension, takeExtension )
import qualified System.FilePath.Posix as Posix
import Data.List
import Data.Maybe
import Data.Time (UTCTime, formatTime)
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.JSON as JSON
         ( JSValue(..), toJSObject, toJSString, encodeStrict )
import qualified Text.CSV as CSV

import Paths_hackage_server as Paths (version)

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
          _ | fromFlag (flagVersion globalflags) -> printVersion
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
    printVersion = putStrLn $ "hackage-import " ++ display version

    commands =
      [ usersCommand    `commandAddAction` usersAction
      , metadataCommand `commandAddAction` metadataAction
      , tarballCommand  `commandAddAction` tarballAction
      , distroCommand   `commandAddAction` distroAction
      , deprecationCommand `commandAddAction` deprecationAction
      , docsCommand     `commandAddAction` docsAction
      ]

    commandAddActionNoArgs cmd action =
      commandAddAction cmd $ \flags extraArgs globalFlags -> do
        when (not (null extraArgs)) $
          die $ "'" ++ commandName cmd
             ++ "' does not take any extra arguments: " ++ unwords extraArgs
        action flags globalFlags



-------------------------------------------------------------------------------
-- Global command
--

data GlobalFlags = GlobalFlags {
    flagVersion :: Flag Bool
  }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    flagVersion = Flag False
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
         flagVersion (\v flags -> flags { flagVersion = v })
         (noArg (Flag True))
      ]
  }


-------------------------------------------------------------------------------
-- Users command
--

data UsersFlags = UsersFlags {
    flagImportHtPasswd  :: Flag FilePath,
    flagImportAddresses :: Flag FilePath
  }

defaultUsersFlags :: UsersFlags
defaultUsersFlags = UsersFlags {
    flagImportHtPasswd  = NoFlag,
    flagImportAddresses = NoFlag
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
          flagImportHtPasswd (\v flags -> flags { flagImportHtPasswd = v })
          (reqArgFlag "HTPASSWD")
      , option [] ["addresses"]
          "Import user email addresses"
          flagImportAddresses (\v flags -> flags { flagImportAddresses = v })
          (reqArgFlag "ADDRESSES")
      ]

usersAction :: UsersFlags -> [String] -> GlobalFlags -> IO ()
usersAction opts args _ = do
    baseURI <- either die return (validateOptsServerURI args)

    when (flagImportHtPasswd opts == NoFlag
       && flagImportAddresses opts == NoFlag) $
      die $ "specify what to import using one or more of the flags:\n"
         ++ "  --htpasswd=  --addresses="

    case flagImportHtPasswd opts of
      NoFlag    -> return ()
      Flag file -> importAccounts file baseURI

    case flagImportAddresses opts of
      NoFlag    -> return ()
      Flag file -> importAddresses file baseURI

importAccounts :: FilePath -> URI -> IO ()
importAccounts htpasswdFile baseURI = do

  htpasswdDb <- either die return . HtPasswdDb.parse
            =<< readFile htpasswdFile

  httpSession $ do
    setAuthorityFromURI baseURI
    sequence_
      [ putUserAccount baseURI username mPasswdhash
      | (username, mPasswdhash) <- htpasswdDb ]


putUserAccount :: URI -> UserName -> Maybe HtPasswdHash -> HttpSession ()
putUserAccount baseURI username mPasswdHash = do

    rsp <- requestPUT userURI "" BS.empty
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

    case mPasswdHash of
      Nothing -> return ()
      Just (HtPasswdHash passwdHash) -> do
        rsp <- requestPUT passwdURI "text/plain" (BS.pack passwdHash)
        case rsp of
          Nothing  -> return ()
          Just err -> fail (formatErrorResponse err)

  where
    userURI   = baseURI <//> "user" </> display username
    passwdURI = userURI <//> "htpasswd"

importAddresses :: FilePath -> URI -> IO ()
importAddresses _addressesFile _baseURI =
    die "--addresses flag not yet implemented"


-------------------------------------------------------------------------------
-- Metadata command
--

data MetadataFlags = MetadataFlags {
    flagImportIndex     :: Flag FilePath,
    flagImportUploadLog :: Flag FilePath
  }

defaultMetadataFlags :: MetadataFlags
defaultMetadataFlags = MetadataFlags {
    flagImportIndex     = NoFlag,
    flagImportUploadLog = NoFlag
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
          flagImportIndex (\v flags -> flags { flagImportIndex = v })
          (reqArgFlag "INDEX")
      , option [] ["upload-log"]
          "Import who uploaded what when, and set maintainer groups"
          flagImportUploadLog (\v flags -> flags { flagImportUploadLog = v })
          (reqArgFlag "LOG")
      ]

metadataAction :: MetadataFlags -> [String] -> GlobalFlags -> IO ()
metadataAction opts args _ = do

    baseURI <- either die return (validateOptsServerURI args)

    when (flagImportIndex opts == NoFlag
       && flagImportUploadLog opts == NoFlag) $
      die $ "specify what to import using one or more of the flags:\n"
         ++ "  --index=  --upload-log="

    case flagImportIndex opts of
      NoFlag    -> return ()
      Flag file -> importIndex file baseURI

    case flagImportUploadLog opts of
      NoFlag    -> return ()
      Flag file -> importUploadLog file baseURI

importIndex :: FilePath -> URI -> IO ()
importIndex indexFile baseURI = do
    info $ "Reading index file " ++ indexFile
    pkgs  <- either fail return
           . PkgIndex.readPkgIndex
         =<< BS.readFile indexFile

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putCabalFile baseURI pkgid cabalFile
        |  (pkgid, cabalFile) <- pkgs ]

putCabalFile :: URI -> PackageId -> ByteString -> HttpSession ()
putCabalFile baseURI pkgid cabalFile = do

    rsp <- requestPUT pkgURI "text/plain" cabalFile
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgid
                      </> display (packageName pkgid) <.> "cabal"


importUploadLog :: FilePath -> URI -> IO ()
importUploadLog uploadLogFile baseURI = do
    info $ "Reading log file " ++ uploadLogFile
    uploadLog <- either fail return
               . UploadLog.read
             =<< readFile uploadLogFile

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putUploadInfo baseURI pkgid time uname
        | (pkgid, time, uname) <- UploadLog.collectUploadInfo uploadLog ]
      sequence_
        [ putMaintainersInfo baseURI pkgname maintainers
        | (pkgname, maintainers) <- UploadLog.collectMaintainerInfo uploadLog ]


putUploadInfo :: URI -> PackageId -> UTCTime -> UserName -> HttpSession ()
putUploadInfo baseURI pkgid time uname = do

    liftIO $ info $ "setting upload info for " ++ display pkgid

    let timeStr = formatTime defaultTimeLocale "%c" time
    rsp <- requestPUT (pkgURI <//> "upload-time") "text/plain" (toBS timeStr)
    case rsp of
      Nothing  -> return ()
      Just err | isErrNotFound err -> liftIO $ info $ "Ignoring upload log entry for package " ++ display pkgid
      Just err -> fail (formatErrorResponse err)

    let nameStr = display uname
    rsp <- requestPUT (pkgURI <//> "uploader") "text/plain" (toBS nameStr)
    case rsp of
      Nothing  -> return ()
      Just err | isErrNotFound err  -> liftIO $ info $ "Ignoring upload log entry for package " ++ display pkgid
      Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgid
    toBS   = BS.pack . toUTF8


putMaintainersInfo :: URI -> PackageName -> [UserName] -> HttpSession ()
putMaintainersInfo baseURI pkgname maintainers =

    -- Add to the package's maintainers group
    forM_ maintainers $ \uname -> do
      rsp <- requestPUT (pkgURI <//> "maintainers" </> "user" </> display uname) "" BS.empty
      case rsp of
        Nothing  -> return ()
        Just err | isErrNotFound err -> liftIO $ info $ "Ignoring upload log entry for package " ++ display pkgname
        Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgname


-------------------------------------------------------------------------------
-- Tarballs command
--

data TarballFlags = TarballFlags

defaultTarballFlags :: TarballFlags
defaultTarballFlags = TarballFlags

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
                  ++ " local .tar.gz files."
    options _  = []

tarballAction :: TarballFlags -> [String] -> GlobalFlags -> IO ()
tarballAction _opts args _ = do

    (baseURI, tarballFiles) <- either die return (validateOptsServerURI' args)

    let pkgidAndTarball =
          [ (mpkgid, file)
          | file <- tarballFiles
          , let pkgidstr = (dropExtension . dropExtension . takeFileName) file
                ext      = takeExtension (dropExtension file)
                             <.> takeExtension file
                mpkgid | ext == ".tar.gz" = simpleParse pkgidstr
                       | otherwise        = Nothing ]

    case [ file | (Nothing, file) <- pkgidAndTarball] of
      []    -> return ()
      files -> die $ "the following files don't match the expected naming "
                  ++ "convention of foo-1.0.tar.gz:\n" ++ unlines files

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putPackageTarball baseURI pkgid tarballFilePath
        |  (Just pkgid, tarballFilePath) <- pkgidAndTarball ]

putPackageTarball :: URI -> PackageId -> FilePath -> HttpSession ()
putPackageTarball baseURI pkgid tarballFilePath = do

    pkgContent <- liftIO $ BS.readFile tarballFilePath
    rsp <- requestPUT pkgURI "application/x-gzip" pkgContent
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

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
                  ++ "convention so we know which package the tags apply to."
    options _  = []

deprecationAction :: DeprecationFlags -> [String] -> GlobalFlags -> IO ()
deprecationAction _opts args _ = do

    (baseURI, tagFiles) <- either die return (validateOptsServerURI' args)

    entries <- forM tagFiles $ \tagFile -> do
      content <- readFile tagFile
      either die return (TagsFile.read tagFile content)

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putDeprecatedInfo baseURI pkgname replacement
        |  (pkgname, replacement) <- TagsFile.collectDeprecated entries ]


putDeprecatedInfo :: URI -> PackageName -> Maybe PackageName -> HttpSession ()
putDeprecatedInfo baseURI pkgname replacement = do

    rsp <- requestPUT (pkgURI <//> "deprecated") "text/json" (toBS deprecatedInfo)
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    pkgURI = baseURI <//> "package" </> display pkgname
    toBS   = BS.pack . toUTF8 . JSON.encodeStrict

    deprecatedInfo =
      JSObject $ toJSObject
          [ ("is-deprecated", JSBool True)
          , ("in-favour-of", JSArray [ JSString $ toJSString $ display pkg
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
distroAction _opts args _ = do

    (baseURI, distroFiles) <- either die return (validateOptsServerURI' args)

    distros <- forM distroFiles $ \distroFile -> do
      content <- readFile distroFile
      let (errs, entries) = DistroMap.read content
      mapM_ info errs
      return (takeFileName distroFile, entries)

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putDistroInfo baseURI distroname entries
        | (distroname, entries) <- distros ]


putDistroInfo :: URI -> String -> [DistroMap.Entry] -> HttpSession ()
putDistroInfo baseURI distroname entries = do

    rsp <- requestPUT distroURI "" BS.empty
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

    rsp <- requestPUT (distroURI <//> "packages.csv") "text/csv" (toBS entries)
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    distroURI = baseURI <//> "distro" </> distroname
    toBS      = BS.pack . toUTF8 . CSV.printCSV . DistroMap.toCSV


-------------------------------------------------------------------------------
-- Documentation tarballs command
--

data DocsFlags = DocsFlags

defaultDocsFlags :: DocsFlags
defaultDocsFlags = DocsFlags

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
                     "The package tarballs can be imported directly from\n"
                  ++ " local .tar.gz files of the html bundle."
    options _  = []

docsAction :: DocsFlags -> [String] -> GlobalFlags -> IO ()
docsAction _opts args _ = do

    (baseURI, docTarballFiles) <- either die return (validateOptsServerURI' args)

    let pkgidAndTarball =
          [ (mpkgid, file)
          | file <- docTarballFiles
          , let pkgidstr = (dropExtension . dropExtension. takeFileName) file
                ext      = takeExtension (dropExtension file)
                             <.> takeExtension file
                mpkgid | ext == ".tar.gz" = simpleParse pkgidstr
                       | otherwise        = Nothing ]

    case [ file | (Nothing, file) <- pkgidAndTarball] of
      []    -> return ()
      files -> die $ "the following files don't match the expected naming "
                  ++ "convention of foo-1.0[-*].tar.gz:\n" ++ unlines files

    httpSession $ do
      setAuthorityFromURI baseURI
      sequence_
        [ putPackageDocsTarball baseURI pkgid tarballFilePath
        |  (Just pkgid, tarballFilePath) <- pkgidAndTarball ]

putPackageDocsTarball :: URI -> PackageId -> FilePath -> HttpSession ()
putPackageDocsTarball baseURI pkgid tarballFilePath = do

    tarballContent <- liftIO $ BS.readFile tarballFilePath
    rsp <- requestPUT pkgDocURI "application/x-gzip" tarballContent
    case rsp of
      Nothing  -> return ()
      Just err -> fail (formatErrorResponse err)

  where
    pkgDocURI = baseURI <//> "package" </> display pkgid </> "doc"


-------------------------
-- HTTP utilities
-------------------------

infixr 5 <//>

(<//>) :: URI -> FilePath -> URI
uri <//> path = uri { uriPath = Posix.addTrailingPathSeparator (uriPath uri)
                                Posix.</> path }

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
      setUserAgent  ("hackage-import/" ++ display version)
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

setAuthorityFromURI :: URI -> HttpSession ()
setAuthorityFromURI uri = do
   setAuthorityGen provideAuthInfo
  where
    credentials = extractCredentials uri

    provideAuthInfo :: URI -> String -> IO (Maybe (String, String))
    provideAuthInfo uri' _realm
      | hostName uri' == hostName uri = return credentials
      | otherwise                     = return Nothing
      where
        hostName = fmap uriRegName . uriAuthority

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


-------------------------------------------------------------------------------
-- Utils
--

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

validateOptsServerURI :: [String] -> Either String URI
validateOptsServerURI [server] = validateHttpURI server
validateOptsServerURI _        = Left $ "The command expects the target server "
                            ++ "URI e.g. http://admin:admin@localhost:8080/"

validateOptsServerURI' :: [String] -> Either String (URI, [String])
validateOptsServerURI' (server:opts) = (\uri -> (uri,opts)) `fmap` validateHttpURI server
validateOptsServerURI' _             = Left $ "The command expects the target server "
                            ++ "URI e.g. http://admin:admin@localhost:8080/"

