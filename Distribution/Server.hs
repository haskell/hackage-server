module Distribution.Server (
    -- * Server control
    Server,
    initialise,
    run,
    shutdown,
    checkpoint,

    -- * Server configuration
    ServerConfig(..),
    defaultServerConfig,
    hasSavedState,

    -- * First time initialisation of the database
    bulkImport,
    importServerTar,
    exportServerTar,
    initState,

    -- * Temporary server while loading data
    setUpTemp,
    tearDownTemp
 ) where

import Happstack.Server hiding (port, host)
import qualified Happstack.Server
-- for resetting the temporary 'loading' server
import qualified Happstack.Util.Concurrent as HappsLoad

import Distribution.Server.Acid (Acid, checkpointAcid, startAcid, stopAcid, update)
import qualified Distribution.Server.Framework.BackupRestore as Import
import Distribution.Server.Framework.BackupDump
-- TODO: move this to BulkImport module
import Distribution.Server.Users.Backup (usersToCSV, groupToCSV)
import Distribution.Server.Packages.Backup (infoToCurrentEntries, maintToExport)
import Distribution.Server.Packages.Backup.Tags (tagsToCSV)
import Distribution.Server.Features.Tags (constructTagIndex)
import qualified Distribution.Server.Users.Group as Group

import Distribution.Server.Framework
import Distribution.Server.Framework.Feature as Feature
import qualified Distribution.Server.Features as Features

import Distribution.Server.Users.State as State
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.LegacyImport.BulkImport as BulkImport
import qualified Distribution.Server.LegacyImport.UploadLog as UploadLog
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth
import qualified Distribution.Server.Auth.Basic as Auth

import Distribution.Text

import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Control.Concurrent
import Control.Monad (when, mplus, msum)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI (URIAuth(URIAuth))
import Network.BSD (getHostName)
import Data.List (foldl')
import Data.Int  (Int64)

import Paths_hackage_server (getDataDir)

data ServerConfig = ServerConfig {
  confHostName  :: String,
  confPortNum   :: Int,
  confStateDir  :: FilePath,
  confStaticDir :: FilePath,
  confTmpDir    :: FilePath
} deriving (Show)

confAcidStateDir, confBlobStoreDir :: ServerConfig -> FilePath
confAcidStateDir config = confStateDir config </> "db"
confBlobStoreDir  config = confStateDir config </> "blobs"

defaultServerConfig :: IO ServerConfig
defaultServerConfig = do
  hostName <- getHostName
  dataDir  <- getDataDir
  return ServerConfig {
    confHostName  = hostName,
    confPortNum   = 8080,
    confStateDir  = "state",
    confStaticDir = dataDir </> "static",
    confTmpDir    = "upload-tmp"
  }

data Server = Server {
  serverAcid      :: Acid,
  serverFeatures  :: [HackageFeature],
  serverPort      :: Int,
  serverEnv       :: ServerEnv
}

-- | If we made a server instance from this 'ServerConfig', would we find some
-- existing saved state or would it be a totally clean instance with no
-- existing state.
--
hasSavedState :: ServerConfig -> IO Bool
hasSavedState = doesDirectoryExist . confAcidStateDir

-- | Make a server instance from the server configuration.
--
-- This does not yet run the server (see 'run') but it does setup the server
-- state system, making it possible to import data, and initializes the
-- features.
--
-- Note: the server instance must eventually be 'shutdown' or you'll end up
-- with stale lock files.
--
initialise :: ServerConfig -> IO Server
initialise initConfig@(ServerConfig hostName portNum stateDir staticDir tmpDir) = do
    exists <- doesDirectoryExist staticDir
    when (not exists) $ fail $ "The static files directory " ++ staticDir ++ " does not exist."

    createDirectoryIfMissing False stateDir
    store   <- BlobStorage.open blobStoreDir

--    txCtl   <- runTxSystem (Queue (FileSaver happsStateDir)) hackageEntryPoint
    acid <- startAcid acidStateDir

    let env = ServerEnv {
            serverStaticDir = staticDir,
            serverStateDir  = stateDir,
            serverBlobStore = store,
            serverTmpDir    = tmpDir,
            serverHostURI   = hostURI
         }
    -- do feature initialization
    features <- Features.initHackageFeatures env

    return Server {
        serverAcid      = acid,
        serverFeatures  = features,
        serverPort      = portNum,
        serverEnv       = env
    }

  where
    acidStateDir = confAcidStateDir initConfig
    blobStoreDir  = confBlobStoreDir  initConfig
    hostURI       = URIAuth "" hostName portStr
      where portStr | portNum == 80 = ""
                    | otherwise     = ':':show portNum

-- | Actually run the server, i.e. start accepting client http connections.
--
run :: Server -> IO ()
run server =
    simpleHTTP conf $ do

      handlePutPostQuotas

      fakeBrowserHttpMethods (impl server)

  where
    conf = nullConf { Happstack.Server.port = serverPort server }

    -- HS6 - Quotas should be configurable as well. Also there are places in
    -- the code that want to work with the request body directly but maybe
    -- fail if the request body has already been consumed. The body will only
    -- be consumed if it is a POST/PUT request *and* the content-type is
    -- multipart/form-data. If this does happen, you should get a clear error
    -- message saying what happened.
    handlePutPostQuotas = decodeBody bodyPolicy
      where
        tmpdir = serverTmpDir (serverEnv server)
        quota  = 10 ^ (6 :: Int64)
        bodyPolicy = defaultBodyPolicy tmpdir quota quota quota


    -- This is a cunning hack to solve the problem that current web browsers
    -- (non-HTML5 forms) do not support PUT, DELETE, etc, they only support GET
    -- and POST. We don't want to compromise the design of the whole server
    -- just because of browsers not supporting HTTP properly, so we allow
    -- browsers to PUT/DELETE etc by POSTing with a query or body paramater of
    -- _method=PUT/DELETE.
    fakeBrowserHttpMethods part =
      msum [ do methodOnly POST
                withDataFn (lookRead "_method") $ \mthd ->
                  localRq (\req -> req { rqMethod = mthd}) part

             -- or just do things the normal way
           , part
           ]


-- | Perform a clean shutdown of the server.
--
shutdown :: Server -> IO ()
shutdown server = stopAcid (serverAcid server)
--TODO: stop accepting incomming connections,
-- wait for connections to be processed.

-- | Write out a checkpoint of the server state. This makes recovery quicker
-- because fewer logged transactions have to be replayed.
--
checkpoint :: Server -> IO ()
checkpoint server = checkpointAcid (serverAcid server)

-- Convert a set of old data into a new export tarball.
-- This also populates the blob database, which is then
-- repopulated upon import of the new export tarball.
-- This is not really a good thing. (FIXME)
--
-- However, it does not need happstack-state to function.
bulkImport :: FilePath -- path to blob storage, get rid of this
           -> ByteString  -- Index
           -> String      -- Log
           -> Maybe ByteString -- archive
           -> Maybe String -- users
           -> Maybe String -- admin users
           -> IO ([UploadLog.Entry], ByteString)
bulkImport storageDir indexFile logFile archiveFile htPasswdFile adminsFile = do
    createDirectoryIfMissing True storageDir
    storage <- BlobStorage.open storageDir

    putStrLn "Reading index file"
    -- [(PackageIdentifier, Tar.Entry)]
    pkgIndex  <- either fail return $ BulkImport.importPkgIndex indexFile
    putStrLn "Reading log file"
    -- [UploadLog.Entry].
    uploadLog <- either fail return $ BulkImport.importUploadLog logFile
    -- [(PackageIdentifier, BlobId)]
    -- needs IO to store the blobs. with enough craftiness, the blob storage
    -- needn't be involved at all, merging on-the-fly
    putStrLn "Reading archive file"
    tarballs  <- BulkImport.importTarballs storage archiveFile
    -- Users
    putStrLn "Reading user accounts"
    accounts  <- either fail return $ BulkImport.importUsers htPasswdFile
    let admins = importAdminsList adminsFile

    -- [PkgInfo], Users, [UploadLog.Entry]
    -- it might be possible to export by skipping PkgInfo entirely, exporting
    -- the files along with versionListToCSV
    putStrLn "Merging package info"
    (pkgsInfo, users, badLogEntries) <- either fail return $ BulkImport.mergePkgInfo pkgIndex uploadLog tarballs accounts
    let pkgsIndex = PackageIndex.fromList pkgsInfo
        maint = BulkImport.mergeMaintainers pkgsInfo
    putStrLn "Done merging"
    adminUids <- case admins of
        Nothing -> return []
        Just adminUsers -> either fail return $ lookupUsers users adminUsers
    let adminFile = groupToCSV $ Group.fromList adminUids
        getEntries = do
            putStrLn "Creating package entries"
            currentPackageEntries <- readExportBlobs storage (concatMap infoToCurrentEntries pkgsInfo)
            putStrLn "Creating user entries"
            let userEntry  = csvToBackup ["users.csv"] . usersToCSV $ users
                adminEntry = csvToBackup ["admins.csv"] adminFile
            return $ currentPackageEntries ++ [userEntry, adminEntry]
        getTags = return [csvToBackup ["tags.csv"] . tagsToCSV . constructTagIndex $ pkgsIndex]
        getGroups = return [maintToExport maint, csvToBackup ["trustees.csv"] adminFile]
    putStrLn "Actually creating tarball"
    tarBytes <- exportTar [("core", getEntries),
                           ("tags", getTags),
                           ("upload", getGroups)]
    return (badLogEntries, tarBytes)
 where
    importAdminsList :: Maybe String -> Maybe [Users.UserName]
    importAdminsList = fmap (map Users.UserName . lines)

    lookupUsers users names = mapM lookupUser names
      where lookupUser name = case Users.lookupName name users of
                Nothing -> Left $ "User " ++ show name ++ " not found"
                Just uid -> Right uid

exportServerTar :: Server -> IO ByteString
exportServerTar server =
    exportTar
      [ (featureName feature, dump)
      | feature@HackageFeature {
          featureDumpRestore = Just (dump, _restore)
        } <- serverFeatures server ]

importServerTar :: Server -> ByteString -> IO (Maybe String)
importServerTar server tar =
    Import.importTar tar
      [ (featureName feature, restore)
      | feature@HackageFeature {
          featureDumpRestore = Just (_dump, restore)
        } <- serverFeatures server ]


-- An alternative to an import: starts the server off to a sane initial state.
-- To accomplish this, we import a 'null' tarball, finalizing immediately after initializing import
initState ::  Server -> (String, String) -> IO ()
initState server (admin, pass) = do
    Import.importBlank
      [ (featureName feature, restore)
      | feature@HackageFeature {
          featureDumpRestore = Just (_dump, restore)
        } <- serverFeatures server ]
    -- create default admin user
    muid <- case simpleParse admin of
        Just uname -> do
            let userAuth = Auth.newPasswdHash Auth.authorizationRealm uname (Auth.PasswdPlain pass)
            update $ AddUser uname (Users.UserAuth userAuth Auth.DigestAuth)
        Nothing -> fail "Couldn't parse admin name (should be alphanumeric)"
    case muid of
        Just uid -> update $ State.AddHackageAdmin uid
        Nothing  -> fail "Failed to create admin user"

-- The top-level server part.
-- It collects resources from Distribution.Server.Features, collects
-- them into a path hierarchy, and serves them.
impl :: Server -> ServerPart Response
impl server =
      flip mplus (fileServe ["hackage.html"] . serverStaticDir $ serverEnv server)
    -- ServerPart Response
    . renderServerTree []
    -- ServerTree ServerResponse
    . fmap serveResource
    -- ServerTree Resource
    . foldl' (\acc res -> addServerNode (resourceLocation res) res acc) serverTreeEmpty
    -- [Resource]
    $ concatMap Feature.featureResources (serverFeatures server)
-- where showServerTree tree = trace (drawServerTree tree (Just $ show . resourceMethods)) tree

data TempServer = TempServer ThreadId

setUpTemp :: ServerConfig -> Int -> IO TempServer
setUpTemp sconf secs = do
    tid <- forkIO $ do
        -- wait a certain amount of time before setting it up, because sometimes
        -- happstack-state is very fast, and switching the servers has a time
        -- cost to it
        threadDelay $ secs*1000000
        -- could likewise specify a mirror to redirect to for tarballs, and 503 for everything else
        simpleHTTP conf $ (resp 503 $ setHeader "Content-Type" "text/html" $ toResponse html503)
    return (TempServer tid)
  where
    conf = nullConf { Happstack.Server.port = confPortNum sconf }

-- | Static 503 page, based on Happstack's 404 page.
html503 :: String
html503 =
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ++
    "<html><head><title>503 Service Unavailable</title></head><body><h1>" ++
    "503 Service Unavailable</h1><p>The server is undergoing maintenance" ++
    "<br>It'll be back soon</p></body></html>"

tearDownTemp :: TempServer -> IO ()
tearDownTemp (TempServer tid) = do
    killThread tid
    HappsLoad.reset
    -- apparently reset doesn't give the server enough time to release the bind
    threadDelay $ 1000000

