{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Distribution.Server (
    -- * Server control
    Server,
    initialise,
    run,
    shutdown,
    checkpoint,

    -- * Server configuration
    ListenOn(..),
    ServerConfig(..),
    defaultServerConfig,
    hasSavedState,

    -- * First time initialisation of the database
    importServerTar,
    exportServerTar,
    testRoundtrip,
    initState,

    -- * Temporary server while loading data
    setUpTemp,
    tearDownTemp
 ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import qualified Distribution.Server.Framework.BackupRestore as Import
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Server.Framework.Feature as Feature
import qualified Distribution.Server.Features as Features
import Distribution.Server.Features.Users
import Distribution.Server.Framework.AuthTypes (PasswdPlain(..))

import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Group

import Distribution.Text
import Distribution.Verbosity as Verbosity

import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Control.Concurrent
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI (URIAuth(URIAuth))
import Network.BSD (getHostName)
import Data.List (foldl')
import Data.Int  (Int64)
import qualified System.Log.Logger as HsLogger

import Paths_hackage_server (getDataDir)


data ListenOn = ListenOn {
  loPortNum :: Int,
  loIP :: String
} deriving (Show)

data ServerConfig = ServerConfig {
  confVerbosity :: Verbosity,
  confHostName  :: String,
  confListenOn  :: ListenOn,
  confStateDir  :: FilePath,
  confStaticDir :: FilePath,
  confTmpDir    :: FilePath
} deriving (Show)

confDbStateDir, confBlobStoreDir :: ServerConfig -> FilePath
confDbStateDir   config = confStateDir config </> "db"
confBlobStoreDir config = confStateDir config </> "blobs"

defaultServerConfig :: IO ServerConfig
defaultServerConfig = do
  hostName <- getHostName
  dataDir  <- getDataDir
  return ServerConfig {
    confVerbosity = Verbosity.normal,
    confHostName  = hostName,
    confListenOn  = ListenOn {
                        loPortNum = 8080,
                        loIP = "0.0.0.0"
                    },
    confStateDir  = "state",
    confStaticDir = dataDir </> "static",
    confTmpDir    = "upload-tmp"
  }

data Server = Server {
  serverFeatures    :: [HackageFeature],
  serverUserFeature :: UserFeature,
  serverListenOn    :: ListenOn,
  serverVerbosity   :: Verbosity,
  serverEnv         :: ServerEnv
}

-- | If we made a server instance from this 'ServerConfig', would we find some
-- existing saved state or would it be a totally clean instance with no
-- existing state.
--
hasSavedState :: ServerConfig -> IO Bool
hasSavedState = doesDirectoryExist . confDbStateDir

-- | Make a server instance from the server configuration.
--
-- This does not yet run the server (see 'run') but it does setup the server
-- state system, making it possible to import data, and initializes the
-- features.
--
-- Note: the server instance must eventually be 'shutdown' or you'll end up
-- with stale lock files.
--
initialise :: Bool -> ServerConfig -> IO Server
initialise enableCaches initConfig@(ServerConfig verbosity hostName listenOn
                                                 stateDir staticDir tmpDir) = do
    createDirectoryIfMissing False stateDir
    store   <- BlobStorage.open blobStoreDir

    let env = ServerEnv {
            serverStaticDir = staticDir,
            serverStateDir  = stateDir,
            serverBlobStore = store,
            serverTmpDir    = tmpDir,
            serverHostURI   = hostURI
         }
    -- do feature initialization
    (features, userFeature) <- Features.initHackageFeatures enableCaches env

    return Server {
        serverFeatures    = features,
        serverUserFeature = userFeature,
        serverListenOn    = listenOn,
        serverVerbosity   = verbosity,
        serverEnv         = env
    }

  where
    blobStoreDir  = confBlobStoreDir  initConfig
    hostURI       = URIAuth "" hostName portStr
      where portNum = loPortNum listenOn
            portStr | portNum == 80 = ""
                    | otherwise     = ':':show portNum

-- | Actually run the server, i.e. start accepting client http connections.
--
run :: Server -> IO ()
run server = do
    -- We already check this in Main, so we expect this check to always
    -- succeed, but just in case...
    let staticDir = serverStaticDir (serverEnv server)
    exists <- doesDirectoryExist staticDir
    when (not exists) $ fail $ "The static files directory " ++ staticDir ++ " does not exist."

    runServer listenOn $ do

      handlePutPostQuotas

      setLogging

      fakeBrowserHttpMethods (impl server)

  where
    listenOn = serverListenOn server

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

    setLogging =
        liftIO $ HsLogger.updateGlobalLogger
                   "Happstack.Server"
                   (adjustLogLevel (serverVerbosity server))
      where
        adjustLogLevel v
          | v == Verbosity.normal    = HsLogger.setLevel HsLogger.WARNING
          | v == Verbosity.verbose   = HsLogger.setLevel HsLogger.INFO
          | v == Verbosity.deafening = HsLogger.setLevel HsLogger.DEBUG
          | otherwise                = id

    -- This is a cunning hack to solve the problem that current web browsers
    -- (non-HTML5 forms) do not support PUT, DELETE, etc, they only support GET
    -- and POST. We don't want to compromise the design of the whole server
    -- just because of browsers not supporting HTTP properly, so we allow
    -- browsers to PUT/DELETE etc by POSTing with a query or body paramater of
    -- _method=PUT/DELETE.
    fakeBrowserHttpMethods part =
      msum [ do method POST
                methodOverrideHack part

             -- or just do things the normal way
           , part
           ]


-- | Perform a clean shutdown of the server.
--
shutdown :: Server -> IO ()
shutdown server =
  Features.shutdownAllFeatures (serverFeatures server)

--TODO: stop accepting incomming connections,
-- wait for connections to be processed.

-- | Write out a checkpoint of the server state. This makes recovery quicker
-- because fewer logged transactions have to be replayed.
--
checkpoint :: Server -> IO ()
checkpoint server =
  Features.checkpointAllFeatures (serverFeatures server)

exportServerTar :: Server -> IO ByteString
exportServerTar server = exportTar store
  [ (featureName feature, backupState st <$> getState st)
  | feature               <- serverFeatures server
  , SomeStateComponent st <- featureState feature
  ]
    where
      store = serverBlobStore (serverEnv server)

importServerTar :: Server -> ByteString -> IO (Maybe String)
importServerTar server tar = Import.importTar tar
  [ (featureName feature, featureRestoreState feature)
  | feature <- serverFeatures server
  ]

-- Import.importTar requires a single entry per feature
featureRestoreState :: HackageFeature -> Import.RestoreBackup
featureRestoreState feature =
  mconcat [ restoreState st
          | SomeStateComponent st <- featureState feature
          ]

-- TODO: this computes all the checks before executing them, rather than
-- interleaving the creation of the checks with their execution.
-- This is bad for memory usage, but interleaving would require a different
-- setup; in particular, a different type for testRoundtrip.
testRoundtrip :: Server -> Import.TestRoundtrip
testRoundtrip server = do
    checks <- sequence [ addFeatureName feature <$> testBackup st
                       | feature               <- serverFeatures server
                       , SomeStateComponent st <- featureState feature
                       ] :: IO [IO [String]]
    return $ concat <$> sequence checks
  where
    -- Add the name of the feature to the reported errors
    addFeatureName :: HackageFeature -> IO [String] -> IO [String]
    addFeatureName feature = liftM $ map ((featureName feature ++ ": ") ++)

-- An alternative to an import: starts the server off to a sane initial state.
-- To accomplish this, we import a 'null' tarball, finalizing immediately after initializing import
initState ::  Server -> (String, String) -> IO ()
initState server (admin, pass) = do
    void $ Import.importBlank
      [ (featureName feature, featureRestoreState feature)
      | feature <- serverFeatures server
      ]
    -- create default admin user
    let UserFeature{updateAddUser, adminGroup} = serverUserFeature server
    muid <- case simpleParse admin of
        Just uname -> do
            let userAuth = newPasswdHash hackageRealm uname (PasswdPlain pass)
            updateAddUser uname (Users.NewUserAuth userAuth)
        Nothing -> fail "Couldn't parse admin name (should be alphanumeric)"
    case muid of
        Right uid -> Group.addUserList adminGroup uid
        Left err  -> fail $ "Failed to create admin user: " ++ err

-- The top-level server part.
-- It collects resources from Distribution.Server.Features, collects
-- them into a path hierarchy, and serves them.
impl :: Server -> ServerPart Response
impl server =
      flip mplus (serveDirectory DisableBrowsing ["hackage.html"] . serverStaticDir $ serverEnv server)
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
        runServer listenOn $ (resp 503 $ setHeader "Content-Type" "text/html" $ toResponse html503)
    return (TempServer tid)
  where listenOn = confListenOn sconf

runServer :: (ToMessage a) => ListenOn -> ServerPartT IO a -> IO ()
runServer listenOn f
    = do socket <- bindIPv4 (loIP listenOn) (loPortNum listenOn)
         simpleHTTPWithSocket socket nullConf f

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
    -- give the server enough time to release the bind
    threadDelay $ 1000000

