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
   importTar,
   initState,
 ) where

import Happstack.Server hiding (port, host)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import qualified Distribution.Server.Backup.Import as Import (importTar)

--import Distribution.Server.Users.ServerParts
import Distribution.Server.Packages.ServerParts (stateToCache, updateCache) -- for the centralized caches
--import Distribution.Server.Distributions.ServerParts -- this will take some effort to revamp

import qualified Distribution.Server.Feature as Feature
import qualified Distribution.Server.Features as Features

import Distribution.Server.State as State
import Distribution.Server.Packages.State as State hiding (bulkImport)
import Distribution.Server.Users.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.Backup.BulkImport as BulkImport
import qualified Distribution.Server.Backup.UploadLog as UploadLog

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import Distribution.Server.Resource
import Distribution.Server.Types

import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans
import Control.Monad (when, mplus)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI (URIAuth(URIAuth))
import Network.BSD (getHostName)
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy.Char8 as BS

import Paths_hackage_server (getDataDir)

data ServerConfig = ServerConfig {
  confHostName  :: String,
  confPortNum   :: Int,
  confStateDir  :: FilePath,
  confStaticDir :: FilePath
} deriving (Show)

confHappsStateDir, confBlobStoreDir :: ServerConfig -> FilePath
confHappsStateDir config = confStateDir config </> "db"
confBlobStoreDir  config = confStateDir config </> "blobs"

defaultServerConfig :: IO ServerConfig
defaultServerConfig = do
  hostName <- getHostName
  dataDir  <- getDataDir
  return ServerConfig {
    confHostName  = hostName,
    confPortNum   = 8080,
    confStateDir  = "state",
    confStaticDir = dataDir </> "static"
  }

data Server = Server {
  serverTxControl :: MVar TxControl,
  serverFeatures  :: [Feature.HackageModule],
  serverPort      :: Int,
  serverConfig    :: Config
}

-- | If we made a server instance from this 'Config', would we find some
-- existing saved state or would it be a totally clean instance with no
-- existing state.
--
hasSavedState :: ServerConfig -> IO Bool
hasSavedState = doesDirectoryExist . confHappsStateDir

-- | Make a server instance from the server configuration.
--
-- This does not yet run the server (see 'run') but it does setup the server
-- state system, making it possible to import data.
--
-- Note: the server instance must eventually be 'shutdown' or you'll end up
-- with stale lock files.
--
initialise :: ServerConfig -> IO Server
initialise config@(ServerConfig hostName portNum stateDir staticDir) = do
  exists <- doesDirectoryExist staticDir
  when (not exists) $
    fail $ "The static files directory " ++ staticDir ++ " does not exist."

  createDirectoryIfMissing False stateDir
  store   <- BlobStorage.open blobStoreDir

  txCtl   <- runTxSystem (Queue (FileSaver happsStateDir)) hackageEntryPoint
  cache   <- do
      packages <- query GetPackagesState
      users    <- query GetUserDb
      Cache.new =<< stateToCache hostURI packages users

  features <- Features.hackageFeatures

  return Server {
    serverTxControl  = txCtl,
    serverFeatures   = features,
    serverPort       = portNum,
    serverConfig = Config {
      serverStore      = store,
      serverStaticDir  = staticDir,
      serverURI        = hostURI,
      serverCache      = cache
    }
  }

  where
    happsStateDir = confHappsStateDir config
    blobStoreDir  = confBlobStoreDir  config
    hostURI       = URIAuth "" hostName portStr
      where portStr | portNum == 80 = ""
                    | otherwise     = ':' : show portNum

hackageEntryPoint :: Proxy HackageEntryPoint
hackageEntryPoint = Proxy

-- | Actually run the server, i.e. start accepting client http connections.
--
run :: Server -> IO ()
run server = simpleHTTP conf $ mungeRequest $ impl server
  where
    conf = nullConf { Happstack.Server.port = serverPort server }
    mungeRequest :: ServerPart Response -> ServerPart Response
    mungeRequest = localRq mungeMethod
    -- like HTTP methods, but.. less so.
    mungeMethod req = case (rqMethod req, lookup "_method" (rqInputs req)) of
        (POST, Just input) -> case reads . map toUpper . BS.unpack $ inputValue input of
            [(newMethod, "")] -> req { rqMethod = newMethod }
            _ -> req
        _ -> req
    -- todo: given a .json or .html suffix, munge it into an Accept header
    -- can use MessageWrap.pathEls to reparse rqPath
    {- considered but discarded for PUTs: case lookup "_patharg" (rqInputs req) of
                Just param -> req' { rqUri = rqUri req </> SURI.escape param, rqPath = rqPath req ++ [param] }
                _ -> req'
              where req' = -}

-- | Perform a clean shutdown of the server.
--
shutdown :: Server -> IO ()
shutdown server = shutdownSystem (serverTxControl server)

-- | Write out a checkpoint of the server state. This makes recovery quicker
-- because fewer logged transactions have to be replayed.
--
checkpoint :: Server -> IO ()
checkpoint server = createCheckpoint (serverTxControl server)

bulkImport :: Server
           -> ByteString  -- Index
           -> String      -- Log
           -> Maybe ByteString -- archive
           -> Maybe String -- users
           -> Maybe String -- admin users
           -> IO [UploadLog.Entry]
bulkImport server  indexFile logFile archiveFile htPasswdFile adminsFile = do
    let config = serverConfig server
    pkgIndex  <- either fail return (BulkImport.importPkgIndex indexFile)
    uploadLog <- either fail return (BulkImport.importUploadLog logFile)
    tarballs  <- BulkImport.importTarballs (serverStore config) archiveFile
    accounts  <- either fail return (BulkImport.importUsers htPasswdFile)
    let admins = importAdminsList adminsFile

    (pkgsInfo, users, badLogEntries) <- either fail return
        (BulkImport.mergePkgInfo pkgIndex uploadLog tarballs accounts)

    update $ BulkImport pkgsInfo
    update $ ReplaceUserDb users

    case admins of
      Nothing -> return ()
      Just adminUsers -> do
        userDb <- query GetUserDb
        uids <- either fail return $ lookupUsers userDb adminUsers
        mapM_ (\uid -> update $ AddHackageAdmin uid) uids
    --let uploadPerms = map (\pkg -> (pkgUploadUser pkg, PackageMaintainer (packageName pkg))) pkgsInfo 
    --update $ BulkImportPermissions (admPerms ++ uploadPerms)

    updateCache config

    return badLogEntries

 where
   importAdminsList :: Maybe String -> Maybe [Users.UserName]
   importAdminsList
       = maybe Nothing (Just . map Users.UserName . lines)

   lookupUsers users names = mapM lookupUser names
    where lookupUser name = case Users.lookupName name users of
           Nothing -> Left $ "User " ++ show name ++ " not found"
           Just uid -> Right uid

importTar :: Server -> ByteString -> IO (Maybe String)
importTar server tar = do
    let config = serverConfig server
    let featureMap = Map.fromList . concatMap (\f -> maybe [] (\r -> [(Feature.featureName f, r)]) $ Feature.restoreBackup f) $ serverFeatures server
    res <- Import.importTar (serverStore config) tar featureMap
    case res of
        Nothing -> updateCache config
        Just _err -> return ()
    return res

-- An alternative to an import.
-- Starts the server off to a sane initial state.
initState ::  MonadIO m => Server -> m ()
initState server = do
  -- clear off existing state
  update $ BulkImport []
  update $ ReplaceUserDb Users.empty

  -- create default admin user
  let userName = Users.UserName "admin"
      userAuth = Auth.newDigestPass userName (Auth.PasswdPlain "admin") "hackage"
  res <- update $ AddUser userName (Users.UserAuth userAuth Auth.DigestAuth)

  case res of
    Just user -> update $ State.AddHackageAdmin user
    _ -> fail "Failed to create admin user!"

  updateCache (serverConfig server)


impl :: Server -> ServerPart Response
impl server =
      flip mplus (fileServe ["hackage.html"] . serverStaticDir . serverConfig $ server)
    -- ServerPart Response
    . renderServerTree (serverConfig server) []
    -- ServerTree ServerResponse
    . fmap (snd . serveResource)
    -- ServerTree Resource
    . foldl' (\acc res -> addServerNode (resourceLocation res) res acc) serverTreeEmpty
    -- [Resource]
    $ concatMap Feature.resources (serverFeatures server)
--where showServerTree tree = trace (drawServerTree tree) tree

{-core :: Server -> ServerPart Response
core server = msum []
  [ dir "package" $ msum
      [ path $ msum . handlePackageById store
      , path $ servePackage store
      ]
  [ dir "buildreports" $ msum (buildReports store)
--  , dir "groups" (groupInterface)
  , dir "recent.rss" $ msum
      [ methodSP GET $ ok . Cache.packagesFeed =<< Cache.get cache ]
  , dir "recent.html" $ msum
      [ methodSP GET $ ok . Cache.recentChanges =<< Cache.get cache ]
  , dir "admin" $ admin static store
  , dir "check" checkPackage
--  , dir "htpasswd" $ msum [ changePassword ]
--  , dir "distro" distros
  , fileServe ["hackage.html"] static
  ]

-- Top level server part for administrative actions under the "admin"
-- directory
admin :: FilePath -> BlobStorage -> ServerPart Response
admin static storage = do
    userDb <- query State.GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.requireHackageAuth userDb (Just admins) Nothing
    msum
        [ dir "users" userAdmin
        , dir "export.tar.gz" (export storage)
--        , adminDist
        , fileServe ["admin.html"] static
        ]-}
