module Distribution.Server (
   -- * Server control
   Server,
   initialise,
   run,
   shutdown,
   checkpoint,

   -- * Server configuration
   Config(..),
   defaultConfig,
   hasSavedState,

   -- * First time initialisation of the database
   bulkImport,
   importTar,
   initState,
 ) where

import Distribution.Package
         ( PackageIdentifier(..), packageName, PackageName(..) )
import Distribution.Text
         ( display )
import Happstack.Server hiding (port)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import qualified Distribution.Server.Import as Import ( importTar )

import Distribution.Server.Packages.ServerParts
import Distribution.Server.Users.ServerParts
import Distribution.Server.Users.Permissions (GroupName(..))

import Distribution.Server.State as State
import Distribution.Server.Packages.State as State hiding (buildReports, bulkImport)
import Distribution.Server.Users.State as State
import qualified Distribution.Server.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.Auth.Types as Auth
import Distribution.Server.Packages.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import qualified Distribution.Server.BulkImport as BulkImport
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users

import Distribution.Server.Auth.Types (PasswdPlain(..))

import System.FilePath ((</>))
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist )
import Control.Concurrent.MVar (MVar)
import Data.Maybe
import Control.Monad.Trans
import Control.Monad (when,msum)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI
         ( URIAuth(URIAuth) )
import Network.BSD
         ( getHostName )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8

import Paths_hackage_server (getDataDir)

data Config = Config {
  confHostName  :: String,
  confPortNum   :: Int,
  confStateDir  :: FilePath,
  confStaticDir :: FilePath
}

confHappsStateDir, confBlobStoreDir :: Config -> FilePath
confHappsStateDir config = confStateDir config </> "db"
confBlobStoreDir  config = confStateDir config </> "blobs"

defaultConfig :: IO Config
defaultConfig = do
  hostName <- getHostName
  dataDir  <- getDataDir
  return Config {
    confHostName  = hostName,
    confPortNum   = 8080,
    confStateDir  = "state",
    confStaticDir = dataDir </> "static"
  }

data Server = Server {
  serverStore      :: BlobStorage,
  serverStaticDir  :: FilePath,
  serverTxControl  :: MVar TxControl,
  serverCache      :: Cache.Cache,
  serverURI        :: URIAuth,
  serverPort       :: Int
}

-- | If we made a server instance from this 'Config', would we find some
-- existing saved state or would it be a totally clean instance with no
-- existing state.
--
hasSavedState :: Config -> IO Bool
hasSavedState = doesDirectoryExist . confHappsStateDir

-- | Make a server instance from the server configuration.
--
-- This does not yet run the server (see 'run') but it does setup the server
-- state system, making it possible to import data.
--
-- Note: the server instance must eventually be 'shutdown' or you'll end up
-- with stale lock files.
--
initialise :: Config -> IO Server
initialise config@(Config hostName portNum stateDir staticDir) = do

  exists <- doesDirectoryExist staticDir
  when (not exists) $
    fail $ "The static files directory " ++ staticDir ++ " does not exist."

  createDirectoryIfMissing False stateDir
  store   <- BlobStorage.open blobStoreDir

  txCtl   <- runTxSystem (Queue (FileSaver happsStateDir)) hackageEntryPoint
  cache   <- Cache.new =<< stateToCache hostURI =<< query GetPackagesState

  return Server {
    serverStore      = store,
    serverStaticDir  = staticDir,
    serverTxControl  = txCtl,
    serverCache      = cache,
    serverURI        = hostURI,
    serverPort       = portNum
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
run server = simpleHTTP conf $ msum (impl server)
  where
    conf = nullConf { Happstack.Server.port = serverPort server }

-- | Perform a clean shutdown of the server.
--
shutdown :: Server -> IO ()
shutdown server = closeTxControl (serverTxControl server)

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
bulkImport (Server store _ _ cache host _)
           indexFile logFile archiveFile htPasswdFile adminsFile = do
  pkgIndex  <- either fail return (BulkImport.importPkgIndex indexFile)
  uploadLog <- either fail return (BulkImport.importUploadLog logFile)
  tarballs  <- BulkImport.importTarballs store archiveFile
  accounts  <- either fail return (BulkImport.importUsers htPasswdFile)
  let admins = importAdminsList adminsFile

  (pkgsInfo, users, badLogEntries) <- either fail return
    (BulkImport.mergePkgInfo pkgIndex uploadLog tarballs accounts)

  update $ BulkImport pkgsInfo users

  admPerms <- case admins of
    Nothing -> return []
    Just adminUsers -> do
        state <- query GetPackagesState
        uids <- either fail return $ lookupUsers (userDb state) adminUsers
        return $ map (\uid -> (uid, Administrator)) uids

  let uploadPerms
          = map (\pkg -> (pkgUploadUser pkg, PackageMaintainer (packageName pkg))) pkgsInfo 

  update $ BulkImportPermissions (admPerms ++ uploadPerms)

  updateCache cache host

  return badLogEntries

 where
   importAdminsList :: Maybe String -> Maybe [Users.UserName]
   importAdminsList
       = maybe Nothing (Just . map Users.UserName . lines)

   lookupUsers users names = mapM lookup names
    where lookup name = case Users.lookupName name users of
           Nothing -> Left $ "User " ++ show name ++ " not found"
           Just uid -> Right uid

importTar :: Server -> ByteString -> IO (Maybe String)
importTar (Server store _ _ cache host _) tar = do
  res <- Import.importTar store tar
  case res of
    Nothing -> do
             updateCache cache host
    Just err -> return ()
  return res

-- An alternative to an import.
-- Starts the server off to a sane initial state.
initState ::  MonadIO m => Server -> m ()
initState (Server _ _ _ cache host _) = do
  -- clear off existing state
  update $ BulkImport [] Users.empty
  update $ BulkImportPermissions []

  -- create default admin user
  let userName = Users.UserName "admin"
  userAuth <- newPasswd (PasswdPlain "admin")
  result <- update $ AddUser userName userAuth

  case result of
    Just user -> update $ AddToGroup Administrator user
    _ -> fail "Failed to create admin user!"

  updateCache cache host

-- Support the same URL scheme as the first version of hackage.
legacySupport :: ServerPart Response
legacySupport = msum
    [ path $ \name -> msum
      [ path $ \version -> msum
        [ let dirName = display pkgid ++ ".tar.gz"
              pkgid = PackageIdentifier {pkgName = PackageName name, pkgVersion = version}
          in dir dirName $ msum
             [ methodSP GET $
                 movedPermanently ("/package/"++ display pkgid ++ "/") (toResponse "")
             ]
        ]]
    , dir "00-index.tar.gz" $ msum
      [ methodSP GET $
               movedPermanently "/00-index.tar.gz" (toResponse "")
      ]
    ]

impl :: Server -> [ServerPartT IO Response]
impl (Server store static _ cache host _) =
  [ dir "packages" $ msum
      [ legacySupport
      , methodSP GET $
          ok . Cache.packagesPage =<< Cache.get cache
      ]
  , dir "package" $ msum
      [ path $ msum . handlePackageById store
      , path $ servePackage store
      ]
  , dir "buildreports" $ msum (buildReports store)
--  , dir "groups" (groupInterface)
  , dir "recent.rss" $ msum
      [ methodSP GET $ ok . Cache.packagesFeed =<< Cache.get cache ]
  , dir "recent.html" $ msum
      [ methodSP GET $ ok . Cache.recentChanges =<< Cache.get cache ]
  , dir "upload" $ msum
      [ uploadPackage store cache host ]
  , dir "00-index.tar.gz" $ msum
      [ methodSP GET $ do
          cacheState <- Cache.get cache
          ok $ toResponse $ Resource.IndexTarball (Cache.indexTarball cacheState)
      ]
  , dir "admin" (admin store cache host)
  , dir "check" checkPackage
  , dir "htpasswd" $ msum
      [ changePassword ]
  ,
  fileServe ["hackage.html"] static
  ]
