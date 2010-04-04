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
         ( PackageIdentifier(..), packageName, PackageId )
import Distribution.Text
         ( display, simpleParse )
import Happstack.Server hiding (port, host)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import Distribution.Server.ServerParts
    (guardAuth)
import qualified Distribution.Server.Import as Import ( importTar )

import Distribution.Server.Packages.ServerParts
import Distribution.Server.Users.ServerParts
import Distribution.Server.Distributions.ServerParts
import Distribution.Server.Users.Permissions (GroupName(..))

import Distribution.Server.State as State
import Distribution.Server.Packages.State as State hiding (buildReports, bulkImport)
import Distribution.Server.Users.State as State
import qualified Distribution.Server.Cache as Cache
import Distribution.Server.Packages.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Util.Happstack (remainingPath)
import qualified Distribution.Server.BulkImport as BulkImport
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users

import Distribution.Server.Export.ServerParts (export)

import Distribution.Server.Auth.Types (PasswdPlain(..))

import System.FilePath ((</>))
import qualified System.FilePath.Posix as Posix (joinPath, splitExtension)
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist )
import Control.Concurrent.MVar (MVar)
import Control.Monad.Trans
import Control.Monad (when,msum,mzero)
import Data.ByteString.Lazy.Char8 (ByteString)
import Network.URI
         ( URIAuth(URIAuth) )
import Network.BSD
         ( getHostName )


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

   lookupUsers users names = mapM lookupUser names
    where lookupUser name = case Users.lookupName name users of
           Nothing -> Left $ "User " ++ show name ++ " not found"
           Just uid -> Right uid

importTar :: Server -> ByteString -> IO (Maybe String)
importTar (Server store _ _ cache host _) tar = do
  res <- Import.importTar store tar
  case res of
    Nothing -> updateCache cache host
    Just _err -> return ()
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
  res <- update $ AddUser userName userAuth

  case res of
    Just user -> update $ AddToGroup Administrator user
    _ -> fail "Failed to create admin user!"

  updateCache cache host

-- Support the same URL scheme as the first version of hackage.
legacySupport :: ServerPart Response
legacySupport
    = msum [ legPackagesPath, legCgiScripts]

 where

   -- the old "packages/archive" directory
   legPackagesPath
       = dir "packages" $
         dir "archive" $
         msum
         [ path $ \name ->
           path $ \version ->
           let pkgid = PackageIdentifier {pkgName = name, pkgVersion = version}
           in msum
                  [ let dirName = display pkgid ++ ".tar.gz"
                    in dir dirName $ methodSP GET $
                    movedPermanently (packageTarball pkgid) (toResponse "")

                  , let fileName = display name ++ ".cabal"
                    in dir fileName $ methodSP GET $
                    movedPermanently (cabalPath pkgid) (toResponse "")

                  , dir "doc" $ dir "html" $ remainingPath $ \paths ->
                    let doc = Posix.joinPath paths
                    in methodSP GET $
                       movedPermanently (docPath pkgid doc) (toResponse "")
                  ]         

         , dir "package" $ path $ \fileName -> methodSP GET $
           do packageStr <- splitExtensions fileName [".gz", ".tar"]
              case simpleParse packageStr of
                Just pkgid -> movedPermanently (packageTarball pkgid) $ toResponse ""
                _ -> mzero

         , dir "00-index.tar.gz" $
           methodSP GET $
           movedPermanently "/00-index.tar.gz" (toResponse "")
         ]

   -- the old "cgi-bin/hackage-scripts" directory
   legCgiScripts =
       dir "cgi-bin" $
       dir "hackage-scripts" $
       msum
       [ dir "check-pkg" $
         methodSP POST $
         movedPermanently "/check" $
         toResponse ""
       , dir "upload-pkg" $
         methodSP POST $
         movedPermanently "/upload" $
         toResponse ""
       , dir "package" $ path $
         \packageId ->
         methodSP GET $
         movedPermanently ("/package/" ++ display (packageId :: PackageId)) $
         toResponse ""
       ]


   packageTarball :: PackageId -> String
   packageTarball pkgid
       = "/package/" ++ display pkgid ++ ".tar.gz"

   docPath pkgid file = "/package/" ++ display pkgid ++ "/"
                        ++ "documentation/" ++ file

   cabalPath pkgid = "/package/" ++ display pkgid ++ "/"
                     ++ display (packageName pkgid) ++ ".cabal"

   splitExtensions fp [] = return fp
   splitExtensions fp (x:xs) =
       case Posix.splitExtension fp of
         (fp', ext) | ext == x -> splitExtensions fp' xs
         _ -> mzero


impl :: Server -> [ServerPartT IO Response]
impl (Server store static _ cache host _) =
  [ legacySupport
  , dir "packages" $
    methodSP GET $
             ok . Cache.packagesPage =<< Cache.get cache
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
  , dir "admin" $ admin store
  , dir "check" checkPackage
  , dir "htpasswd" $ msum
      [ changePassword ]
  , dir "distro" distros
  , fileServe ["hackage.html"] static
  ]


-- Top level server part for administrative actions under the "admin"
-- directory
admin :: BlobStorage -> ServerPart Response
admin storage = do

  guardAuth [Administrator]

  msum
   [ dir "users" userAdmin
   , dir "export.tar.gz" (export storage)
   , adminDist
   ]
