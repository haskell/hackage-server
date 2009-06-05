module Distribution.Server (
   Server(),
   initialise,
   Config(..),
   defaultConfig,
   run,
   bulkImport,
 ) where

import Distribution.Package ( PackageIdentifier(..), Package(packageId)
                            , packageName, packageVersion, PackageName(..) )
import Distribution.Text    (simpleParse, display)
import Happstack.Server hiding (port)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import Distribution.Server.State as State hiding (buildReports, bulkImport)
import qualified  Distribution.Server.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Server.Auth.Basic as Auth
import Distribution.Server.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.Recent  as Pages
import qualified Distribution.Server.Pages.BuildReports as Pages
import qualified Distribution.Server.Packages.Index as Packages.Index (write)
import qualified Distribution.Server.PackageUpload.Unpack as Upload (unpackPackage)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Util.Serve (serveTarball)
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import qualified Distribution.Server.BulkImport as BulkImport
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog

import System.FilePath ((</>))
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist )
import Control.Concurrent.MVar (MVar)
import Data.Maybe; import Data.Version
import Control.Monad.Trans
import Control.Monad (when,msum,mzero)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time.Clock
import Network.URI
         ( URIAuth(URIAuth) )
import Network.BSD
         ( getHostName )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Codec.Compression.GZip as GZip

import Paths_hackage_server (getDataDir)

data Config = Config {
  confHostName  :: String,
  confPortNum   :: Int,
  confStateDir  :: FilePath,
  confStaticDir :: FilePath
}

defaultConfig :: IO Config
defaultConfig = do
  hostName <- getHostName
  dataDir  <- getDataDir
  return Config {
    confHostName  = hostName,
    confPortNum   = 5000,
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

initialise :: Config -> IO Server
initialise (Config hostName portNum stateDir staticDir) = do

  exists <- doesDirectoryExist staticDir
  when (not exists) $
    fail $ "The directory '" ++ staticDir ++ "' does not exist. It should "
        ++ "contain the hackage server's static html and other files."

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
    happsStateDir = stateDir </> "db"
    blobStoreDir  = stateDir </> "blobs"
    hostURI       = URIAuth "" hostName portStr
      where portStr | portNum == 80 = ""
                    | otherwise     = ':' : show portNum

hackageEntryPoint :: Proxy HackageEntryPoint
hackageEntryPoint = Proxy

run :: Server -> IO ()
run server = simpleHTTP conf $ msum (impl server)
  where
    conf = nullConf { Happstack.Server.port = serverPort server }

bulkImport :: Server
           -> ByteString -> String -> Maybe ByteString -> Maybe String
           -> IO [UploadLog.Entry]
bulkImport (Server store _ _ cache host _)
           indexFile logFile archiveFile htPasswdFile = do
  pkgIndex  <- either fail return (BulkImport.importPkgIndex indexFile)
  uploadLog <- either fail return (BulkImport.importUploadLog logFile)
  tarballs  <- BulkImport.importTarballs store archiveFile
  accounts  <- either fail return (BulkImport.importUsers htPasswdFile)

  (pkgsInfo, users, badLogEntries) <- either fail return
    (BulkImport.mergePkgInfo pkgIndex uploadLog tarballs accounts)

  update $ BulkImport pkgsInfo users
  Cache.put cache =<< stateToCache host =<< query GetPackagesState

  return badLogEntries

updateCache :: MonadIO m => Cache.Cache -> URIAuth -> m ()
updateCache cache host
    = liftIO (Cache.put cache =<< stateToCache host =<< query GetPackagesState)

stateToCache :: URIAuth -> PackagesState -> IO Cache.State
stateToCache host state = getCurrentTime >>= \now -> return
  Cache.State {
    Cache.packagesPage  = toResponse $ Resource.XHtml $
                            Pages.packageIndex index,
    Cache.indexTarball  = GZip.compress $ Packages.Index.write users index,
    Cache.recentChanges = toResponse $ Resource.XHtml $
                            Pages.recentPage users recentChanges,
    Cache.packagesFeed  = toResponse $
                            Pages.recentFeed users host now recentChanges
  }
  where index = packageList state
        users = userDb state
        recentChanges = reverse $ sortBy (comparing pkgUploadTime) (PackageIndex.allPackages index)

-- Support the same URL scheme as the first version of hackage.
legacySupport :: ServerPart Response
legacySupport = mzero
    -- msum
    -- [ path $ \name -> msum
    --   [ path $ \version -> msum
    --     [ let dirName = display pkgid ++ ".tar.gz"
    --           pkgid = PackageIdentifier {pkgName = PackageName name, pkgVersion = version}
    --       in dir dirName msum
    --          [ method GET $ do
    --              movedPermanently ("/packages/"++display pkgid++"/tarball") (toResponse "")
    --          ]
    --     ]]
    -- , dir "00-index.tar.gz" msum
    --   [ method GET $
    --            movedPermanently "/00-index.tar.gz" (toResponse "")
    --   ]
    -- ]

handlePackageById :: BlobStorage -> PackageIdentifier -> [ServerPart Response]
handlePackageById store pkgid = 
  [ withPackage $ \state pkg pkgs ->
      method GET $
        ok $ toResponse $ Resource.XHtml $
          Pages.packagePage (userDb state) (packageList state) pkg pkgs

--   , dir "cabal" msum
--     [ withPackage $ \_ pkg _pkgs ->
--       method GET $
--         ok $ toResponse (Resource.CabalFile (pkgData pkg))
-- --  , method PUT $ do ...
--     ]

--   , dir "tarball" msum
--     [ withPackage $ \_ pkg _pkgs -> do
--         method GET $
--           case pkgTarball pkg of
--             Nothing -> notFound $ toResponse "No tarball available"
--             Just blobId -> do
--               file <- liftIO $ BlobStorage.fetch store blobId
--               ok $ toResponse $
--                 Resource.PackageTarball file blobId (pkgUploadTime pkg)
--     ]
--   , dir "buildreports" msum
--     [ method GET $ do
--         state <- query GetPackagesState
--         case PackageIndex.lookupPackageId (packageList state) pkgid of
--           Nothing -> notFound $ toResponse "No such package"
--           Just _  -> do
--             let reports = BuildReports.lookupPackageReports
--                             (State.buildReports state) pkgid
--             ok $ toResponse $ Resource.XHtml $
--                    Pages.buildReportSummary pkgid reports
--     ]
--   , dir "documentation" msum
--     [ withPackage $ \state pkg pkgs ->
--         methodSP POST $ do
--           authGroup <- query $ LookupUserGroups [Trustee, PackageMaintainer (pkgName pkgid)]
--           user <- Auth.hackageAuth (userDb state) Nothing -- (Just authGroup)
--           withRequest $ \Request{rqBody = Body body} -> do
--               blob <- liftIO $ BlobStorage.add store body
--               liftIO $ putStrLn $ "Putting to: " ++ show (display pkgid, blob)
--               update $ InsertDocumentation pkgid blob
--               seeOther ("/packages/"++display pkgid++"/documentation/") $ toResponse ""
--     , require (query $ LookupDocumentation pkgid) $ \blob -> msum
--       [ withRequest $ \rq ->
--          do tarball <- liftIO $ BlobStorage.fetch store blob
--             serveTarball ["index.html"] (display pkgid) rq tarball
--       ]
--     ]
  ]
  
  where
    withPackage action = do
      state <- query GetPackagesState
      let index = packageList state
      case PackageIndex.lookupPackageName index (packageName pkgid) of
        []   -> anyRequest $ notFound $ toResponse "No such package"
        pkgs  | pkgVersion pkgid == Version [] []
             -> action state pkg pkgs
          where pkg = maximumBy (comparing packageVersion) pkgs
 
        pkgs -> case listToMaybe [ pkg | pkg <- pkgs, packageVersion pkg
                                               == packageVersion pkgid ] of
          Nothing  -> anyRequest $ notFound $ toResponse "No such package version"
          Just pkg -> action state pkg pkgs

uploadPackage :: BlobStorage -> Cache.Cache -> URIAuth -> ServerPart Response
uploadPackage store cache host =
  methodSP POST $ do
    state <- query GetPackagesState
    user <- Auth.hackageAuth (userDb state) Nothing
    withDataFn (lookInput "package") $ \input ->
          let {-withEntry = do str <- look "entry"
                               case simpleParse str of
                                 Nothing -> fail "no parse"
                                 Just x  -> return (x::UploadLog.Entry)-}
              fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in msum [ anyRequest $ upload user fileName fileContent ]
  where
    upload user name content = do
      --TODO: check if the package is in the index already, before we embark
      -- on writing the tarball into the store and validating etc.
      res <- liftIO $ BlobStorage.addWith store content
                        (Upload.unpackPackage name)
      case res of
        Left  err -> badRequest $ toResponse err
        Right (((pkg, pkgStr), warnings), blobId) -> do
          (realUser, realTime) <- do now <- liftIO $ getCurrentTime
                                     return (user,now)
          success <- update $ Insert PkgInfo {
            pkgInfoId     = packageId pkg,
            pkgDesc       = pkg,
            pkgData       = pkgStr,
            pkgTarball    = Just blobId,
            pkgUploadTime = realTime,
            pkgUploadUser = realUser,
            pkgUploadOld  = []
          }
          if success
             then do updateCache cache host
                     ok $ toResponse $ unlines warnings
             else forbidden $ toResponse "Package already exists."

buildReports :: BlobStorage -> [ServerPart Response]
buildReports store =
  [ path $ \reportId -> msum
    [ method GET $ do
        reports <- return . State.buildReports =<< query GetPackagesState
        case BuildReports.lookupReport reports reportId of
          Nothing     -> notFound $ toResponse "No such report"
          Just report ->
            ok $ toResponse $ Pages.buildReportDetail report reportId buildLog
            where
              buildLog = BuildReports.lookupBuildLog reports reportId

    , dir "buildlog" $ msum
      [ method GET $ do
          reports <- return . State.buildReports =<< query GetPackagesState
          case BuildReports.lookupBuildLog reports reportId of
            Nothing -> notFound $ toResponse "No build log available"
            Just (BuildReports.BuildLog blobId) -> do
              file <- liftIO $ BlobStorage.fetch store blobId
              ok $ toResponse $
                Resource.BuildLog file

      , methodSP PUT $ withRequest $ \Request { rqBody = Body body } -> do
          reports <- return . State.buildReports =<< query GetPackagesState
          case BuildReports.lookupReport reports reportId of
            Nothing -> notFound $ toResponse "No such report"
            Just _  -> do
              --FIXME: authorisation, depending on report id
              blobId <- liftIO $ BlobStorage.add store body
              update $ AddBuildLog reportId (BuildReports.BuildLog blobId)
              setResponseCode 204
              return $ toResponse ""
      ]
    ]
  , methodSP POST $ withRequest $ \Request { rqBody = Body body } ->
      case BuildReport.parse (BS.Char8.unpack body) of
        Left err -> badRequest $ toResponse err
        Right report -> do
          reportId <- update $ AddReport report
          seeOther ("/buildreports/"++display reportId) $
            toResponse ""
  ]

{-
{-
/groups/pkg/packageName
/groups/admin
/groups/trustee
-}
groupInterface :: [ServerPart Response]
groupInterface =
    [ dir "pkg"
      [ path $ \pkgName -> groupMethods (PackageMaintainer (PackageName pkgName)) ]
    , dir "admin" (groupMethods Administrator)
    , dir "trustee" (groupMethods Trustee)
    ]
    where groupMethods groupName
              = [ method GET $
                  do userGroup <- query $ LookupUserGroup groupName
                     userNames <- query $ ListGroupMembers userGroup
                     ok $ toResponse $ unlines (map display userNames)
                , method PUT $ ...
                , method DELETE $ ...
                ]
-}


instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

instance FromReqURI Version where
  fromReqURI = simpleParse

instance FromReqURI BuildReports.BuildReportId where
  fromReqURI = simpleParse

impl :: Server -> [ServerPartT IO Response]
impl (Server store static _ cache host _) =
  [
--   [ dir "packages" msum [ msum $ path $ handlePackageById store
--                         , legacySupport
--                         , method GET $ do
--                             cacheState <- Cache.get cache
--                             ok $ Cache.packagesPage cacheState
--                         ]
--   , dir "buildreports" msum (buildReports store)
-- --  , dir "groups" (groupInterface)
--   , dir "recent.rss" msum
--       [ method GET $ ok . Cache.packagesFeed =<< Cache.get cache ]
--   , dir "recent.html" msum
--       [ method GET $ ok . Cache.recentChanges =<< Cache.get cache ]
--   , dir "upload" msum
--       [ uploadPackage store cache host ]
--   , dir "00-index.tar.gz" msum
--       [ method GET $ do
--           cacheState <- Cache.get cache
--           ok $ toResponse $ Resource.IndexTarball (Cache.indexTarball cacheState)
--       ]
--   ,
  fileServe ["hackage.html"] static
  ]
