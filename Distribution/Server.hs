module Distribution.Server (
   Server(),
   initialise,
   Config(..),
   defaultConfig,
   run,
   bulkImport,
   initState,
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
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth
import Distribution.Server.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.PackageAdmin as Pages
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

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Groups

import Distribution.Server.Auth.Types (PasswdPlain(..))


import System.FilePath ((</>))
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist )
import System.Random (newStdGen)
import Control.Concurrent.MVar (MVar)
import Data.Maybe; import Data.Version
import Control.Monad.Trans
import Control.Monad (when,msum,mzero,liftM2,mplus)
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

  Cache.put cache =<< stateToCache host =<< query GetPackagesState

  return badLogEntries

 where
   importAdminsList :: Maybe String -> Maybe [Users.UserName]
   importAdminsList
       = maybe Nothing (Just . map Users.UserName . lines)

   lookupUsers users names = mapM lookup names
    where lookup name = case Users.lookupName name users of
           Nothing -> Left $ "User " ++ show name ++ " not found"
           Just uid -> Right uid

-- An alternative to a bulk import.
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
legacySupport = msum
    [ path $ \name -> msum
      [ path $ \version -> msum
        [ let dirName = display pkgid ++ ".tar.gz"
              pkgid = PackageIdentifier {pkgName = PackageName name, pkgVersion = version}
          in dir dirName $ msum
             [ methodSP GET $ do
                 movedPermanently ("/packages/"++ display pkgid ++ "/") (toResponse "")
             ]
        ]]
    , dir "00-index.tar.gz" $ msum
      [ methodSP GET $
               movedPermanently "/00-index.tar.gz" (toResponse "")
      ]
    ]

handlePackageById :: BlobStorage -> PackageIdentifier -> [ServerPart Response]
handlePackageById store pkgid = 
  [ withPackage pkgid $ \state pkg pkgs ->
      methodSP GET $
        ok $ toResponse $ Resource.XHtml $
          Pages.packagePage (userDb state) (packageList state) pkg pkgs

  , dir "cabal" $ msum
    [ withPackage pkgid $ \_ pkg _pkgs ->
      methodSP GET $
        ok $ toResponse (Resource.CabalFile (pkgData pkg))
--  , methodSP PUT $ do ...
    ]
  , dir "buildreports" $ msum
    [ methodSP GET $ do
        state <- query GetPackagesState
        case PackageIndex.lookupPackageId (packageList state) pkgid of
          Nothing -> notFound $ toResponse "No such package"
          Just _  -> do
            let reports = BuildReports.lookupPackageReports
                            (State.buildReports state) pkgid
            ok $ toResponse $ Resource.XHtml $
                   Pages.buildReportSummary pkgid reports
    ]
  , dir "documentation" $ msum
    [ withPackage pkgid $ \state pkg _ ->
        methodSP POST $ do
          authGroup <- query $ LookupUserGroups
                       [Trustee, PackageMaintainer (packageName pkg)]
          user <- Auth.hackageAuth (userDb state) (Just authGroup)
          withRequest $ \Request{rqBody = Body body} -> do
              blob <- liftIO $ BlobStorage.add store body
              liftIO $ putStrLn $ "Putting to: " ++ show (display pkgid, blob)
              update $ InsertDocumentation pkgid blob
              seeOther ("/packages/"++display pkgid++"/documentation/") $ toResponse ""
    , require (query $ LookupDocumentation pkgid) $ \blob -> msum
      [ withRequest $ \rq ->
         do tarball <- liftIO $ BlobStorage.fetch store blob
            serveTarball ["index.html"] (display pkgid) rq tarball
      ]
    ]
  , dir "admin" (packageAdmin pkgid)
  ]

packageAdmin :: PackageIdentifier -> ServerPart Response
packageAdmin pkgid =
    withPackage pkgid $ \state pkg _ -> do
    guardAuth [Trustee, PackageMaintainer (packageName pkg)]
    msum
     [ methodSP GET $ do
        maintainers <- packageMaintainers pkg
        ok $ toResponse $ Resource.XHtml $
           Pages.packageAdminPage maintainers pkg
     , adminPost
     ]

 where
   packageMaintainers pkg =
    do
      group <- query $ LookupUserGroup (PackageMaintainer (packageName pkg))
      let uids = Groups.enumerate group
      state <- query $ GetPackagesState
      return $ lookupUserNames (userDb state) uids

   -- this needs work, as it won't skip over deleted users.
   lookupUserNames users = map (Users.idToName users)

   lookUser = do
     reqData <- getDataFn (look "user")
     case reqData of
       Nothing -> return Nothing
       Just userString ->
           query $ LookupUserName (Users.UserName userString)

   adminPost :: ServerPart Response
   adminPost = msum
       [ dir "addMaintainer" $ methodSP POST $ do
           userM <- lookUser
           case userM of
             Nothing -> ok $ toResponse "Not a valid user!"
             Just user -> do
                 update $ AddToGroup (PackageMaintainer (packageName pkgid)) user
                 ok $ toResponse "Ok!"
       , dir "removeMaintainer" $ methodSP POST $ do
           userM <- lookUser
           case userM of
             Nothing -> ok $ toResponse "Not a valid user!"
             Just user -> do
                 update $ RemoveFromGroup (PackageMaintainer (packageName pkgid)) user
                 ok $ toResponse "Ok!"
       ]


withPackage :: PackageIdentifier -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response
withPackage pkgid action = do
  state <- query GetPackagesState
  let index = packageList state
  case PackageIndex.lookupPackageName index (packageName pkgid) of
    []   -> anyRequest $ notFound $ toResponse "No such package in package index"
    pkgs  | pkgVersion pkgid == Version [] []
         -> action state pkg pkgs
      where pkg = maximumBy (comparing packageVersion) pkgs

    pkgs -> case listToMaybe [ pkg | pkg <- pkgs, packageVersion pkg
                                           == packageVersion pkgid ] of
      Nothing  -> anyRequest $ notFound $ toResponse "No such package version"
      Just pkg -> action state pkg pkgs

servePackage :: BlobStorage -> String -> ServerPart Response
servePackage store pkgIdStr = methodSP GET $ do
    let (pkgVer,t) = splitAt (length pkgIdStr - length ext) pkgIdStr
        pkgid = fromReqURI pkgVer
    case pkgid of
        Just p  -> serve p t
        Nothing -> notFound $ toResponse "Not a valid package-version format"
  where
    ext = ".tar.gz"
    serve pkgId t | t /= ext = notFound $ toResponse "No such package in store"
                  | otherwise = withPackage pkgId $ \_ pkg _ ->
        case pkgTarball pkg of
            Nothing     -> notFound $ toResponse "No tarball available"
            Just blobId -> do
                  file <- liftIO $ BlobStorage.fetch store blobId
                  ok $ toResponse $ 
                    Resource.PackageTarball file blobId (pkgUploadTime pkg)


checkPackage :: ServerPart Response
checkPackage = methodSP POST $ do
    input <- getDataFn (lookInput "package") >>= maybe mzero return
    let res = Upload.unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input)
    case res of
         Left err -> return $ toResponse err
         Right (_,[]) -> return $ toResponse "Check succeeded, no warnings."
         Right (_,warn) -> return . toResponse . unlines $ "Check succeeded with warnings.\n" : warn


uploadPackage :: BlobStorage -> Cache.Cache -> URIAuth -> ServerPart Response
uploadPackage store cache host =
  methodSP POST $ do
    withDataFn (lookInput "package") $ \input ->
          let {-withEntry = do str <- look "entry"
                               case simpleParse str of
                                 Nothing -> fail "no parse"
                                 Just x  -> return (x::UploadLog.Entry)-}
              fileName    = (fromMaybe "noname" $ inputFilename input)
              fileContent = inputValue input
          in msum [ upload fileName fileContent ]
  where
    upload name content = do
      --TODO: check if the package is in the index already, before we embark
      -- on writing the tarball into the store and validating etc.
      res <- liftIO $ BlobStorage.addWith store content
                        (Upload.unpackPackage name)
      case res of
        Left  err -> badRequest $ toResponse err
        Right (((pkg, pkgStr), warnings), blobId) -> do
          state <- query GetPackagesState

          let pkgExists = packageExists state pkg
          user <- uploadUser state pkg

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
             then do
               -- Update the package maintainers group.
               if not pkgExists then
                   update $ AddToGroup (PackageMaintainer (packageName pkg)) realUser
                else return ()
               
               updateCache cache host
               ok $ toResponse $ unlines warnings
             else forbidden $ toResponse "Package already exists."

    uploadUser state pkg = do
          group <- uploadUserGroup state pkg
          Auth.hackageAuth (userDb state) group

    -- Auth group for uploading a package.
    -- A new package may be uped by anyone
    -- An existing package may only be uploaded by a maintainer of
    -- that package or a trustee.
    uploadUserGroup state pkg =          
          if packageExists state pkg
             then Just `fmap` (query $ LookupUserGroups [Trustee, PackageMaintainer (packageName pkg)])
             else return Nothing

    packageExists state pkg = not . null $ PackageIndex.lookupPackageName  (packageList state) (packageName pkg)

data ChangePassword = ChangePassword { first, second :: String } deriving (Eq, Ord, Show)
instance FromData ChangePassword where
	fromData = liftM2 ChangePassword (look "new" `mplus` return "") (look "new2" `mplus` return "")

changePassword :: ServerPart Response
changePassword =
  methodSP POST $ do
    state <- query GetPackagesState
    let users = userDb state
    uid <- Auth.hackageAuth users Nothing
    let name = Users.idToName users uid
    pwd <- getData >>= maybe (return $ ChangePassword "not" "valid") return
    if (first pwd == second pwd && first pwd /= "")
        then do let passwd = PasswdPlain (first pwd)
                auth <- newPasswd passwd
                update $ ReplaceUserAuth uid auth
                ok $ toResponse "Password Changed"
        else forbidden $ toResponse "Copies of new password do not match or is an invalid password (ex: blank)"

buildReports :: BlobStorage -> [ServerPart Response]
buildReports store =
  [ path $ \reportId -> msum
    [ methodSP GET $ do
        reports <- return . State.buildReports =<< query GetPackagesState
        case BuildReports.lookupReport reports reportId of
          Nothing     -> notFound $ toResponse "No such report"
          Just report ->
            ok $ toResponse $ Pages.buildReportDetail report reportId buildLog
            where
              buildLog = BuildReports.lookupBuildLog reports reportId

    , dir "buildlog" $ msum
      [ methodSP GET $ do
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
              = [ methodSP GET $
                  do userGroup <- query $ LookupUserGroup groupName
                     userNames <- query $ ListGroupMembers userGroup
                     ok $ toResponse $ unlines (map display userNames)
                , methodSP PUT $ ...
                , methodSP DELETE $ ...
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
  [ dir "packages" $ msum [ path $ msum . handlePackageById store
                          , legacySupport
                          , methodSP GET $ do
                            cacheState <- Cache.get cache
                            ok $ Cache.packagesPage cacheState
                        ]
  , dir "package" (path $ servePackage store)
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
  , dir "admin" $ msum
      [ admin cache host ]
  , dir "check" checkPackage
  , dir "htpasswd" $ msum
      [ changePassword ]
  ,
  fileServe ["hackage.html","admin.html"] static
  ]

guardAuth :: [GroupName] -> ServerPart ()
guardAuth gNames = do
  state <- query GetPackagesState
  group <- query $ LookupUserGroups gNames
  _ <- Auth.hackageAuth (userDb state) (Just group)
  return ()

-- Top level server part for administrative actions under the "admin"
-- directory
admin :: Cache.Cache -> URIAuth -> ServerPart Response
admin cache host = do

  guardAuth [Administrator]

  msum
   [ dir "users" (userAdmin cache host)
   ]

-- Assumes that the user has already been autheniticated
-- and has proper permissions
userAdmin :: Cache.Cache -> URIAuth -> ServerPart Response
userAdmin cache host
    = msum
      [ dir "add" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn lookUserNamePasswords
              case reqData of
                Nothing -> ok $ toResponse "try to fill out all the fields"
                Just (userName, pwd1, pwd2) ->

                    adminAddUser cache host (Users.UserName userName)
                           (Auth.PasswdPlain pwd1) (Auth.PasswdPlain pwd2)
          ]
      , dir "change-password" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn lookUserNamePasswords
              case reqData of
                Nothing -> ok $ toResponse "try to fill out all the fields"
                Just (userName, pwd1, pwd2) ->

                    adminChangePassword cache host (Users.UserName userName)
                             (Auth.PasswdPlain pwd1) (Auth.PasswdPlain pwd2)
          ]
      -- , dir "disable" $ undefined
      -- , dir "enable" $ undefined
      -- , dir "delete" $ undefined
      , dir "toggle-admin" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn $ do
                userName <- lookUserName
                makeAdmin <- lookRead "admin"
                return (userName, makeAdmin)
              
              case reqData of
                Nothing -> ok $ toResponse "Bad inputs, somehow"
                Just (userName, makeAdmin) ->
                    adminToggleAdmin (Users.UserName userName) makeAdmin
          ]
      ]

 where lookUserNamePasswords = do
         userName <- lookUserName
         pwd1 <- look "password"
         pwd2 <- look "repeat-password"
         return (userName, pwd1, pwd2)

       lookUserName = look "user-name"

adminToggleAdmin :: Users.UserName -> Bool -> ServerPart Response
adminToggleAdmin userName makeAdmin
    = do
  mUser <- query $ LookupUserName userName

  if isNothing mUser then ok $ toResponse "Unknown user name" else do

  let Just user = mUser

  if makeAdmin
   then update $ AddToGroup Administrator user
   else update $ RemoveFromGroup Administrator user

  ok $ toResponse "Success!"


adminChangePassword
    :: Cache.Cache -> URIAuth
    -> Users.UserName -> Auth.PasswdPlain -> Auth.PasswdPlain
    -> ServerPart Response
adminChangePassword _ _ _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminChangePassword cache host userName password _
    = do

  mUser <- query $ LookupUserName userName

  case mUser of
    Nothing -> ok $ toResponse "Unknown user name"
    Just user ->
        do
          auth <- newPasswd password
          result <- update $ ReplaceUserAuth user auth
          updateCache cache host

          ok $ toResponse $
           if result then "Success!"
           else "Failure!"


adminAddUser :: Cache.Cache -> URIAuth
        -> Users.UserName -> Auth.PasswdPlain -> Auth.PasswdPlain
        -> ServerPart Response
adminAddUser _ _ _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminAddUser cache host userName password _
    = do

  userAuth <- newPasswd password
  result <- update $ AddUser userName userAuth  

  case result of
    Nothing -> ok $ toResponse "Failed!"
    Just _  ->
        do
          updateCache cache host
          ok $ toResponse "Ok!"
          
newPasswd :: MonadIO m => Auth.PasswdPlain -> m Auth.PasswdHash
newPasswd pwd =
    do
      gen <- liftIO newStdGen
      return $ Auth.newPasswd gen pwd
