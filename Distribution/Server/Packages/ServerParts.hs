module Distribution.Server.Packages.ServerParts (
    updateCache,
    stateToCache,
    
    handlePackageById,
    servePackage,
    checkPackage,
    uploadPackage,
    buildReports,
  ) where

import Distribution.Package
         ( PackageIdentifier(..), packageName, packageVersion
         , Package(packageId) )
import Distribution.Text    (simpleParse, display)
import Happstack.Server hiding (port)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import Distribution.Server.Users.ServerParts (guardAuth)

import Distribution.Server.Packages.State as State hiding (buildReports)
import Distribution.Server.Users.State as State

import qualified  Distribution.Server.Packages.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Server.Auth.Basic as Auth
import Distribution.Server.Packages.Types
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

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Groups

import Data.Maybe
import Data.Version
import Control.Monad.Trans
import Control.Monad (msum,mzero,unless)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Time.Clock
import Network.URI
         ( URIAuth )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Codec.Compression.GZip as GZip


--TODO: switch to new cache mechanism:

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
      state <- query GetPackagesState
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
  methodSP POST $
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

          (realUser, realTime) <- do now <- liftIO getCurrentTime
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
               unless pkgExists $
                   update $ AddToGroup (PackageMaintainer (packageName pkg)) realUser
               
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
             then Just `fmap` query (LookupUserGroups [Trustee, PackageMaintainer (packageName pkg)])
             else return Nothing

    packageExists state pkg = not . null $ PackageIndex.lookupPackageName  (packageList state) (packageName pkg)

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


instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

instance FromReqURI Version where
  fromReqURI = simpleParse

instance FromReqURI BuildReports.BuildReportId where
  fromReqURI = simpleParse
