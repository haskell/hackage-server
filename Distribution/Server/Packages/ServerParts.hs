module Distribution.Server.Packages.ServerParts (
    withPackageId,
    withPackagePath,
    withPackage,
    servePackageTarball,
    serveCabalFile,

    packagePagesFeature,
    updateCache,
    stateToCache,
    handlePackageById,
    servePackage,
    checkPackage,
    buildReports,
  ) where

import Distribution.Package (PackageIdentifier(..), PackageId, packageName, packageVersion , Package(packageId))
import Distribution.Text    (simpleParse, display)
import Happstack.Server hiding (port, host)
import Happstack.State hiding (Version)

--import Distribution.Server.ServerParts (guardAuth)
import Distribution.Server.Instances ()

import Distribution.Server.Packages.State as State
import Distribution.Server.Users.State as State

import qualified Distribution.Server.Util.TarIndex as TarIndex
import qualified Distribution.Server.Util.Serve as TarIndex

import qualified  Distribution.Server.Packages.State as State
import qualified  Distribution.Server.Distributions.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.PackageIndex as PackageIndex
import qualified Distribution.Server.Auth.Basic as Auth
import Distribution.Server.Packages.Types (PkgInfo(..), pkgUploadTime)
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.Recent  as Pages
import qualified Distribution.Server.Pages.BuildReports as Pages
import qualified Distribution.Server.Packages.Index as Packages.Index (write)
import qualified Distribution.Server.Packages.Unpack as Upload (unpackPackage)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Util.Serve (serveTarball)
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports

--import Distribution.Server.Resource
import Distribution.Server.Feature
import Distribution.Server.Types
--import Distribution.Server.Hook

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Groups
import Distribution.Server.Users.Group (UserGroup(..))

import Data.Maybe
import Data.Version
import Control.Monad.Trans
import Control.Monad (msum, mzero, guard, unless)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Time.Clock
import Network.URI (URIAuth)
import System.FilePath.Posix ((</>))

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Codec.Compression.GZip as GZip

packagePagesFeature :: HackageModule
packagePagesFeature = HackageModule {
    featureName = "pkgpage",
    -- todo: add checking
    resources   = [] {-(resourceAt "/packages/") { resourceGet = Just serveIndexPage, resourcePost = Just uploadPackageTarball }
                  , (resourceAt "/packages/index.tar.gz") { resourceGet = Just serveIndexTarball }
                  , (resourceAt "/package/:package") { resourceGet = Just servePackagePage, resourceDelete = Nothing, resourcePut = Nothing }
                  , (resourceAt "/package/:package/:cabal") { resourceGet = Just serveCabalFile, resourcePut = Nothing }
                  , (resourceAt "/package/:package/:tarball") { resourceGet = Just servePackageTarball, resourcePut = Nothing, resourceDelete = Nothing }
                  , (resourceAt "/package/:package/doc/:doctree")
                  , (resourceAt "/package/:package/buildreports/") { resourceGet = Just serveBuildReports }
                  ] ++ makeGroupResources (trunkAt "/package/:package/maintainers") maintainersGroup -},
    dumpBackup    = return [],
    restoreBackup = Nothing
}
-- "/package/:package/candidate", "/package/:package/candidate/:cabal", "/package/:package/candidate/:tarball"

maintainersGroup :: DynamicPath -> IO (Maybe UserGroup)
maintainersGroup dpath = case fmap pkgName (simpleParse =<< lookup "package" dpath) of
  Nothing -> return Nothing
  Just name -> do 
    pkgstate <- query GetPackagesState
    case PackageIndex.lookupPackageName (packageList pkgstate) name of
      []   -> return Nothing
      pkgs -> do
        let pkgInfo = maximumBy (comparing packageVersion) pkgs
        return . Just $ UserGroup {
            groupDesc = Pages.maintainerDescription pkgInfo,
            queryUserList = query $ GetPackageMaintainers name,
            addUserList = update . AddPackageMaintainer name,
            removeUserList = update . RemovePackageMaintainer name
        }

serveBuildReports, serveIndexPage, serveIndexTarball, servePackageTarball,
    servePackagePage, serveCabalFile :: Config -> DynamicPath -> ServerPart Response

serveBuildReports config dpath = withPackageId dpath $ \pkgid -> do
    state <- query GetPackagesState
    buildReports <- query GetBuildReports
    case PackageIndex.lookupPackageId (packageList state) pkgid of
        Nothing -> notFound $ toResponse "No such package"
        Just _  -> do
            let reports = BuildReports.lookupPackageReports
                            buildReports pkgid
            ok $ toResponse $ Resource.XHtml $
                   Pages.buildReportSummary pkgid reports

serveIndexPage config dpath = do
    cacheState <- Cache.get (serverCache config)
    ok $ Cache.packagesPage cacheState

uploadPackageTarball :: Config -> DynamicPath -> ServerPart Response
uploadPackageTarball config dpath = uploadPackage config

serveIndexTarball config dpath = do
    cacheState <- Cache.get (serverCache config)
    ok $ toResponse $ Resource.IndexTarball (Cache.indexTarball cacheState)

servePackageTarball config dpath = withPackageId dpath $ \pkgid ->
           require (return $ lookup "tarball" dpath) $ \tarball -> do
    -- FIXME: more accurate versioning. currently /package/foo-1.2/bar-3.14.tar.gz is possible
    servePackage (serverStore config) tarball
   
servePackagePage config dpath = withPackagePath dpath $ \state pkg pkgs -> do
    let pkgid = pkgInfoId pkg
    distributions <- query $ State.PackageStatus (packageName pkg)
    hasDocs       <- query $ State.HasDocumentation (packageId pkg)
    let docURL | hasDocs   = Just $ "/package" </> display pkgid </> "documentation"
               | otherwise = Nothing
    userDb <- query $ GetUserDb
    ok $ toResponse $ Resource.XHtml $
      Pages.packagePage userDb (packageList state) pkg pkgs distributions docURL

serveCabalFile config dpath = withPackagePath dpath $ \_ pkg _ -> do
    guard (lookup "cabal" dpath == Just (display (packageName pkg) ++ ".cabal"))
    ok $ toResponse (Resource.CabalFile (pkgData pkg))

withPackagePath :: DynamicPath -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response
withPackagePath dpath func = withPackageId dpath $ \pkgid -> withPackage pkgid func

withPackageId :: DynamicPath -> (PackageId -> ServerPart Response) -> ServerPart Response
withPackageId dpath = require (return $ lookup "package" dpath >>= fromReqURI)

--TODO: switch to new cache mechanism: ??
updateCache :: MonadIO m => Config -> m ()
updateCache config = liftIO $ do
    state  <- query GetPackagesState
    userDb <- query GetUserDb
    cacheState <- stateToCache (serverURI config) state userDb
    Cache.put (serverCache config) cacheState

stateToCache :: URIAuth -> PackagesState -> Users.Users -> IO Cache.State
stateToCache host state users = getCurrentTime >>= \now -> return
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
        recentChanges = reverse $ sortBy (comparing (fst . pkgUploadData)) (PackageIndex.allPackages index)

handlePackageById :: BlobStorage -> PackageId -> [ServerPart Response]
handlePackageById store pkgid = 
  [ dir "documentation" $ msum
    [ withPackage pkgid $ \state pkg _ ->
        let resolvedPkgId = packageId pkg in msum

        [ methodSP POST $ do
            requirePackageAuth pkgid
            withRequest $ \Request{rqBody = Body body} -> do
              {-
                The order of operations:
                - Insert new documentation into blob store
                - Generate the new index
                - Drop the index for the old tar-file
                - Link the new documentation to the package
               -}
              blob <- liftIO $ BlobStorage.add store (GZip.decompress body)
              tarIndex <- liftIO $ TarIndex.readTarIndex (BlobStorage.filepath store blob)
              update $ TarIndex.AddIndex blob tarIndex
              liftIO $ putStrLn $ "Putting to: " ++ show (display resolvedPkgId, blob)
              dropOldDocs resolvedPkgId
              update $ InsertDocumentation resolvedPkgId blob
              seeOther ("/package/"++display pkgid++"/documentation/") $ toResponse ""

        , require (query $ LookupDocumentation resolvedPkgId) $ \blob ->
          require (query $ TarIndex.LookupIndex blob)         $ \index -> do
            let tarball = BlobStorage.filepath store blob
            serveTarball ["index.html"] (display $ packageName pkgid) tarball index
        ]
    ]
  ]

 where dropOldDocs pkgId
           = do
         mBlob <- query $ LookupDocumentation pkgId
         case mBlob of
           Nothing -> return ()
           Just blob -> do
               update $ TarIndex.DropIndex blob

withPackage :: PackageId -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response
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
            [] -> notFound $ toResponse "No tarball available"
            ((blobId, _):_) -> do
                  file <- liftIO $ BlobStorage.fetch store blobId
                  ok $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

checkPackage :: ServerPart Response
checkPackage = methodSP POST $ do
    input <- getDataFn (lookInput "package") >>= maybe mzero return
    let res = Upload.unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input)
    case res of
         Left err -> return $ toResponse err
         Right (_,[]) -> return $ toResponse "Check succeeded, no warnings."
         Right (_,warn) -> return . toResponse . unlines $ "Check succeeded with warnings.\n" : warn


uploadPackage :: Config -> ServerPart Response
uploadPackage config =
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
      res <- liftIO $ BlobStorage.addWith (serverStore config) content
                        (Upload.unpackPackage name)
      case res of
        Left  err -> badRequest $ toResponse err
        Right (((pkg, pkgStr), warnings), blobId) -> do
          state  <- query GetPackagesState
          let pkgExists = packageExists state pkg
          user <- uploadingUser state (packageId pkg)
          uploadData <- do now <- liftIO getCurrentTime
                           return (now, user)
          success <- update $ Insert PkgInfo {
            pkgInfoId     = packageId pkg,
            pkgDesc       = pkg,
            pkgData       = pkgStr,
            pkgTarball    = [(blobId, uploadData)], --does this merge properly?
            pkgUploadData = uploadData,
            pkgDataOld    = [] -- what about this?
          }
          if success
             then do
               -- Update the package maintainers group.
               unless pkgExists $ update $ AddPackageMaintainer (packageName pkg) user
               updateCache config
               ok $ toResponse $ unlines warnings
             else forbidden $ toResponse "Package already exists."

    -- Auth group for uploading a package.
    -- A new package may be uped by anyone
    -- An existing package may only be uploaded by a maintainer of
    -- that package or a trustee.
    uploadingUser state pkg =
      if packageExists state pkg
        then requirePackageAuth pkg
        else query GetUserDb >>= \users -> Auth.requireHackageAuth users Nothing Nothing

    packageExists state pkg = not . null $ PackageIndex.lookupPackageName (packageList state) (packageName pkg)

requirePackageAuth :: (MonadIO m, Package pkg) => pkg -> ServerPartT m Users.UserId
requirePackageAuth pkg = do
    userDb  <- query $ GetUserDb
    pkgm    <- query $ GetPackageMaintainers (packageName pkg)
    trustee <- query $ GetHackageTrustees
    let groupSum = Groups.unions [trustee, pkgm]
    Auth.requireHackageAuth userDb (Just groupSum) Nothing

buildReports :: BlobStorage -> [ServerPart Response]
buildReports store =
  [ path $ \reportId -> msum
    [ methodSP GET $ do
        reports <- query GetBuildReports
        case BuildReports.lookupReport reports reportId of
          Nothing     -> notFound $ toResponse "No such report"
          Just report ->
            ok $ toResponse $ Pages.buildReportDetail report reportId buildLog
            where
              buildLog = BuildReports.lookupBuildLog reports reportId

    , dir "buildlog" $ msum
      [ methodSP GET $ do
          reports <- query GetBuildReports
          case BuildReports.lookupBuildLog reports reportId of
            Nothing -> notFound $ toResponse "No build log available"
            Just (BuildReports.BuildLog blobId) -> do
              file <- liftIO $ BlobStorage.fetch store blobId
              ok $ toResponse $
                Resource.BuildLog file

      , methodSP PUT $ withRequest $ \Request { rqBody = Body body } -> do
          reports <- query GetBuildReports
          case BuildReports.lookupReport reports reportId of
            Nothing -> notFound $ toResponse "No such report"
            Just _  -> do
              --FIXME: authorisation, depending on report id
              blobId <- liftIO $ BlobStorage.add store body
              _ <- update $ AddBuildLog reportId (BuildReports.BuildLog blobId)
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

instance FromReqURI BuildReports.BuildReportId where
    fromReqURI = simpleParse
