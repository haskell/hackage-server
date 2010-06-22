module Distribution.Server.Packages.ServerParts (
    serveBuildReports,

    updateCache,
    stateToCache,
    handlePackageById,
    checkPackage,
    buildReports,
  ) where

import Distribution.Package (PackageId)
import Distribution.Text (simpleParse, display)
import Happstack.Server hiding (port, host)
import Happstack.State hiding (Version)

--import Distribution.Server.ServerParts (guardAuth)
import Distribution.Server.Instances ()

import Distribution.Server.Packages.State as State
import Distribution.Server.Users.State as State

--import qualified Distribution.Server.Util.TarIndex as TarIndex
--import qualified Distribution.Server.Util.Serve as TarIndex

import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
--import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.Recent  as Pages
import qualified Distribution.Server.Pages.BuildReports as Pages
import qualified Distribution.Server.Packages.Index as Packages.Index (write)
import qualified Distribution.Server.Packages.Unpack as Upload (unpackPackage)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
--import Distribution.Server.Util.Serve (serveTarball)
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports

--import Distribution.Server.Feature
import Distribution.Server.Types

import qualified Distribution.Server.Users.Users as Users

import Distribution.Server.Features.Core (withPackageId)

import Data.Maybe
import Control.Monad.Trans
import Control.Monad (msum, mzero)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock
import Network.URI (URIAuth)

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Codec.Compression.GZip as GZip

{-packagePagesFeature :: HackageModule
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
}-}
-- "/package/:package/candidate", "/package/:package/candidate/:cabal", "/package/:package/candidate/:tarball"

serveBuildReports :: Config -> DynamicPath -> ServerPart Response

serveBuildReports _ dpath = withPackageId dpath $ \pkgid -> do
    state <- query GetPackagesState
    reports <- query GetBuildReports
    case PackageIndex.lookupPackageId (packageList state) pkgid of
        Nothing -> notFound $ toResponse "No such package"
        Just _  -> do
            let pkgReports = BuildReports.lookupPackageReports reports pkgid
            ok $ toResponse $ Resource.XHtml $ Pages.buildReportSummary pkgid pkgReports


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
handlePackageById _ _ = []
{-handlePackageById store pkgid = 
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
               update $ TarIndex.DropIndex blob-}


checkPackage :: ServerPart Response
checkPackage = methodSP POST $ do
    input <- getDataFn (lookInput "package") >>= maybe mzero return
    let res = Upload.unpackPackage (fromMaybe "noname" $ inputFilename input) (inputValue input)
    case res of
         Left err -> return $ toResponse err
         Right (_,[]) -> return $ toResponse "Check succeeded, no warnings."
         Right (_,warn) -> return . toResponse . unlines $ "Check succeeded with warnings.\n" : warn


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
