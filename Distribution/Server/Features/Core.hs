module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,
    withPackage,
    withPackageId,
    withPackagePath,
    withPackageName,
    withPackageTarball,
    basicPackageSection
  ) where

--import Distribution.Server.Users.Resource (makeGroupResources)
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Cache as Cache
import Distribution.Server.Packages.PackageBackup
import Distribution.Server.Users.UserBackup
import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Hook
import Distribution.Server.Backup.Export

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Distribution.Server.Pages.Index as Pages
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.PackageIndex as PackageIndex
import qualified Distribution.Server.Util.BlobStorage as BlobStorage

import Distribution.Text (display)
import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Data.Monoid (mconcat)
import Data.Function (fix)
import Happstack.Server
import Happstack.State (update, query)
import Text.XHtml.Strict (Html, toHtml, unordList, h3, (<<), anchor, href, (!))
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy, find)
import Distribution.Package
import Distribution.Version (Version(..))
import qualified Data.ByteString.Lazy.Char8 as BS

data CoreFeature = CoreFeature {
    coreResource :: CoreResource,
    cacheIndexTarball :: Cache.GenCache BS.ByteString,
    cachePackagesPage :: Cache.GenCache Response,
    -- Updating top-level packages
    -- A Maybe PkgInfo argument might also be desirable.
    packageIndexChange :: HookList (IO ()),
    -- For download counters, although an update for every download doesn't scale well
    tarballDownload    :: HookList (PackageId -> IO ()),
    adminGroup :: UserGroup
}
data CoreResource = CoreResource {
    coreIndexPage    :: Resource,
    coreIndexTarball :: Resource,
    corePackagesPage :: Resource,
    corePackagePage  :: Resource,
    coreCabalFile    :: Resource,
    corePackageTarball :: Resource,
    indexTarballUri   :: String,
    indexPackageUri   :: String -> String,
    corePackageUri :: String -> PackageId -> String,
    coreCabalUri   :: PackageId -> String,
    coreTarballUri :: PackageId -> String
}

instance HackageFeature CoreFeature where
    getFeature core = HackageModule
      { featureName = "core"
      , resources   = map ($coreResource core) [coreIndexPage, coreIndexTarball, corePackagesPage, corePackagePage, corePackageTarball, coreCabalFile]
                      -- maybe: makeGroupResources (trunkAt "/users/admins") (adminGroup core)
      , dumpBackup = Just $ \store -> do
            users    <- query GetUserDb
            packages <- query GetPackagesState
            admins   <- query GetHackageAdmins
            packageEntries <- readExportBlobs store $ indexToAllVersions packages
            return $ packageEntries ++ [csvToBackup ["users.csv"] $ usersToCSV users, csvToBackup ["admins.csv"] $ groupToCSV admins]
      , restoreBackup = Just $ \store -> mconcat [userBackup, packagesBackup store, groupBackup ["admins.csv"] ReplaceHackageAdmins]
      }
    initHooks core = [runZeroHook (packageIndexChange core)]

initCoreFeature :: IO CoreFeature
initCoreFeature = do
    -- Caches
    thePackages <- Cache.newCacheable
    indexTar    <- Cache.newCacheable
    -- Hooks
    downHook <- newHookList
    changeHook <- newHookList
    registerHook changeHook $ computeCache thePackages indexTar

    return CoreFeature
      { coreResource = fix $ \r -> CoreResource {
            -- the rudimentary HTML resources are for when we don't want an additional HTML feature
            coreIndexPage = (resourceAt "/.:format") { resourceGet = [("html", indexPage)] }
          , coreIndexTarball = (resourceAt "/packages/index.tar.gz") { resourceGet = [("tarball", Cache.respondCache indexTar Resource.IndexTarball)] }
          , corePackagesPage = (resourceAt "/packages/.:format") { resourceGet = [("html", Cache.respondCache thePackages id)] }
          , corePackagePage = (resourceAt "/package/:package.:format") { resourceGet = [("html", basicPackagePage r)] }
          , corePackageTarball = (resourceAt "/package/:package/:tarball.tar.gz") { resourceGet = [("tarball", servePackageTarball downHook)] }
          , coreCabalFile  = (resourceAt "/package/:package/:cabal.cabal") { resourceGet = [("cabal", serveCabalFile)] }
          , indexTarballUri = renderResource (coreIndexTarball r) []
          , indexPackageUri = \format -> renderResource (corePackagesPage r) [format]
          , corePackageUri  = \format pkgid -> renderResource (corePackagePage r) [display pkgid, format]
          , coreCabalUri   = \pkgid -> renderResource (coreCabalFile r) [display pkgid, display (packageName pkgid)]
          , coreTarballUri = \pkgid -> renderResource (corePackageTarball r) [display pkgid, display pkgid]
          }
      , cacheIndexTarball  = indexTar
      , cachePackagesPage  = thePackages
      , packageIndexChange = changeHook
      , tarballDownload = downHook
      , adminGroup = UserGroup {
            groupDesc = nullDescription { groupTitle = "Hackage admins", groupEntityURL = "/" },
            queryUserList = query GetHackageAdmins,
            addUserList = update . AddHackageAdmin,
            removeUserList = update . RemoveHackageAdmin
        }
    }
  where
    indexPage config _ = serveFile (const $ return "text/html") (serverStaticDir config ++ "/hackage.html")
    computeCache thePackages indexTar = do
        users <- query GetUserDb
        index <- fmap packageList $ query GetPackagesState
        -- TODO: instead of using the complicated pages feature, make a basicPackageIndex function
        Cache.putCache thePackages (toResponse $ Resource.XHtml $ Pages.packageIndex index)
        Cache.putCache indexTar (GZip.compress $ Packages.Index.write users index)

-- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
basicPackagePage :: CoreResource -> Config -> DynamicPath -> ServerPart Response
basicPackagePage r _ dpath = withPackagePath dpath $ \_ _ pkgs ->
  ok . toResponse $ Resource.XHtml $ showAllP $ sortBy (flip $ comparing packageVersion) pkgs
  where
    showAllP :: [PkgInfo] -> Html
    showAllP pkgs = toHtml [
    	h3 << "Downloads",
    	unordList $ map (basicPackageSection (coreCabalUri r) (coreTarballUri r)) pkgs
     ]

basicPackageSection :: (PackageId -> String) -> (PackageId -> String) -> PkgInfo -> [Html]
basicPackageSection cabalUrl tarUrl pkgInfo = let pkgId = packageId pkgInfo; pkgStr = display pkgId in [
    toHtml pkgStr,
    unordList $ [
        [anchor ! [href (cabalUrl pkgId)] << "Package description",
         toHtml " (included in the package)"],
        case pkgTarball pkgInfo of
            [] -> [toHtml "Package not available"];
            _ ->  [anchor ! [href (tarUrl pkgId)] << (pkgStr ++ ".tar.gz"),
                   toHtml " (Cabal source package)"]
    ]
 ]

withPackage :: PackageId -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response
withPackage pkgid func = do
    state <- query GetPackagesState
    case PackageIndex.lookupPackageName (packageList state) (packageName pkgid) of
        []   -> notFound $ toResponse "No such package in package index"
        pkgs  | pkgVersion pkgid == Version [] [] -> func state (maximumBy (comparing packageVersion) pkgs) pkgs
        pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
            Nothing  -> notFound $ toResponse "No such package version"
            Just pkg -> func state pkg pkgs

withPackagePath :: DynamicPath -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response
withPackagePath dpath func = withPackageId dpath $ \pkgid -> withPackage pkgid func

withPackageId :: DynamicPath -> (PackageId -> ServerPart Response) -> ServerPart Response
withPackageId dpath = require (return $ lookup "package" dpath >>= fromReqURI)

withPackageName :: DynamicPath -> (PackageName -> ServerPart Response) -> ServerPart Response
withPackageName dpath = require (return $ lookup "package" dpath >>= fromReqURI)

withPackageTarball :: DynamicPath -> (PackageId -> ServerPart Response) -> ServerPart Response
withPackageTarball dpath func = withPackageId dpath $ \(PackageIdentifier name version) ->
    require (return $ lookup "tarball" dpath >>= fromReqURI) $ \pkgid@(PackageIdentifier name' version') -> do
    guard $ name == name' && version' /= Version [] [] && (version == version' || version == Version [] [])
    func pkgid
---------------------------------------------

servePackageTarball :: HookList (PackageId -> IO ()) -> Config -> DynamicPath -> ServerPart Response
servePackageTarball hook config dpath = withPackageTarball dpath $ \pkgid -> withPackage pkgid $ \_ pkg _ -> case pkgTarball pkg of
    [] -> notFound $ toResponse "No tarball available"
    ((blobId, _):_) -> do
        file <- liftIO $ BlobStorage.fetch (serverStore config) blobId
        liftIO $ runOneHook hook pkgid
        ok $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

serveCabalFile :: Config -> DynamicPath -> ServerPart Response
serveCabalFile _ dpath = withPackagePath dpath $ \_ pkg _ -> do
    guard (lookup "cabal" dpath == Just (display $ packageName pkg))
    ok $ toResponse (Resource.CabalFile (pkgData pkg))

