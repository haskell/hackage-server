module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature
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
import Distribution.Server.Backup.Import (BackupEntry)
import Text.CSV (printCSV, CSV)

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import Distribution.Server.Packages.ServerParts (withPackageId, withPackage, servePackageTarball, serveCabalFile)
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Distribution.Server.Pages.Index as Pages
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.ResourceTypes as Resource

import Distribution.Text (display)
import Control.Monad
import Data.Monoid (mconcat)
import Happstack.Server
import Happstack.State (update, query)
import Text.XHtml.Strict
import Data.Ord (comparing)
import Data.List (sortBy)
import Distribution.Package
import qualified Data.ByteString.Lazy.Char8 as BS

data CoreFeature = CoreFeature {
    coreResource :: CoreResource,
    cacheIndexTarball :: Cache.GenCache BS.ByteString,
    cachePackagesPage :: Cache.GenCache Response,
    -- Updating top-level packages
    -- A Maybe PkgInfo argument might also be desirable.
    packageIndexChange :: HookList (IO ()),
    -- For download counters, although an update for every download doesn't scale well
    tarballDownload    :: HookList (IO ()),
    adminGroup :: UserGroup
}
data CoreResource = CoreResource {
    coreIndexPage    :: Resource,
    coreIndexTarball :: Resource,
    corePackagesPage :: Resource,
    corePackagePage  :: Resource,
    coreCabalFile    :: Resource,
    corePackageTarball :: Resource
}

instance HackageFeature CoreFeature where
    getFeature core = HackageModule
      { featureName = "core"
      , resources   = map ($coreResource core) [coreIndexPage, coreIndexTarball, corePackagesPage, corePackagePage, corePackageTarball, coreCabalFile]
                      -- maybe: makeGroupResources (trunkAt "/users/admins") (adminGroup core)
      , dumpBackup = do
            users    <- query GetUserDb
            packages <- query GetPackagesState
            admins   <- query GetHackageAdmins
            return $ [csvToBackup ["users.csv"] $ usersToCSV users, csvToBackup ["admins.csv"] $ groupToCSV admins] ++ packageEntries packages
      , restoreBackup = Just (mconcat [userBackup, packagesBackup]) -- [adminBackup]
      }
    initHooks core = [runZeroHook (packageIndexChange core)]

csvToBackup :: [String] -> CSV -> BackupEntry
csvToBackup fpath csv = (fpath, BS.pack (printCSV csv))

initCoreFeature :: IO CoreFeature
initCoreFeature = do
    let indexPage config _ = fileServe ["hackage.html"] (serverStaticDir config) -- perhaps have a less 'spritzy' index page
    -- Caches
    thePackages <- Cache.newCache (toResponse ()) (BS.length . rsBody)
    indexTar    <- Cache.newCache (BS.empty) (BS.length)
    -- Hooks
    downHook <- newHookList
    changeHook <- newHookList
    registerHook changeHook $ computeCache thePackages indexTar

    return CoreFeature
      { coreResource = let resource = CoreResource {
            -- the rudimentary HTML resources are for when we don't want an additional HTML feature
            coreIndexPage = (resourceAt "") { resourceGet = [("html", indexPage)] }
          , coreIndexTarball = (resourceAt "/packages/index.tar.gz") { resourceGet = [("tarball", respondCache indexTar Resource.IndexTarball)] }
          , corePackagesPage = (resourceAt "/packages/") { resourceGet = [("html", respondCache thePackages id)] }
          , corePackagePage = (resourceAt "/package/:package") { resourceGet = [("html", basicPackagePage (renderResource $ coreCabalFile resource) (renderResource $ corePackageTarball resource))] }
          , corePackageTarball = (resourceAt "/package/:package/:tarball") { resourceGet = [("tarball", servePackageTarball)] }
          , coreCabalFile  = (resourceAt "/package/:package/:cabal") { resourceGet = [("cabal", serveCabalFile)] }
          } in resource
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
    respondCache :: ToMessage r => Cache.GenCache a -> (a -> r) -> Config -> DynamicPath -> ServerPart Response
    respondCache cache func _ _ = ok . toResponse . func =<< Cache.getCache cache
    computeCache thePackages indexTar = do
        users <- query GetUserDb
        index <- fmap packageList $ query GetPackagesState
        -- TODO: instead of using the complicated pages feature, make a basicPackageIndex function
        Cache.putCache thePackages (toResponse $ Resource.XHtml $ Pages.packageIndex index)
        Cache.putCache indexTar (GZip.compress $ Packages.Index.write users index)

-- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
basicPackagePage :: URIGen -> URIGen -> Config -> DynamicPath -> ServerPart Response
basicPackagePage cabalUrl tarUrl config dpath = withPackageId dpath $ \pkgid -> withPackage pkgid $ \_ pkg pkgs ->
  ok . toResponse $ Resource.XHtml $ showAllP $ sortBy (flip $ comparing packageVersion) pkgs
  where
    showAllP :: [PkgInfo] -> Html
    showAllP pkgs = toHtml [
    	h3 << "Downloads",
    	unordList (map showP pkgs)
     ]
    showP :: PkgInfo -> [Html]
    showP pkgInfo = let pkgId = packageId pkgInfo; pkgStr = display pkgId; tarName = display pkgId ++ ".tar.gz" in [
        toHtml pkgStr,
        unordList $ [
            [renderLink cabalUrl [("package", pkgStr), ("cabal", display (packageName pkgId) ++ ".cabal")] "Package description",
             toHtml " (included in the package)"],
            case pkgTarball pkgInfo of
                [] -> [toHtml "Package not available"];
                _ ->  [renderLink tarUrl [("package", display pkgId), ("tarball", tarName)] tarName,
                       toHtml " (Cabal source package)"]
        ]
     ]
