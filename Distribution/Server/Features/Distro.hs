module Distribution.Server.Features.Distro (
    DistroFeature(..),
    initDistroFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Features.Users
import Distribution.Server.Resource
import Distribution.Server.Types

import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import Distribution.Server.Distributions.State
import Distribution.Server.Distributions.Types
import Distribution.Server.Distributions.DistroBackup

import Happstack.Server
import Happstack.State
import Data.List (intercalate)
import Distribution.Text (display, simpleParse)
import Control.Monad
import Distribution.Package

data DistroFeature = DistroFeature {
    distroResource   :: DistroResource,
    maintainersGroup :: DynamicPath -> IO (Maybe UserGroup)
}

data DistroResource = DistroResource {
    distroIndexPage :: Resource,
    distroAllPage  :: Resource,
    distroPackage   :: Resource
}

instance HackageFeature DistroFeature where
    getFeature distro = HackageModule
      { featureName = "distro"
      , resources   = map ($distroResource distro) [distroIndexPage, distroAllPage, distroPackage]
      , dumpBackup    = Just $ \_ -> do
            allDist <- query GetDistributions
            let distros  = distDistros allDist
                versions = distVersions allDist
            return $ distroUsersToExport distros:distrosToExport distros versions
      , restoreBackup = Just $ \_ -> distroBackup
      }

initDistroFeature :: Config -> CoreFeature -> UserFeature -> PackagesFeature -> IO DistroFeature
initDistroFeature _ _ users _ = do
    let admins = adminGroup users
    return $ DistroFeature
      { distroResource = DistroResource
          { distroIndexPage = (resourceAt "/distros/.:format") { resourceGet = [("txt", textEnumDistros)], resourcePost = [("", distroNew)] }
          , distroAllPage = (resourceAt "/distro/:distro/.:format") { resourceGet = [("txt", textDistroPkgs)], resourceDelete = [("", distroDelete)] }
          , distroPackage = (resourceAt "/distro/:distro/package/:package.:format") { resourceGet = [("txt", textDistroPkg)], resourcePut = [("", distroPackagePut)], resourceDelete = [("", distroPackageDelete)] }
          }
      , maintainersGroup = \dpath -> case simpleParse =<< lookup "distro" dpath of
            Nothing -> return Nothing
            Just dname -> getMaintainersGroup admins dname
      }
  where
    textEnumDistros _ = fmap (toResponse . intercalate ", " . map display) (query EnumerateDistros)
    textDistroPkgs dpath = withDistroPath dpath $ \dname pkgs -> do
        let pkglines = map (\(name, info) -> display name ++ " at " ++ display (distroVersion info) ++ ": " ++ distroUrl info) $ pkgs
        return $ toResponse (unlines $ ("Packages for " ++ display dname):pkglines)
    textDistroPkg dpath = withDistroPackagePath dpath $ \_ _ info -> return . toResponse $ show info

    -- result: see-other uri, or an error: not authenticated or not found (todo)
    distroDelete dpath = withDistroNamePath dpath $ \distro -> do
        -- authenticate Hackage admins
        -- should also check for existence here of distro here
        update $ RemoveDistro distro
        seeOther ("/distros/") (toResponse ())

    -- result: ok response or not-found error
    distroPackageDelete dpath = withDistroPackagePath dpath $ \dname pkgname info -> do
        -- authenticate distro maintainer
        case info of
            Nothing -> notFound . toResponse $ "Package not found for " ++ display pkgname
            Just {} -> do
                update $ DropPackage dname pkgname
                ok $ toResponse "Ok!"

    -- result: see-other response, or an error: not authenticated or not found (todo)
    distroPackagePut dpath = withDistroPackagePath dpath $ \dname pkgname _ -> lookPackageInfo $ \newPkgInfo -> do
        -- authenticate distro maintainer
        update $ AddPackage dname pkgname newPkgInfo
        seeOther ("/distro/" ++ display dname ++ "/" ++ display pkgname) $ toResponse "Ok!"

    -- result: see-other response, or an error: not authentcated or bad request
    distroNew _ = lookDistroName $ \dname -> do
        success <- update $ AddDistro dname
        if success
            then seeOther ("/distro/" ++ display dname) $ toResponse "Ok!"
            else badRequest $ toResponse "Selected distribution name is already in use"

withDistroNamePath :: DynamicPath -> (DistroName -> ServerPart Response) -> ServerPart Response
withDistroNamePath dpath = require (return $ simpleParse =<< lookup "distro" dpath)

withDistroPath :: DynamicPath -> (DistroName -> [(PackageName, DistroPackageInfo)] -> ServerPart Response) -> ServerPart Response
withDistroPath dpath func = withDistroNamePath dpath $ \dname -> do
    isDist <- query (IsDistribution dname)
    case isDist of
      False -> notFound $ toResponse "Distribution does not exist"
      True -> do
        pkgs <- query (DistroStatus dname)
        func dname pkgs

-- guards on the distro existing, but not the package
withDistroPackagePath :: DynamicPath -> (DistroName -> PackageName -> Maybe DistroPackageInfo -> ServerPart Response) -> ServerPart Response
withDistroPackagePath dpath func = withDistroNamePath dpath $ \dname -> withPackageName dpath $ \pkgname -> do
    isDist <- query (IsDistribution dname)
    case isDist of
      False -> notFound $ toResponse "Distribution does not exist"
      True -> do
        pkgInfo <- query (DistroPackageStatus dname pkgname)
        func dname pkgname pkgInfo

lookPackageInfo :: (DistroPackageInfo -> ServerPart Response) -> ServerPart Response
lookPackageInfo func = do
    mInfo <- getDataFn $ do
        pVerStr <- look "version"
        pUriStr  <- look "uri"
        case simpleParse pVerStr of
            Nothing -> mzero
            Just pVer -> return $ DistroPackageInfo pVer pUriStr
    case mInfo of
        Nothing -> ok $ toResponse "Sorry, something went wrong there"
        Just pInfo -> func pInfo

lookDistroName :: (DistroName -> ServerPart Response) -> ServerPart Response
lookDistroName func = withDataFn (look "distro") $ \dname -> case simpleParse dname of
    Just distro -> func distro
    _ -> badRequest $ toResponse "Not a valid distro name"

getMaintainersGroup :: UserGroup -> DistroName -> IO (Maybe UserGroup)
getMaintainersGroup admins dname = do
    isDist <- query (IsDistribution dname)
    case isDist of
      False -> return Nothing
      True  -> return . Just $ UserGroup
        { groupDesc = maintainerDescription dname
        , queryUserList = query $ GetDistroMaintainers dname
        , addUserList = update . AddDistroMaintainer dname
        , removeUserList = update . RemoveDistroMaintainer dname
        , groupExists = query (IsDistribution dname)
        , canAddGroup = [admins]
        , canRemoveGroup = [admins]
        }


maintainerDescription :: DistroName -> GroupDescription
maintainerDescription dname = nullDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (str, Just $ "/distro/" ++ display dname)
  , groupPrologue = "Maintainers for a distribution can map packages to it."
  }
  where str = display dname

