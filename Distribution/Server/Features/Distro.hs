module Distribution.Server.Features.Distro (
    DistroFeature(..),
    initDistroFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Resource
import Distribution.Server.Types

import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..))
import Distribution.Server.Distributions.State
import Distribution.Server.Distributions.Types

import Happstack.Server
import Happstack.State
import Data.List (intercalate, unlines)
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
      { featureName = "Distro"
      , resources   = map ($distroResource distro) [distroIndexPage, distroAllPage, distroPackage]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initDistroFeature :: CoreFeature -> PackagesFeature -> IO DistroFeature
initDistroFeature _ _ = do
    return $ DistroFeature
      { distroResource = DistroResource
          { distroIndexPage = (resourceAt "/distros/.:format") { resourceGet = [("txt", textEnumDistros)], resourcePost = [("", distroNew)] }
          , distroAllPage = (resourceAt "/distro/:distro/.:format") { resourceGet = [("txt", textDistroPkgs)], resourceDelete = [("", distroDelete)] }
          , distroPackage = (resourceAt "/distro/:distro/:package.:format") { resourceGet = [("txt", textDistroPkg)], resourcePut = [("", distroPackagePut)], resourceDelete = [("", distroPackageDelete)] }
          }
      , maintainersGroup = getMaintainersGroup
      }
  where
    textEnumDistros _ _ = fmap (toResponse . intercalate ", " . map display) (query Enumerate)
    textDistroPkgs _ dpath = withDistroPath dpath $ \dname pkgs -> do
        let pkglines = map (\(name, info) -> display name ++ " at " ++ display (distroVersion info) ++ ": " ++ distroUrl info) $ pkgs
        return $ toResponse (unlines $ ("Packages for " ++ display dname):pkglines)
    textDistroPkg _ dpath = withDistroPackagePath dpath $ \_ _ info -> return . toResponse $ show info

    distroDelete _ dpath = withDistroNamePath dpath $ \distro -> do
        -- authenticate Hackage admins
        -- should also check for existence here..
        update $ RemoveDistro distro
        seeOther ("/distros/") (toResponse ())

    distroPackageDelete _ dpath = withDistroPackagePath dpath $ \dname pkgname info -> do
        -- authenticate distro maintainer
        case info of
            Nothing -> notFound . toResponse $ "Package not found for " ++ display pkgname
            Just {} -> do
                update $ DropPackage dname pkgname
                ok $ toResponse "Ok!"

    -- should probably substituted the badRequests with something more specific
    distroPackagePut _ dpath = withDistroPackagePath dpath $ \dname pkgname _ -> lookPackageInfo $ \newPkgInfo -> do
        -- authenticate distro maintainer
        update $ AddPackage dname pkgname newPkgInfo
        seeOther ("/distro/" ++ display dname ++ "/" ++ display pkgname) $ toResponse "Ok!"

    distroNew _ _ = lookDistroName $ \dname -> do
        success <- update $ AddDistro dname
        if success
            then seeOther ("/distro/" ++ display dname) $ toResponse "Ok!"
            else badRequest $ toResponse "Selected distribution name is already in use"

withDistroNamePath :: DynamicPath -> (DistroName -> ServerPart Response) -> ServerPart Response
withDistroNamePath dpath func = case simpleParse =<< lookup "distro" dpath of
    Nothing    -> notFound $ toResponse "Could not find distro: not a valid distro name"
    Just dname -> func dname

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

getMaintainersGroup :: DynamicPath -> IO (Maybe UserGroup)
getMaintainersGroup dpath = case simpleParse =<< lookup "distro" dpath of
  Nothing -> return Nothing
  Just dname -> do
    isDist <- query (IsDistribution dname)
    case isDist of
      False -> return Nothing
      True  -> return . Just $ UserGroup
        { groupDesc = maintainerDescription dname
        , queryUserList = query $ GetDistroMaintainers dname
        , addUserList = update . AddDistroMaintainer dname
        , removeUserList = update . RemoveDistroMaintainer dname
        }

maintainerDescription :: DistroName -> GroupDescription
maintainerDescription dname = GroupDescription
  { groupTitle = "Maintainers for " ++ display dname
  , groupShort = ""
  , groupEntityURL = "/distro/" ++ display dname
  , groupPrologue  = [] --prologue (desciption pkg)?
  }

