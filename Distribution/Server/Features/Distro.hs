{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Distro (
    DistroFeature(..),
    DistroResource(..),
    initDistroFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import Distribution.Server.Features.Distro.State
import Distribution.Server.Features.Distro.Types
import Distribution.Server.Features.Distro.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Text (display, simpleParse)
import Distribution.Package

import Data.List (intercalate)
import Text.CSV (parseCSV)

-- TODO:
-- 1. write an HTML view for this module, and delete the text
-- 2. use GroupResource from the Users feature
-- 3. use MServerPart to support multiple views
data DistroFeature = DistroFeature {
    distroFeatureInterface :: HackageFeature,
    distroResource   :: DistroResource,
    maintainersGroup :: DynamicPath -> IO (Maybe UserGroup),
    queryPackageStatus :: forall m. MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
}

instance IsHackageFeature DistroFeature where
    getFeatureInterface = distroFeatureInterface

data DistroResource = DistroResource {
    distroIndexPage :: Resource,
    distroAllPage   :: Resource,
    distroPackages  :: Resource,
    distroPackage   :: Resource
}

initDistroFeature :: ServerEnv
                  -> IO (UserFeature -> CoreFeature -> IO DistroFeature)
initDistroFeature ServerEnv{serverStateDir} = do
    distrosState <- distrosStateComponent serverStateDir

    return $ \user core -> do
      let feature = distroFeature user core distrosState
      return feature

distrosStateComponent :: FilePath -> IO (StateComponent AcidState Distros)
distrosStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Distros") initialDistros
  return StateComponent {
      stateDesc    = ""
    , stateHandle  = st
    , getState     = query st GetDistributions
    , putState     = \(Distros dists versions) -> update st (ReplaceDistributions dists versions)
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = distrosStateComponent
    }

distroFeature :: UserFeature
              -> CoreFeature
              -> StateComponent AcidState Distros
              -> DistroFeature
distroFeature UserFeature{..}
              CoreFeature{coreResource=CoreResource{packageInPath}}
              distrosState
  = DistroFeature{..}
  where
    distroFeatureInterface = (emptyHackageFeature "distro") {
        featureResources =
          map ($ distroResource) [
              distroIndexPage
            , distroAllPage
            , distroPackages
            , distroPackage
            ]
      , featureState = [abstractAcidStateComponent distrosState]
      }

    queryPackageStatus :: MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
    queryPackageStatus pkgname = queryState distrosState (PackageStatus pkgname)

    distroResource = DistroResource
          { distroIndexPage = (resourceAt "/distros/.:format") {
                resourceGet  = [("txt", textEnumDistros)],
                resourcePost = [("", distroPostNew)]
              }
          , distroAllPage = (resourceAt "/distro/:distro") {
                resourcePut    = [("", distroPutNew)],
                resourceDelete = [("", distroDelete)]
              }
          , distroPackages = (resourceAt "/distro/:distro/packages.:format") {
                resourceGet    = [("txt", textDistroPkgs),
                                  ("csv", csvDistroPackageList)],
                resourcePut    = [("csv", distroPackageListPut)]
              }
          , distroPackage = (resourceAt "/distro/:distro/package/:package.:format") {
                resourceGet    = [("txt", textDistroPkg)],
                resourcePut    = [("",    distroPackagePut)],
                resourceDelete = [("",    distroPackageDelete)]
              }
          }

    maintainersGroup = \dpath -> case simpleParse =<< lookup "distro" dpath of
            Nothing -> return Nothing
            Just dname -> getMaintainersGroup adminGroup dname

    textEnumDistros _ = fmap (toResponse . intercalate ", " . map display) (queryState distrosState EnumerateDistros)
    textDistroPkgs dpath = withDistroPath dpath $ \dname pkgs -> do
        let pkglines = map (\(name, info) -> display name ++ " at " ++ display (distroVersion info) ++ ": " ++ distroUrl info) $ pkgs
        return $ toResponse (unlines $ ("Packages for " ++ display dname):pkglines)
    csvDistroPackageList dpath = withDistroPath dpath $ \_dname pkgs -> do
        return $ toResponse $ packageListToCSV $ pkgs
    textDistroPkg dpath = withDistroPackagePath dpath $ \_ _ info -> return . toResponse $ show info

    -- result: see-other uri, or an error: not authenticated or not found (todo)
    distroDelete dpath =
      withDistroNamePath dpath $ \distro -> do
        guardAuthorised_ [InGroup adminGroup] --TODO: use the per-distro maintainer groups
        -- should also check for existence here of distro here
        void $ updateState distrosState $ RemoveDistro distro
        seeOther ("/distros/") (toResponse ())

    -- result: ok response or not-found error
    distroPackageDelete dpath =
      withDistroPackagePath dpath $ \dname pkgname info -> do
        guardAuthorised_ [AnyKnownUser] --TODO: use the per-distro maintainer groups
        case info of
            Nothing -> notFound . toResponse $ "Package not found for " ++ display pkgname
            Just {} -> do
                void $ updateState distrosState $ DropPackage dname pkgname
                ok $ toResponse "Ok!"

    -- result: see-other response, or an error: not authenticated or not found (todo)
    distroPackagePut dpath =
      withDistroPackagePath dpath $ \dname pkgname _ -> lookPackageInfo $ \newPkgInfo -> do
        guardAuthorised_ [AnyKnownUser] --TODO: use the per-distro maintainer groups
        void $ updateState distrosState $ AddPackage dname pkgname newPkgInfo
        seeOther ("/distro/" ++ display dname ++ "/" ++ display pkgname) $ toResponse "Ok!"

    -- result: see-other response, or an error: not authentcated or bad request
    distroPostNew _ =
      lookDistroName $ \dname -> do
        guardAuthorised_ [AnyKnownUser] --TODO: use the per-distro maintainer groups
        success <- updateState distrosState $ AddDistro dname
        if success
            then seeOther ("/distro/" ++ display dname) $ toResponse "Ok!"
            else badRequest $ toResponse "Selected distribution name is already in use"

    distroPutNew dpath =
      withDistroNamePath dpath $ \dname -> do
        guardAuthorised_ [AnyKnownUser] --TODO: use the per-distro maintainer groups
        _success <- updateState distrosState $ AddDistro dname
        -- it doesn't matter if it exists already or not
        ok $ toResponse "Ok!"

    -- result: ok repsonse or not-found error
    distroPackageListPut dpath =
      withDistroPath dpath $ \dname _pkgs -> do
        guardAuthorised_ [AnyKnownUser] --TODO: use the per-distro maintainer groups
        lookCSVFile $ \csv ->
            case csvToPackageList csv of
                Left  msg  ->
                    badRequest $ toResponse $
                      "Could not parse CSV File to a distro package list: " ++ msg
                Right list -> do
                    void $ updateState distrosState $ PutDistroPackageList dname list
                    ok $ toResponse "Ok!"

    withDistroNamePath :: DynamicPath -> (DistroName -> ServerPartE Response) -> ServerPartE Response
    withDistroNamePath dpath = require (return $ simpleParse =<< lookup "distro" dpath)

    withDistroPath :: DynamicPath -> (DistroName -> [(PackageName, DistroPackageInfo)] -> ServerPartE Response) -> ServerPartE Response
    withDistroPath dpath func = withDistroNamePath dpath $ \dname -> do
        isDist <- queryState distrosState (IsDistribution dname)
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            pkgs <- queryState distrosState (DistroStatus dname)
            func dname pkgs

    -- guards on the distro existing, but not the package
    withDistroPackagePath :: DynamicPath -> (DistroName -> PackageName -> Maybe DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    withDistroPackagePath dpath func =
      withDistroNamePath dpath $ \dname -> do
        pkgname <- packageInPath dpath
        isDist <- queryState distrosState (IsDistribution dname)
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            pkgInfo <- queryState distrosState (DistroPackageStatus dname pkgname)
            func dname pkgname pkgInfo

    lookPackageInfo :: (DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    lookPackageInfo func = do
        mInfo <- getDataFn $ do
            pVerStr <- look "version"
            pUriStr  <- look "uri"
            case simpleParse pVerStr of
                Nothing -> mzero
                Just pVer -> return $ DistroPackageInfo pVer pUriStr
        case mInfo of
            (Left errs) -> ok $ toResponse $ unlines $ "Sorry, something went wrong there." : errs
            (Right pInfo) -> func pInfo

    lookDistroName :: (DistroName -> ServerPartE Response) -> ServerPartE Response
    lookDistroName func = withDataFn (look "distro") $ \dname -> case simpleParse dname of
        Just distro -> func distro
        _ -> badRequest $ toResponse "Not a valid distro name"

    getMaintainersGroup :: UserGroup -> DistroName -> IO (Maybe UserGroup)
    getMaintainersGroup admins dname = do
        isDist <- queryState distrosState (IsDistribution dname)
        case isDist of
          False -> return Nothing
          True  -> return . Just $ UserGroup
            { groupDesc             = maintainerGroupDescription dname
            , queryUserGroup        = queryState distrosState $ GetDistroMaintainers dname
            , addUserToGroup        = updateState distrosState . AddDistroMaintainer dname
            , removeUserFromGroup   = updateState distrosState . RemoveDistroMaintainer dname
            , groupsAllowedToAdd    = [admins]
            , groupsAllowedToDelete = [admins]
            }


maintainerGroupDescription :: DistroName -> GroupDescription
maintainerGroupDescription dname = nullDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (str, Just $ "/distro/" ++ display dname)
  , groupPrologue = "Maintainers for a distribution can map packages to it."
  }
  where str = display dname

-- TODO: This calls parseCSV rather that importCSV -- not sure if that
-- matters (in particular, importCSV chops off the last, extranenous,
-- null entry that parseCSV adds)
lookCSVFile :: (CSVFile -> ServerPartE Response) -> ServerPartE Response
lookCSVFile func = do
    fileContents <- expectCSV
    case parseCSV "PUT input" (unpackUTF8 fileContents) of
      Left err -> badRequest $ toResponse $ "Could not parse CSV File: " ++ show err
      Right csv -> func (CSVFile csv)

packageListToCSV :: [(PackageName, DistroPackageInfo)] -> CSVFile
packageListToCSV entries
    = CSVFile $ map (\(pn,DistroPackageInfo version url) -> [display pn, display version, url]) entries

csvToPackageList :: CSVFile -> Either String [(PackageName, DistroPackageInfo)]
csvToPackageList (CSVFile records)
    = mapM fromRecord records
 where
    fromRecord [packageStr, versionStr, uri]
      | Just package <- simpleParse packageStr
      , Just version <- simpleParse versionStr
      = return (package, DistroPackageInfo version uri)
    fromRecord rec
      = Left $ "Invalid distro package entry: " ++ show rec
