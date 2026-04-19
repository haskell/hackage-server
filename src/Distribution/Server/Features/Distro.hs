{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, RecursiveDo #-}
module Distribution.Server.Features.Distro (
    DistroFeature(..),
    DistroResource(..),
    initDistroFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), nullDescription)
import qualified Distribution.Server.Features.Distro.State as Acid
import Distribution.Server.Features.Distro.Types
import Distribution.Server.Features.Distro.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Text (display, simpleParse)
import Distribution.Package

import Data.List (intercalate)
import qualified Data.Text as T
import Text.CSV (parseCSV)

-- TODO:
-- 1. write an HTML view for this module, and delete the text
-- 2. use GroupResource from the Users feature
-- 3. use MServerPart to support multiple views
data DistroFeature = DistroFeature {
    distroFeatureInterface :: HackageFeature,
    distroResource   :: DistroResource,
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

    return $ \user@UserFeature{adminGroup, groupResourcesAt} core@CoreFeature{coreResource} -> do
      rec
        let
          maintainersUserGroup :: DistroName -> UserGroup
          maintainersUserGroup name =
            UserGroup {
              groupDesc             = maintainerGroupDescription name,
              queryUserGroup        = queryState  distrosState $ Acid.GetDistroMaintainers name,
              addUserToGroup        = updateState distrosState . Acid.AddDistroMaintainer name,
              removeUserFromGroup   = updateState distrosState . Acid.RemoveDistroMaintainer name,
              groupsAllowedToAdd    = [adminGroup],
              groupsAllowedToDelete = [adminGroup]
            }
          feature = distroFeature user core distrosState maintainersGroupResource maintainersUserGroup
        distroNames <- queryState distrosState Acid.EnumerateDistros
        (_maintainersGroup, maintainersGroupResource) <-
          groupResourcesAt "/distro/:package/maintainers"
                           maintainersUserGroup
                           (\distroName -> [("package", display distroName)])
                           (packageInPath coreResource)
                           distroNames

      return feature

distrosStateComponent :: FilePath -> IO (StateComponent AcidState Acid.Distros)
distrosStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Distros") Acid.initialDistros
  return StateComponent {
      stateDesc    = ""
    , stateHandle  = st
    , getState     = query st Acid.GetDistributions
    , putState     = \(Acid.Distros dists versions) -> update st (Acid.ReplaceDistributions dists versions)
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = distrosStateComponent
    }

distroFeature :: UserFeature
              -> CoreFeature
              -> StateComponent AcidState Acid.Distros
              -> GroupResource
              -> (DistroName -> UserGroup)
              -> DistroFeature
distroFeature UserFeature{..}
              CoreFeature{coreResource=CoreResource{packageInPath}}
              distrosState
              maintainersGroupResource
              distroGroup
  = DistroFeature{..}
  where
    distroFeatureInterface = (emptyHackageFeature "distro") {
        featureResources =
         groupResource maintainersGroupResource
         : groupUserResource maintainersGroupResource
         : map ($ distroResource) [
              distroIndexPage
            , distroAllPage
            , distroPackages
            , distroPackage
            ]
      , featureState = [abstractAcidStateComponent distrosState]
      }

    queryPackageStatus :: MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
    queryPackageStatus pkgname = queryState distrosState (Acid.PackageStatus pkgname)

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

    textEnumDistros _ = fmap (toResponse . intercalate ", " . map display) (queryState distrosState Acid.EnumerateDistros)
    textDistroPkgs dpath = withDistroPath dpath $ \dname pkgs -> do
        let pkglines = map (\(name, info) -> display name ++ " at " ++ display (distroVersion info) ++ ": " ++ distroUrl info) pkgs
        return $ toResponse (unlines $ ("Packages for " ++ display dname):pkglines)
    csvDistroPackageList dpath = withDistroPath dpath $ \_dname pkgs -> do
        return $ toResponse $ packageListToCSV pkgs
    textDistroPkg dpath = withDistroPackagePath dpath $ \_ _ info -> return . toResponse $ show info

    -- result: see-other uri, or an error: not authenticated or not found (todo)
    distroDelete dpath =
      withDistroNamePath dpath $ \distro -> do
        guardAuthorised_ [InGroup adminGroup]
        -- should also check for existence here of distro here
        void $ updateState distrosState $ Acid.RemoveDistro distro
        seeOther "/distros/" (toResponse ())

    -- result: ok response or not-found error
    distroPackageDelete dpath =
      withDistroPackagePath dpath $ \dname pkgname info -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        case info of
            Nothing -> notFound . toResponse $ "Package not found for " ++ display pkgname
            Just {} -> do
                void $ updateState distrosState $ Acid.DropPackage dname pkgname
                ok $ toResponse "Ok!"

    -- result: see-other response, or an error: not authenticated or not found (todo)
    distroPackagePut dpath =
      withDistroPackagePath dpath $ \dname pkgname _ -> lookPackageInfo $ \newPkgInfo -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        void $ updateState distrosState $ Acid.AddPackage dname pkgname newPkgInfo
        seeOther ("/distro/" ++ display dname ++ "/" ++ display pkgname) $ toResponse "Ok!"

    -- result: see-other response, or an error: not authentcated or bad request
    distroPostNew _ =
      lookDistroName $ \dname -> do
        guardAuthorised_ [InGroup adminGroup]
        success <- updateState distrosState $ Acid.AddDistro dname
        if success
            then seeOther ("/distro/" ++ display dname) $ toResponse "Ok!"
            else badRequest $ toResponse "Selected distribution name is already in use"

    distroPutNew dpath =
      withDistroNamePath dpath $ \dname -> do
        guardAuthorised_ [InGroup adminGroup]
        _success <- updateState distrosState $ Acid.AddDistro dname
        -- it doesn't matter if it exists already or not
        ok $ toResponse "Ok!"

    -- result: ok repsonse or not-found error
    distroPackageListPut dpath =
      withDistroPath dpath $ \dname _pkgs -> do
        guardAuthorised_ [InGroup $ distroGroup dname]
        lookCSVFile $ \csv ->
            case csvToPackageList csv of
                Left  msg  ->
                    badRequest $ toResponse $
                      "Could not parse CSV File to a distro package list: " ++ msg
                Right list -> do
                    void $ updateState distrosState $ Acid.PutDistroPackageList dname list
                    ok $ toResponse "Ok!"

    withDistroNamePath :: DynamicPath -> (DistroName -> ServerPartE Response) -> ServerPartE Response
    withDistroNamePath dpath = require (return $ simpleParse =<< lookup "distro" dpath)

    withDistroPath :: DynamicPath -> (DistroName -> [(PackageName, DistroPackageInfo)] -> ServerPartE Response) -> ServerPartE Response
    withDistroPath dpath func = withDistroNamePath dpath $ \dname -> do
        isDist <- queryState distrosState (Acid.IsDistribution dname)
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            pkgs <- queryState distrosState (Acid.DistroStatus dname)
            func dname pkgs

    -- guards on the distro existing, but not the package
    withDistroPackagePath :: DynamicPath -> (DistroName -> PackageName -> Maybe DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    withDistroPackagePath dpath func =
      withDistroNamePath dpath $ \dname -> do
        pkgname <- packageInPath dpath
        isDist <- queryState distrosState (Acid.IsDistribution dname)
        case isDist of
          False -> notFound $ toResponse "Distribution does not exist"
          True -> do
            pkgInfo <- queryState distrosState (Acid.DistroPackageStatus dname pkgname)
            func dname pkgname pkgInfo

    lookPackageInfo :: (DistroPackageInfo -> ServerPartE Response) -> ServerPartE Response
    lookPackageInfo func = do
        mInfo <- getDataFn $ do
            pVerStr <- look "version"
            pUriStr  <- look "uri"
            case simpleParse pVerStr of
                Just pVer | isValidDistroURI pUriStr -> return $ DistroPackageInfo pVer pUriStr
                _ -> mzero
        case mInfo of
            (Left errs) -> ok $ toResponse $ unlines $ "Sorry, something went wrong there." : errs
            (Right pInfo) -> func pInfo

    lookDistroName :: (DistroName -> ServerPartE Response) -> ServerPartE Response
    lookDistroName func = withDataFn (look "distro") $ \dname -> case simpleParse dname of
        Just distro -> func distro
        _ -> badRequest $ toResponse "Not a valid distro name"

maintainerGroupDescription :: DistroName -> GroupDescription
maintainerGroupDescription dname = nullDescription
  { groupTitle = "Maintainers"
  , groupEntity = Just (str, Just $ "/distro/" ++ display dname)
  , groupPrologue = "Maintainers for a distribution can map packages to it."
  }
  where str = display dname

-- TODO: This calls parseCSV rather that importCSV -- not sure if that
-- matters (in particular, importCSV chops off the last, extraneous,
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

isValidDistroURI :: String -> Bool
isValidDistroURI uri =
  T.pack "https:" `T.isPrefixOf` T.pack uri

csvToPackageList :: CSVFile -> Either String [(PackageName, DistroPackageInfo)]
csvToPackageList (CSVFile records)
    = mapM fromRecord records
 where
    fromRecord [packageStr, versionStr, uri]
      | Just package <- simpleParse packageStr
      , Just version <- simpleParse versionStr
      , isValidDistroURI uri
      = return (package, DistroPackageInfo version uri)
    fromRecord record
      = Left $ "Invalid distro package entry: " ++ show record
