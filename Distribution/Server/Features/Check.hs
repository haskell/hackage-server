module Distribution.Server.Features.Check (
    CheckFeature(..),
    initCheckFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Features.Upload

import Distribution.Server.Packages.State
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.ResourceTypes as Resource

import Distribution.Text
import Distribution.Package
import Data.Version
import Happstack.Server
import Happstack.State
import Text.XHtml.Strict (unordList, h3, (<<), toHtml)
import Data.Function (fix)
import Data.List (find, sort)
import Control.Monad (guard, unless)
import Control.Monad.Trans (liftIO)
import Data.Maybe (maybe, listToMaybe, fromJust)
import Data.Time.Clock (getCurrentTime)


data CheckFeature = CheckFeature {
    checkResource :: CheckResource
}

data CheckResource = CheckResource {
    candidatesPage :: Resource,
    candidatePage :: Resource,
    packageCandidatesPage :: Resource,
    publishPage :: Resource,
    candidateCabal :: Resource,
    candidateTarball :: Resource,
    -- there can also be build reports - can use the same package id index
    candidatesUri :: String -> String,
    candidateUri :: String -> PackageId -> String,
    packageCandidatesUri :: String -> PackageId -> String,
    publishUri   :: String -> PackageId -> String,
    candidateTarballUri :: PackageId -> String,
    candidateCabalUri :: PackageId -> String
}


-- candidates can be published at any time. there is one candidate per package.
-- they can be deleted, but it's not required

instance HackageFeature CheckFeature where
    getFeature check = HackageModule
      { featureName = "check"
      , resources   = map ($checkResource check) [candidatesPage, candidatePage, publishPage, candidateCabal, candidateTarball]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

-- URI generation (string-based), using maps; user groups
initCheckFeature :: CoreFeature -> PackagesFeature -> UploadFeature -> IO CheckFeature
initCheckFeature _ _ _ = return CheckFeature
      { checkResource = fix $ \r -> CheckResource
          { candidatesPage = (resourceAt "/packages/candidates/.:format") { resourceGet = [("txt", textCandidatesPage)], resourcePost = [("", postCandidate r)] }
          , candidatePage = (resourceAt "/package/:package/candidate.:format") { resourceGet = [("html", basicCandidatePage r)], resourcePut = [("html", putPackageCandidate r)], resourceDelete = [("", doDeleteCandidate)] }
          , packageCandidatesPage = (resourceAt "/package/:package/candidates/.:format") { resourceGet = [("txt", textPkgCandidatesPage r)], resourcePost = [("", postPackageCandidate r)] }
          , publishPage = (resourceAt "/package/:package/candidate/publish.:format") { resourceGet = [("txt", textPublishForm)], resourcePost = [("", postPublish)] }
          , candidateCabal = (resourceAt "/package/:package/candidate/:cabal.cabal") { resourceGet = [("cabal", serveCandidateCabal)] }
          , candidateTarball = (resourceAt "/package/:package/candidate/:tarball.tar.gz") { resourceGet = [("tarball", serveCandidateTarball)] }
          , candidatesUri = \format -> renderResource (candidatesPage r) [format]
          , candidateUri  = \format pkgid -> renderResource (candidatePage r) [display pkgid, format]
          , packageCandidatesUri = \format pkgid -> renderResource (packageCandidatesPage r) [display pkgid, format]
          , publishUri = \format pkgid -> renderResource (publishPage r) [display pkgid, format]
          , candidateTarballUri = \pkgid -> renderResource (candidateTarball r) [display pkgid, display pkgid]
          , candidateCabalUri = \pkgid -> renderResource (candidateCabal r) [display pkgid, display (packageName pkgid)]
          }
      }
  where
    basicCandidatePage :: CheckResource -> Config -> DynamicPath -> ServerPart Response
    basicCandidatePage r _ dpath = withPackageId dpath $ \pkgid ->
                                   withCandidate pkgid $ \_ mpkg _ -> case mpkg of
        Nothing -> ok . toResponse $ "Insert submission form here for " ++ display (packageName pkgid)
        Just pkg -> ok . toResponse $ Resource.XHtml $ toHtml
                      [ h3 << "Downloads"
                      , toHtml (section pkg)
                      , h3 << "Warnings"
                      , unordList (candWarnings pkg)
                      ]
      where self = candidateUri r ""
            -- FIXME: reusing code is nice, but this creates links incorrectly (:package is versioned when it should just be a name)
            -- either change the URI function for the resources or have a more typed way to create URIs
            section cand = basicPackageSection (candidateCabalUri r) (candidateTarballUri r) (candPkgInfo cand)

    textCandidatesPage _ _ = return . toResponse $ "Insert list of candidate packages here"

    textPkgCandidatesPage r _ dpath = withPackageName dpath $ \name ->
                                      withCandidates name $ \_ pkgs -> do
        return . toResponse $ "Insert list of candidate packages here"

    postCandidate r config _ = do
        res <- uploadCandidate (const True) (serverStore config)
        respondToResult r res

    -- POST to /:package/candidates/
    postPackageCandidate r config dpath = withPackageName dpath $ \name -> do
        res <- uploadCandidate ((==name) . packageName) (serverStore config)
        respondToResult r res

    -- PUT to /:package-version/candidate
    putPackageCandidate r config dpath = withPackageId dpath $ \pkgid -> do
        guard (packageVersion pkgid /= Version [] [])
        res <- uploadCandidate (==pkgid) (serverStore config)
        respondToResult r res

    respondToResult :: CheckResource -> Either UploadFailed CandPkgInfo -> ServerPart Response
    respondToResult r res = case res of
        Left (UploadFailed code err) -> resp code $ toResponse err
        Right pkgInfo -> seeOther (candidateUri r "" $ packageId pkgInfo) (toResponse ())

    doDeleteCandidate _ dpath = withCandidatePath dpath $ \_ candidate -> do
        requirePackageAuth candidate
        update $ DeleteCandidate (packageId candidate)
        seeOther "/packages/candidates/" $ toResponse ()

    textPublishForm _ dpath = withCandidatePath dpath $ \_ candidate -> do
        requirePackageAuth candidate
        packages <- fmap packageList $ query GetPackagesState
        case checkPublish packages candidate of
            Just (UploadFailed _ err) -> ok $ toResponse err
            Nothing -> ok . toResponse $ "Post here to publish the package"

    postPublish _ dpath = withCandidatePath dpath $ \_ candidate -> do
        requirePackageAuth candidate
        res <- publishCandidate (packageName candidate) False
        case res of
            Left (UploadFailed code err) -> resp code $ toResponse err
            Right uresult -> ok $ toResponse $ unlines (uploadWarnings uresult)

serveCandidateTarball :: Config -> DynamicPath -> ServerPart Response
serveCandidateTarball config dpath = withPackageTarball dpath $ \pkgid ->
                                     withCandidate pkgid $ \_ mpkg _ -> case mpkg of
    Nothing -> notFound $ toResponse "Candidate package does not exist"
    Just pkg -> case pkgTarball (candPkgInfo pkg) of
        [] -> notFound $ toResponse "No tarball available"
        ((blobId, _):_) -> do
            file <- liftIO $ BlobStorage.fetch (serverStore config) blobId
            ok $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime $ candPkgInfo pkg)

serveCandidateCabal :: Config -> DynamicPath -> ServerPart Response
serveCandidateCabal _ dpath = withCandidatePath dpath $ \_ pkg -> do
    guard (lookup "cabal" dpath == Just (display $ packageName pkg))
    ok $ toResponse (Resource.CabalFile (pkgData $ candPkgInfo pkg))

uploadCandidate :: (PackageId -> Bool) -> BlobStorage -> ServerPart (Either UploadFailed CandPkgInfo)
uploadCandidate isRight storage = do
    regularIndex <- fmap packageList $ query GetPackagesState
    -- ensure that the user has proper auth if the package exists
    res <- extractPackage (processCandidate isRight regularIndex) storage
    case res of
        Left failed -> return $ Left failed
        Right (pkgInfo, uresult) -> do
            let candidate = CandPkgInfo {
                    candInfoId = packageId pkgInfo,
                    candPkgInfo = pkgInfo,
                    candWarnings = uploadWarnings uresult,
                    candPublic = True -- do withDataFn
                }
            update $ AddCandidate candidate
            unless (packageExists regularIndex pkgInfo) $ update $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
            return . Right $ candidate

-- | Helper function for uploadCandidate.
processCandidate :: (PackageId -> Bool) -> PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe UploadFailed)
processCandidate isRight state uid res = do
    let pkg = packageId (uploadDesc res)
    if not (isRight pkg)
      then return . Just $ UploadFailed 403 $ "Name of package or package version does not match"
      else do
        pkgGroup <- getPackageGroup pkg
        if packageExists state pkg && not (uid `Group.member` pkgGroup)
          then return . Just $ UploadFailed 403 "Not authorized to upload a candidate for this package"
          else return Nothing

publishCandidate :: PackageName -> Bool -> ServerPart (Either UploadFailed UploadResult)
publishCandidate name doDelete = do
    packages <- fmap packageList $ query GetPackagesState
    candidates <- fmap candidateList $ query GetCandidatePackages
    require (return . listToMaybe $ PackageIndex.lookupPackageName candidates name) $ \candidate -> do
    -- check authorization to upload - must already be a maintainer
    (uid, _) <- requirePackageAuth candidate
    -- check if package or later already exists
    case checkPublish packages candidate of
        Just failed -> return . Left $ failed
        Nothing -> do
            uploadData <- fmap (flip (,) uid) (liftIO getCurrentTime)
            let pkgInfo = candPkgInfo candidate
            success <- update $ InsertPkgIfAbsent PkgInfo {
                pkgInfoId     = packageId candidate,
                pkgDesc       = pkgDesc pkgInfo,
                pkgData       = pkgData pkgInfo,
                pkgTarball    = case pkgTarball pkgInfo of
                    ((blobId, _):_) -> [(blobId, uploadData)]
                    [] -> [], -- this shouldn't happen, but let's keep this part total anyway
                pkgUploadData = uploadData,
                pkgDataOld    = []
            }
            if success
              then do
--                liftIO $ runZeroHook hook
                return . Right $ UploadResult (pkgDesc pkgInfo) (pkgData pkgInfo) (candWarnings candidate)
              else return . Left $ UploadFailed 403 "Package already exists."

-- | Helper function for publishCandidate that ensures it's safe to insert into the main index.
checkPublish :: PackageIndex PkgInfo -> CandPkgInfo -> Maybe UploadFailed
checkPublish packages candidate = do
    let pkgs = PackageIndex.lookupPackageName packages (packageName candidate)
        candVersion = packageVersion candidate
    case find ((== candVersion) . packageVersion) pkgs of
        Just {} -> Just $ UploadFailed 403 "Package name and version already exist in the database"
        Nothing  -> Nothing
--      Nothing -> case find ((> candVersion) . packageVersion) pkgs of
--          Just pkg -> Just $ UploadFailed 403  $ "Later versions exist in the database than the candidate ("
--                                                 ++display (packageVersion pkg)++" > "++display candVersion++ ")"
--          Nothing -> Nothing

------------------------------------------------------------------------------
data CandidateRender = CandidateRender {
    candPackageRender :: PackageRender,
    renderWarnings :: [String],
    hasIndexedPackage :: Bool
}

doCandidateRender :: PackageIndex CandPkgInfo -> PackageIndex PkgInfo
                  -> Users.Users -> PackageId -> Maybe CandidateRender
doCandidateRender candIndex pkgIndex users pkgid@(PackageIdentifier name version) = do
    cand <- PackageIndex.lookupPackageId candIndex pkgid
    let infos = PackageIndex.lookupPackageName pkgIndex name
        versions = sort (version:map packageVersion infos)
        render = (doPackageRender users (candPkgInfo cand)) { rendAllVersions = versions }
    return CandidateRender {
        candPackageRender = render,
        renderWarnings = candWarnings cand,
        hasIndexedPackage = not (null infos)
    }


------------------------------------------------------------------------------
withCandidatePath :: DynamicPath -> (CandidatePackages -> CandPkgInfo -> ServerPart Response) -> ServerPart Response
withCandidatePath dpath func = withPackageId dpath $ \pkgid -> withCandidate pkgid $ \state mpkg _ -> case mpkg of
    Nothing  -> notFound . toResponse $ "Candidate for " ++ display pkgid ++ " does not exist"
    Just pkg -> func state pkg

withCandidate :: PackageId -> (CandidatePackages -> Maybe CandPkgInfo -> [CandPkgInfo] -> ServerPart Response) -> ServerPart Response
withCandidate pkgid func = do
    state <- query GetCandidatePackages
    let pkgs = PackageIndex.lookupPackageName (candidateList state) (packageName pkgid)
    func state (find ((==pkgid) . packageId) pkgs) pkgs

withCandidates :: PackageName -> (CandidatePackages -> [CandPkgInfo] -> ServerPart Response) -> ServerPart Response
withCandidates name func = withCandidate (PackageIdentifier name $ Version [] []) $ \state _ infos -> func state infos

