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
import Data.List (find)
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
    publishPage :: Resource,
    candidateCabal :: Resource,
    candidateTarball :: Resource
    -- there can also be build reports - can use the same package id index
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
          { candidatesPage = (resourceAt "/packages/candidates/") { resourceGet = [("txt", textCandidatesPage)], resourcePost = [("", postCandidate r)] }
          , candidatePage = (resourceAt "/package/:package/candidate.:format") { resourceGet = [("html", basicCandidatePage r)], resourcePut = [("html", putCandidate r)], resourceDelete = [("", doDeleteCandidate)] }
          , publishPage = (resourceAt "/package/:package/candidate/publish.:format") { resourceGet = [("txt", textPublishForm)], resourcePost = [("", postPublish)] }
          , candidateCabal = (resourceAt "/package/:package/candidate/:cabal.cabal") { resourceGet = [("cabal", serveCandidateCabal)] }
          , candidateTarball = (resourceAt "/package/:package/candidate/:tarball.tar.gz") { resourceGet = [("tarball", serveCandidateTarball)] }
          }
      }
  where
    basicCandidatePage :: CheckResource -> Config -> DynamicPath -> ServerPart Response
    basicCandidatePage r _ dpath = redirectCandidatePath self dpath $ \_ mpkg -> case mpkg of
        Left name -> ok . toResponse $ "Insert submission form here for " ++ display name
        Right pkg -> ok . toResponse $ Resource.XHtml $ toHtml
                      [ h3 << "Downloads"
                      , toHtml (section pkg)
                      , h3 << "Warnings"
                      , unordList (candWarnings pkg)
                      ]
      where self = renderResource $ candidatePage r
            -- FIXME: reusing code is nice, but this creates links incorrectly (:package is versioned when it should just be a name)
            -- either change the URI function for the resources or have a more typed way to create URIs
            section cand = basicPackageSection (renderResource $ candidateCabal r) (renderResource $ candidateTarball r) (candPkgInfo cand)

    textCandidatesPage _ _ = return . toResponse $ "Insert list of candidate packages here"

    postCandidate r config _ = do
        res <- uploadCandidate Nothing (serverStore config)
        case res of
            Left (UploadFailed code err) -> resp code $ toResponse err
            Right pkgInfo -> seeOther (fromJust $ renderResource (candidatePage r) [("package", display $ packageName pkgInfo)]) (toResponse ())

    putCandidate r config dpath = withPackageName dpath $ \name -> do
        res <- uploadCandidate (Just name) (serverStore config)
        case res of
            Left (UploadFailed code err) -> resp code $ toResponse err
            Right _ -> basicCandidatePage r config dpath

    doDeleteCandidate _ dpath = withCandidatePath dpath $ \_ candidate -> do
        requirePackageAuth candidate
        update $ DeleteCandidate (packageName candidate)
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
                                     withCandidate (packageName pkgid) $ \_ mpkg -> case mpkg of
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

uploadCandidate :: Maybe PackageName -> BlobStorage -> ServerPart (Either UploadFailed CandPkgInfo)
uploadCandidate name storage = do
    regularIndex <- fmap packageList $ query GetPackagesState
    -- ensure that the user has proper auth if the package exists
    res <- extractPackage (processCandidate name regularIndex) storage
    case res of
        Left failed -> return $ Left failed
        Right (pkgInfo, uresult) -> do
            let candidate = CandPkgInfo {
                    candInfoId = packageId pkgInfo,
                    candPkgInfo = pkgInfo,
                    candWarnings = uploadWarnings uresult,
                    candPublic = True -- do withDataFn
                }
            update $ SetCandidate candidate
            unless (packageExists regularIndex pkgInfo) $ update $ AddPackageMaintainer (packageName pkgInfo) (pkgUploadUser pkgInfo)
            return . Right $ candidate

-- | Helper function for uploadCandidate.
processCandidate :: Maybe PackageName -> PackageIndex PkgInfo -> Users.UserId -> UploadResult -> IO (Maybe UploadFailed)
processCandidate mpkgName state uid res = do
    let pkg = packageId (uploadDesc res)
    if maybe False (/= packageName pkg) mpkgName
      then return . Just $ UploadFailed 403 $ "Name of package does not match"
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
        Nothing -> case find ((> candVersion) . packageVersion) pkgs of
            Just pkg -> Just $ UploadFailed 403  $ "Later versions exist in the database than the candidate ("
                                                   ++display (packageVersion pkg)++" > "++display candVersion++ ")"
            Nothing  -> Nothing

------------------------------------------------------------------------------
withCandidatePath :: DynamicPath -> (CandidatePackages -> CandPkgInfo -> ServerPart Response) -> ServerPart Response
withCandidatePath dpath func = withPackageName dpath $ \name -> withCandidate name $ \state mpkg -> case mpkg of
    Nothing  -> notFound . toResponse $ "Candidate for " ++ display name ++ " does not exist"
    Just pkg -> func state pkg

-- For the main candidate page -- redirect to non-versioned name if necessary,
-- and continue even if package doesn't exist.
redirectCandidatePath :: URIGen -> DynamicPath -> (CandidatePackages -> Either PackageName CandPkgInfo -> ServerPart Response) -> ServerPart Response
redirectCandidatePath gen dpath func = withPackageId dpath $ \(PackageIdentifier name version) -> case version of
    Version [] [] -> withCandidate name $ \state -> func state . maybe (Left name) Right
    _ -> seeOther (fromJust $ gen $ ("package", display name):dpath) (toResponse ())

withCandidate :: PackageName -> (CandidatePackages -> Maybe CandPkgInfo -> ServerPart Response) -> ServerPart Response
withCandidate name func = do 
    state <- query GetCandidatePackages
    func state $ listToMaybe $ PackageIndex.lookupPackageName (candidateList state) name

