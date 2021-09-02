{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE BangPatterns    #-}

-- | TUF security features
module Distribution.Server.Features.Security (
    initSecurityFeature
  ) where

-- Standard libraries
import Control.Exception
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy

-- Hackage
import Distribution.Server.Features.Core
import Distribution.Server.Features.Security.Backup
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Features.Security.ResponseContentTypes
import Distribution.Server.Features.Security.State
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Framework
import Distribution.Server.Packages.Index
import Distribution.Server.Packages.Types

-- Hackage security
import Hackage.Security.Util.Some
import qualified Hackage.Security.Server      as Sec
import qualified Hackage.Security.Util.Path   as Sec

data SecurityFeature = SecurityFeature {
      securityFeatureInterface :: HackageFeature
    }

instance IsHackageFeature SecurityFeature where
  getFeatureInterface SecurityFeature{..} = securityFeatureInterface

initSecurityFeature :: ServerEnv -> IO (CoreFeature -> IO SecurityFeature)
initSecurityFeature env = do
    securityState <- securityStateComponent env (serverStateDir env)
    return $ \coreFeature -> do

       -- Update the security state whenever the main package index changes
       registerHook (indexUpdatedHook coreFeature) $ \_ ->
         updateIndexFileInfo coreFeature securityState

       -- Add package metadata whenever a package is added/changed
       --
       -- For package changes we just add a new metadata file to the index,
       -- which will override any previous one.
       --
       -- TODO: We cannot deal with deletes (they are a problem elsewhere too)
       --
       -- NOTE: this hook is in general _not atomic_ with the package index update related to it.
       -- It is atomic _only_ in the PackageChangeAdd case. As most other significant cases are no-ops
       -- at the moment for adding index entries, this should be ok. (The exception is updated tarball
       -- but this is only used for the mirror client).
       --
       -- If in the future more stuff is registered here, we may need to change code elsewhere
       -- to ensure that it is added atomically as well...
       registerHook (preIndexUpdateHook coreFeature) $ \chg -> do
         let (ents,msg) = case chg of
                      PackageChangeAdd      pkg -> (indexEntriesFor pkg,"PackageChangeAdd")
                      PackageChangeInfo s _ new -> case s of
                        PackageUpdatedTarball    -> (indexEntriesFor new,"PackageChangeInfo:PackageUpdatedTarball")
                        -- .cabal file is not recorded in the TUF metadata
                        -- (until we have author signing anyway)
                        PackageUpdatedCabalFile  -> ([],"PackageChangeInfo:PackageUpdatedCabalFile")
                        -- the uploader is not included in the TUF metadata
                        PackageUpdatedUploader   -> ([],"PackageChangeInfo:PackageUpdatedUploader")
                        -- upload time is not included in the TUF metadata
                        -- (it is recorded in the MetadataEntry because we use it for
                        -- the tarball construction, but it doesn't affect the contents
                        -- of the TUF metadata)
                        PackageUpdatedUploadTime -> ([],"PackageChangeInfo:PackageUpdatedUploadTime")
                      PackageChangeDelete _     -> ([],"PackageChangeDelete")
                      PackageChangeIndexExtra{} -> ([],"PackageChangeIndexExtra")

         loginfo maxBound (mconcat ["TUF preIndexUpdateHook invoked (", msg, ", n = ", show (length ents), ")"])
         return ents

       return $ securityFeature env securityState
  where
    indexEntriesFor :: PkgInfo -> [TarIndexEntry]
    indexEntriesFor pkgInfo =
      case pkgLatestTarball pkgInfo of
        Nothing -> []
        Just (_tarball, (uploadTime, _uploadUserId), latestRev) ->
          [MetadataEntry (pkgInfoId pkgInfo) latestRev uploadTime]

-- | The main security feature
--
-- Missing resources (for Phase 2 of the security work):
--
-- * Top-level targets.json (currently top-level targets.json is not
--   required because it's hardcoded in the clients)
-- * Other targets.json files for OOT targets
--
-- Note that even once we have author signing, per-package targets.json file
-- do not get their own resource, but are instead recorded in the tarball.
securityFeature :: ServerEnv
                -> StateComponent AcidState SecurityState
                -> SecurityFeature
securityFeature env securityState =
    SecurityFeature{..}
  where
    securityFeatureInterface = (emptyHackageFeature "security") {
        featureDesc        = "TUF Security"
      , featureState       = [abstractAcidStateComponent securityState]
      , featureReloadFiles = updateRootMirrorsAndKeys env securityState
      , featurePostInit    = updateRootMirrorsAndKeys env securityState
                          >> setupResignCronJob env securityState
      , featureResources   = [
            resourceTimestamp
          , resourceSnapshot
          , resourceRoot
          , resourceMirrors
          ]
      }

    resourceTimestamp = (secResourceAt Sec.repoLayoutTimestamp) {
        resourceDesc = [(GET, "Get TUF timestamp")]
      , resourceGet  = [("json", serveFromState securityTimestamp)]
      }
    resourceSnapshot = (secResourceAt Sec.repoLayoutSnapshot) {
        resourceDesc = [(GET, "Get TUF snapshot")]
      , resourceGet  = [("json", serveFromState securitySnapshot)]
      }
    resourceRoot = (secResourceAt Sec.repoLayoutRoot) {
        resourceDesc = [(GET, "Get TUF root")]
      , resourceGet  = [("json", serveFromState securityRoot)]
      }
    resourceMirrors = (secResourceAt Sec.repoLayoutMirrors) {
        resourceDesc = [(GET, "Get TUF mirrors")]
      , resourceGet  = [("json", serveFromState securityMirrors)]
      }

    serveFromState :: (IsTUFFile a, ToMessage a)
                   => (SecurityStateFiles -> a)
                   -> DynamicPath
                   -> ServerPartE Response
    serveFromState file _ = do
      msfiles <- queryState securityState GetSecurityFiles
      case msfiles of
        Nothing -> errNotFound "Security files not available"
                     [MText $ "The repository is not currently using TUF "
                           ++ "security so the security files are not "
                           ++ "available."]
        Just sfiles -> do
          let tufFile = file sfiles
              eTag    = ETag $ show (tufFileHashMD5 tufFile)
          -- Higher max-age values result in higher cache hit ratios, but also
          -- in higher likelihood of cache incoherence problems (and of course in
          -- higher likelihood of caches beind out of date with updates to the
          -- central server).
          cacheControl [Public, NoTransform, maxAgeMinutes 1] eTag
          enableRange
          return $ toResponse tufFile

securityStateComponent :: ServerEnv
                       -> FilePath
                       -> IO (StateComponent AcidState SecurityState)
securityStateComponent env stateDir = do
    let stateFile = stateDir </> "db" </> "TUF"
    st <- logTiming (serverVerbosity env) "Loaded SecurityState" $
            openLocalStateFrom stateFile initialSecurityState
    return StateComponent {
        stateDesc    = "TUF specific state"
      , stateHandle  = st
      , getState     = query st GetSecurityState
      , putState     = update st . ReplaceSecurityState
      , resetState   = securityStateComponent env
      , backupState  = \_ -> securityBackup
      , restoreState = securityRestore
      }

updateIndexFileInfo :: CoreFeature
                    -> StateComponent AcidState SecurityState
                    -> IO ()
updateIndexFileInfo coreFeature securityState = do
    IndexTarballInfo{..}  <- queryGetIndexTarballInfo coreFeature
    let !tarGzFileInfo = fileInfo indexTarballIncremGz
        !tarFileInfo   = fileInfo indexTarballIncremUn
    now <- getCurrentTime
    updateState securityState (SetTarGzFileInfo tarGzFileInfo tarFileInfo now)

updateRootMirrorsAndKeys :: ServerEnv
                         -> StateComponent AcidState SecurityState
                         -> IO ()
updateRootMirrorsAndKeys env securityState = do
    mbRootMirrorsAndKeys <- loadRootMirrorsAndKeys env
    st <- queryState securityState GetSecurityState
    case mbRootMirrorsAndKeys of
      Just (root, mirrors, snapshotKey, timestampKey)
        | anyChange st root mirrors snapshotKey timestampKey
        -> do loginfo (serverVerbosity env) "Security files changed, updating"
              now <- getCurrentTime
              updateState securityState (SetRootMirrorsAndKeys
                                           root mirrors
                                           snapshotKey timestampKey
                                           now)
      _ -> loginfo (serverVerbosity env) "Security files unchanged"
  where
    anyChange SecurityState{ securityStateFiles = Nothing } _ _ _ _ = True
    anyChange SecurityState{ securityStateFiles = Just SecurityStateFiles{..} }
              root mirrors snapshotKey timestampKey =
        securityRoot         /= root
     || securityMirrors      /= mirrors
     || securitySnapshotKey  /= snapshotKey
     || securityTimestampKey /= timestampKey

loadRootMirrorsAndKeys :: ServerEnv
                       -> IO (Maybe (Root, Mirrors, Some Sec.Key, Some Sec.Key))
loadRootMirrorsAndKeys env = do
    anyExist <- (\s t r m -> s || t || r || m)
            <$> Sec.doesFileExist (onDiskSnapshotKey  env)
            <*> Sec.doesFileExist (onDiskTimestampKey env)
            <*> Sec.doesFileExist (onDiskRoot    env)
            <*> Sec.doesFileExist (onDiskMirrors env)
    if not anyExist
      then return Nothing
      else do
        snapshotKey  <- readKey (onDiskSnapshotKey  env)
        timestampKey <- readKey (onDiskTimestampKey env)
        root         <- Root    <$> getTUFFile (onDiskRoot    env)
        mirrors      <- Mirrors <$> getTUFFile (onDiskMirrors env)
        --TODO: check sanity before updating
        return (Just (root, mirrors, snapshotKey, timestampKey))

setupResignCronJob :: ServerEnv
                   -> StateComponent AcidState SecurityState
                   -> IO ()
setupResignCronJob env securityState =
    addCronJob (serverCron env) CronJob {
        cronJobName      = "Resign TUF data"
      , cronJobFrequency = DailyJobFrequency
      , cronJobOneShot   = False
      , cronJobAction    = do
          now <- getCurrentTime
          updateState securityState (ResignSnapshotAndTimestamp maxAge now)
      }
  where
    maxAge = 60 * 60 * 23 -- Don't resign if unchanged and younger than ~1 day

readKey :: Sec.Path Sec.Absolute -> IO (Some Sec.Key)
readKey fp = do
  mKey <- Sec.readJSON_NoKeys_NoLayout fp
  case mKey of
    Left  err -> throwIO err
    Right key -> return key

getTUFFile :: Sec.Path Sec.Absolute -> IO TUFFile
getTUFFile file =
    Sec.withFile file Sec.ReadMode $ \h ->
      evaluate . mkTUFFile =<< BS.Lazy.hGetContents h
