{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
-- | The tar index cache provides generic support for caching a tarball's
-- TarIndex; this is used by various other modules.
module Distribution.Server.Features.TarIndexCache (
    TarIndexCacheFeature(..)
  , initTarIndexCacheFeature
  ) where

import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT(..), runExceptT)

import Data.Serialize (runGetLazy, runPutLazy)
import Data.SafeCopy (safeGet, safePut)
import Data.Maybe (listToMaybe)

import Distribution.Server.Framework
import Distribution.Server.Framework.BlobStorage
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Features.TarIndexCache.State
import Distribution.Server.Features.Users
import Distribution.Server.Packages.Types
import Data.TarIndex
import qualified Data.TarIndex as TarIndex
import Distribution.Server.Util.ServeTarball (constructTarIndex)
import Distribution.Package (packageId)
import Distribution.Text (display)

import qualified Data.Map as Map
import Data.Aeson (toJSON)

data TarIndexCacheFeature = TarIndexCacheFeature {
    tarIndexCacheFeatureInterface :: HackageFeature
  , cachedTarIndex        :: BlobId -> IO TarIndex
  , cachedPackageTarIndex :: PkgTarball -> IO TarIndex
  , packageTarball :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex))
  , findToplevelFile :: PkgInfo -> (FilePath -> Bool)
                     -> IO (Either String (FilePath, ETag, TarEntryOffset, FilePath))
  }

instance IsHackageFeature TarIndexCacheFeature where
  getFeatureInterface = tarIndexCacheFeatureInterface

initTarIndexCacheFeature :: ServerEnv
                         -> IO (UserFeature
                             -> IO TarIndexCacheFeature)
initTarIndexCacheFeature env@ServerEnv{serverStateDir} = do
    tarIndexCache <- tarIndexCacheStateComponent serverStateDir

    return $ \users -> do
      let feature = tarIndexCacheFeature env users tarIndexCache
      return feature

tarIndexCacheStateComponent :: FilePath -> IO (StateComponent AcidState TarIndexCache)
tarIndexCacheStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "TarIndexCache") initialTarIndexCache
  return StateComponent {
      stateDesc    = "Mapping from tarball blob IDs to tarindex blob IDs"
    , stateHandle  = st
    , getState     = query st GetTarIndexCache
    , putState     = update st . ReplaceTarIndexCache
    , resetState   = tarIndexCacheStateComponent
    -- We don't backup the tar indices, but reconstruct them on demand
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "The impossible happened"
                       , restoreFinalize = return initialTarIndexCache
                       }
    }

tarIndexCacheFeature :: ServerEnv
                     -> UserFeature
                     -> StateComponent AcidState TarIndexCache
                     -> TarIndexCacheFeature
tarIndexCacheFeature ServerEnv{serverBlobStore = store}
                     UserFeature{..}
                     tarIndexCache =
   TarIndexCacheFeature{..}
  where
    tarIndexCacheFeatureInterface :: HackageFeature
    tarIndexCacheFeatureInterface = (emptyHackageFeature "tarIndexCache") {
        featureDesc  = "Generic cache for tarball indices"
        -- We don't want to compare blob IDs
        -- (TODO: We could potentially check that if a package occurs in both
        -- packages then both caches point to identical tar indices, but for
        -- that we would need to be in IO)
      , featureState = [abstractAcidStateComponent' (\_ _ -> []) tarIndexCache]
      , featureResources = [
            (resourceAt "/server-status/tarindices.:format") {
                resourceDesc   = [ (GET,    "Which tar indices have been generated?")
                                 , (DELETE, "Delete all tar indices (will be regenerated on the fly)")
                                 ]
              , resourceGet    = [ ("json", \_ -> serveTarIndicesStatus) ]
              , resourceDelete = [ ("",     \_ -> deleteTarIndices) ]
              }
          ]
      }

    -- This is the heart of this feature
    cachedTarIndex :: BlobId -> IO TarIndex
    cachedTarIndex tarBallBlobId = do
      mTarIndexBlobId <- queryState tarIndexCache (FindTarIndex tarBallBlobId)
      case mTarIndexBlobId of
        Just tarIndexBlobId -> do
          serializedTarIndex <- fetch store tarIndexBlobId
          case runGetLazy safeGet serializedTarIndex of
            Left  err      -> throwIO (userError err)
            Right tarIndex -> return tarIndex
        Nothing -> do
          tarBall        <- fetch store tarBallBlobId
          tarIndex       <- case constructTarIndex tarBall of
                              Left  err      -> throwIO (userError err)
                              Right tarIndex -> return tarIndex
          tarIndexBlobId <- add store (runPutLazy (safePut tarIndex))
          updateState tarIndexCache (SetTarIndex tarBallBlobId tarIndexBlobId)
          return tarIndex

    cachedPackageTarIndex :: PkgTarball -> IO TarIndex
    cachedPackageTarIndex = cachedTarIndex . pkgTarballNoGz

    serveTarIndicesStatus :: ServerPartE Response
    serveTarIndicesStatus = do
      TarIndexCache state <- liftIO $ getState tarIndexCache
      return . toResponse . toJSON . Map.toList $ state

    -- | With curl:
    --
    -- > curl -X DELETE http://admin:admin@localhost:8080/server-status/tarindices
    deleteTarIndices :: ServerPartE Response
    deleteTarIndices = do
      guardAuthorised_ [InGroup adminGroup]
      -- TODO: This resets the tar indices _state_ only, we don't actually
      -- remove any blobs
      liftIO $ putState tarIndexCache initialTarIndexCache
      ok $ toResponse "Ok!"

    -- Functions to access specific files in a tarball

    packageTarball :: PkgInfo -> IO (Either String (FilePath, ETag, TarIndex))
    packageTarball pkginfo
      | Just (pkgTarball, _uploadinfo, _revNo) <- pkgLatestTarball pkginfo = do
        let blobid = pkgTarballNoGz pkgTarball
            fp     = BlobStorage.filepath store blobid
            etag   = BlobStorage.blobETag blobid
        index <- cachedPackageTarIndex pkgTarball
        return $ Right (fp, etag, index)
      | otherwise =
        return $ Left "No tarball found"

    -- TODO: Specify *what* file wasn't found in the error. This will require another parameter.
    findToplevelFile :: PkgInfo -> (FilePath -> Bool)
                     -> IO (Either String (FilePath, ETag, TarEntryOffset, FilePath))
    findToplevelFile pkg test = runExceptT $ do
        (fp, etag, index) <- ExceptT $ packageTarball pkg
        (offset, fname)   <- ExceptT $ return . maybe (Left "File not found") Right
                                     $ findFile index
        return (fp, etag, offset, fname)
      where
        topdir :: FilePath
        topdir = display (packageId pkg)

        findFile :: TarIndex -> Maybe (TarEntryOffset, String)
        findFile index = do
          TarDir fnames <- TarIndex.lookup index topdir
          listToMaybe $
            [ (offset, fname')
            | (fname, _) <- fnames
            , test fname
            , let fname' = topdir </> fname
            , Just (TarIndex.TarFileEntry offset) <- [TarIndex.lookup index fname']
            ]
