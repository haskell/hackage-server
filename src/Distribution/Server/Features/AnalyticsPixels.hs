{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.AnalyticsPixels
  ( AnalyticsPixelsFeature(..)
  , AnalyticsPixel(..)
  , initAnalyticsPixelsFeature
  ) where

import Data.Set (Set)

import Distribution.Server.Features.AnalyticsPixels.State

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import Distribution.Package

-- | Define the prototype for this feature
data AnalyticsPixelsFeature = AnalyticsPixelsFeature {
    analyticsPixelsFeatureInterface :: HackageFeature,
    analyticsPixelsResource         :: Resource,
    userAnalyticsPixelsResource     :: Resource,

    analyticsPixelAdded             :: Hook (PackageName, AnalyticsPixel) (),
    analyticsPixelRemoved           :: Hook (PackageName, AnalyticsPixel) (),

    -- | Returns all 'AnalyticsPixel's associated with a 'Package'.
    getPackageAnalyticsPixels       :: forall m. MonadIO m => PackageName -> m (Set AnalyticsPixel),

    -- | Adds a new 'AnalyticsPixel' to a 'Package'. Returns True in case it was added. False in case
    -- it's already existing.
    addPackageAnalyticsPixel        :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m Bool,

    -- | Remove a 'AnalyticsPixel' from a 'Package'.
    removePackageAnalyticsPixel     :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m ()
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature AnalyticsPixelsFeature where
  getFeatureInterface = analyticsPixelsFeatureInterface

-- | Called from Features.hs to initialize this feature
initAnalyticsPixelsFeature :: ServerEnv
                          -> IO ( CoreFeature
                            -> UserFeature
                            -> UploadFeature
                            -> IO AnalyticsPixelsFeature)
initAnalyticsPixelsFeature env@ServerEnv{serverStateDir} = do
  dbAnalyticsPixelsState <- analyticsPixelsStateComponent serverStateDir
  analyticsPixelAdded    <- newHook
  analyticsPixelRemoved  <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} uploadf -> do
    let feature = analyticsPixelsFeature env
                  dbAnalyticsPixelsState
                  coref userf uploadf analyticsPixelAdded analyticsPixelRemoved

    return feature

-- | Define the backing store (i.e. database component)
analyticsPixelsStateComponent :: FilePath -> IO (StateComponent AcidState AnalyticsPixelsState)
analyticsPixelsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AnalyticsPixels") initialAnalyticsPixelsState
  return StateComponent {
      stateDesc    = "Backing store for AnalyticsPixels feature"
    , stateHandle  = st
    , getState     = query st GetAnalyticsPixelsState
    , putState     = update st . ReplaceAnalyticsPixelsState
    , resetState   = analyticsPixelsStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return initialAnalyticsPixelsState
                       }
   }


-- | Default constructor for building this feature.
analyticsPixelsFeature :: ServerEnv
                      -> StateComponent AcidState AnalyticsPixelsState
                      -> CoreFeature                          -- To get site package list
                      -> UserFeature                          -- To authenticate users
                      -> UploadFeature                        -- For accessing package maintainers and trustees
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals addition of a new AnalyticsPixel
                      -> Hook (PackageName, AnalyticsPixel) () -- Signals removeal of a AnalyticsPixel
                      -> AnalyticsPixelsFeature

analyticsPixelsFeature  ServerEnv{..}
              analyticsPixelsState
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              UploadFeature{..}
              analyticsPixelAdded
              analyticsPixelRemoved
  = AnalyticsPixelsFeature {..}
  where
    analyticsPixelsFeatureInterface  = (emptyHackageFeature "AnalyticsPixels") {
        featureDesc      = "Allow users to attach analytics pixels to their packages",
        featureResources = [analyticsPixelsResource, userAnalyticsPixelsResource]
      , featureState     = [abstractAcidStateComponent analyticsPixelsState]
      }

    analyticsPixelsResource :: Resource
    analyticsPixelsResource = resourceAt "/package/:package/analytics-pixels.:format"

    userAnalyticsPixelsResource :: Resource
    userAnalyticsPixelsResource = resourceAt "/user/:username/analytics-pixels.:format"

    getPackageAnalyticsPixels :: MonadIO m => PackageName -> m (Set AnalyticsPixel)
    getPackageAnalyticsPixels name = 
        queryState analyticsPixelsState (AnalyticsPixelsForPackage name)

    addPackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m Bool
    addPackageAnalyticsPixel name pixel = do
        added <- updateState analyticsPixelsState (AddPackageAnalyticsPixel name pixel)
        when added $ runHook_ analyticsPixelAdded (name, pixel)
        pure added

    removePackageAnalyticsPixel :: MonadIO m => PackageName -> AnalyticsPixel -> m ()
    removePackageAnalyticsPixel name pixel = do
        updateState analyticsPixelsState (RemovePackageAnalyticsPixel name pixel)
        runHook_ analyticsPixelRemoved (name, pixel)
