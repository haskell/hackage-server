-- | This module defines a plugin interface for hackage features.
--
{-# LANGUAGE ExistentialQuantification, RankNTypes, NoMonomorphismRestriction #-}
module Distribution.Server.Framework.Feature where

import Distribution.Server.Framework.BackupRestore (RestoreBackup(..), BackupEntry, TestRoundtrip)
import Distribution.Server.Framework.Resource      (Resource)
import Data.Function (fix)
import Control.Monad.Trans (MonadIO)
import Data.Acid
import Data.Acid.Advanced

-- | We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.
--
-- Features can hold their own canonical state and caches, and can provide a
-- set of resources.
--
-- Features that hold canonical state must support dump/restore by defining
-- 'featureDumpRestore' appropriately.
--
data HackageFeature = HackageFeature {
    featureName        :: String
  , featureDesc        :: String
  , featureResources   :: [Resource]

  , featurePostInit    :: IO ()

  , featureState       :: [SomeStateComponent]
  }

-- | A feature with no state and no resources, just a name.
--
-- Define your new feature by extending this one, e.g.
--
-- > myHackageFeature = emptyHackageFeature "wizzo" {
-- >     featureResources = [wizzo]
-- >   }
--
emptyHackageFeature :: String -> HackageFeature
emptyHackageFeature name = HackageFeature {
    featureName      = name,
    featureDesc      = "",
    featureResources = [],

    featurePostInit  = return (),

    featureState     = error $ "Feature state not defined for feature '" ++ name ++ "'"
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- State components                                                           --
--------------------------------------------------------------------------------

data StateComponent st = StateComponent {
    stateDesc    :: String
  , acidState    :: AcidState st
  , getState     :: IO st
  , backupState  :: st -> [BackupEntry]
  , restoreState :: RestoreBackup
  , testBackup   :: TestRoundtrip
  }

data SomeStateComponent = forall st. SomeStateComponent (StateComponent st)

queryState :: (MonadIO m, QueryEvent event)
           => StateComponent (EventState event)
           -> event
           -> m (EventResult event)
queryState = query' . acidState

updateState :: (MonadIO m, UpdateEvent event)
            => StateComponent (EventState event)
            -> event
            -> m (EventResult event)
updateState = update' . acidState

createCheckpointState :: SomeStateComponent -> IO ()
createCheckpointState (SomeStateComponent st) = createCheckpoint (acidState st)

closeState :: SomeStateComponent -> IO ()
closeState (SomeStateComponent st) = closeAcidState (acidState st)
