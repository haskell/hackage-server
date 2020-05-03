-- | This module defines a plugin interface for hackage features.
--
{-# LANGUAGE ExistentialQuantification, RankNTypes, NoMonomorphismRestriction, RecordWildCards #-}
module Distribution.Server.Framework.Feature
  ( -- * Main datatypes
    HackageFeature(..)
  , IsHackageFeature(..)
  , emptyHackageFeature
    -- * State components
  , StateComponent(..)
  , OnDiskState(..)
  , AbstractStateComponent(..)
  , abstractAcidStateComponent
  , abstractAcidStateComponent'
  , abstractOnDiskStateComponent
  , queryState
  , updateState
  , compareState
    -- * Cache components
  , CacheComponent(..)
    -- * Re-exports
  , BlobStorage
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework.BackupDump (BackupType (..))
import Distribution.Server.Framework.BackupRestore (RestoreBackup(..), AbstractRestoreBackup(..), BackupEntry, abstractRestoreBackup)
import Distribution.Server.Framework.Resource      (Resource, ServerErrorResponse)
import Distribution.Server.Framework.BlobStorage   (BlobStorage)
import Distribution.Server.Framework.MemSize

import Data.Acid
import Data.Acid.Advanced

-- | We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.
--
-- Features can hold their own canonical state and caches, and can provide a
-- set of resources.
--
data HackageFeature = HackageFeature {
    featureName        :: String
  , featureDesc        :: String
  , featureResources   :: [Resource]
  , featureErrHandlers :: [(String, ServerErrorResponse)]

  , featurePostInit    :: IO ()
  , featureReloadFiles :: IO ()

  , featureState       :: [AbstractStateComponent]
  , featureCaches      :: [CacheComponent]
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
    featureErrHandlers= [],

    featurePostInit  = return (),
    featureReloadFiles = return (),

    featureState     = error $ "'featureState' not defined for feature '" ++ name ++ "'",
    featureCaches    = []
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- State components                                                           --
--------------------------------------------------------------------------------

-- | A state component encapsulates (part of) a feature's state
data StateComponent f st = StateComponent {
    -- | Human readable description of the state component
    stateDesc    :: String
    -- | Handle required to access the state
  , stateHandle  :: f st
    -- | Return the entire state
  , getState     :: IO st
    -- | Overwrite the state
  , putState     :: st -> IO ()
    -- | (Pure) backup function
  , backupState  :: BackupType -> st -> [BackupEntry]
    -- | (Pure) backup restore
  , restoreState :: RestoreBackup st
    -- | Clone the state component in the given state directory
  , resetState   :: FilePath -> IO (StateComponent f st)
  }

data OnDiskState a = OnDiskState

-- | 'AbstractStateComponent' abstracts away from a particular type of
-- 'StateComponent'
data AbstractStateComponent = AbstractStateComponent {
    abstractStateDesc       :: String
  , abstractStateCheckpoint :: IO ()
  , abstractStateClose      :: IO ()
  , abstractStateBackup     :: BackupType -> IO [BackupEntry]
  , abstractStateRestore    :: AbstractRestoreBackup
  , abstractStateNewEmpty   :: FilePath -> IO (AbstractStateComponent, IO [String])
  , abstractStateSize       :: IO Int
  }

compareState :: (Eq st, Show st) => st -> st -> [String]
compareState old new =
    if old /= new
     then ["Internal state mismatch:\n" ++ difference (show old) (show new)]
     else []
  where
    difference old_str new_str
        -- = indent 2 old_str ++ "Versus:\n" ++ indent 2 new_str
        = "After " ++ show (length common)   ++ " chars, in context:\n" ++
            indent 2 (trunc_last 80 common)  ++ "\nOld data was:\n" ++
            indent 2 (trunc 80 old_str_tail) ++ "\nVersus new data:\n" ++
            indent 2 (trunc 80 new_str_tail)
      where (common, old_str_tail, new_str_tail) = dropCommonPrefix [] old_str new_str

    indent n = unlines . map (replicate n ' ' ++) . lines

    trunc n xs | null zs   = ys
               | otherwise = ys ++ "..."
      where (ys, zs) = splitAt n xs

    trunc_last n xs | null ys_rev = reverse zs_rev
                    | otherwise   = "..." ++ reverse zs_rev
      where (zs_rev, ys_rev) = splitAt n (reverse xs)

    dropCommonPrefix common (x:xs) (y:ys) | x == y = dropCommonPrefix (x:common) xs ys
    dropCommonPrefix common xs ys = (reverse common, xs, ys)

abstractAcidStateComponent :: (Eq st, Show st, MemSize st)
                           => StateComponent AcidState st -> AbstractStateComponent
abstractAcidStateComponent = abstractAcidStateComponent' compareState

abstractAcidStateComponent' :: MemSize st
                            => (st -> st -> [String])
                            -> StateComponent AcidState st -> AbstractStateComponent
abstractAcidStateComponent' cmp st = AbstractStateComponent {
    abstractStateDesc       = stateDesc st
  , abstractStateCheckpoint = createCheckpoint (stateHandle st) *> createArchive (stateHandle st)
  , abstractStateClose      = closeAcidState (stateHandle st)
  , abstractStateBackup     = \t -> liftM (backupState st t) (getState st)
  , abstractStateRestore    = abstractRestoreBackup (putState st) (restoreState st)
  , abstractStateNewEmpty   = \stateDir -> do
                                st' <- resetState st stateDir
                                let cmpSt = liftM2 cmp (getState st) (getState st')
                                return (abstractAcidStateComponent' cmp st', cmpSt)
  , abstractStateSize       = liftM memSize (getState st)
  }

abstractOnDiskStateComponent :: (Eq st, Show st) => StateComponent OnDiskState st -> AbstractStateComponent
abstractOnDiskStateComponent st = AbstractStateComponent {
    abstractStateDesc       = stateDesc st
  , abstractStateCheckpoint = return ()
  , abstractStateClose      = return ()
  , abstractStateBackup     = \t -> liftM (backupState st t) (getState st)
  , abstractStateRestore    = abstractRestoreBackup (putState st) (restoreState st)
  , abstractStateNewEmpty   = \stateDir -> do
                                st' <- resetState st stateDir
                                let cmpSt = liftM2 compareState (getState st) (getState st')
                                return (abstractOnDiskStateComponent st', cmpSt)
  , abstractStateSize       = return 0
  }

instance Monoid AbstractStateComponent where
  mempty = AbstractStateComponent {
      abstractStateDesc       = ""
    , abstractStateCheckpoint = return ()
    , abstractStateClose      = return ()
    , abstractStateBackup     = \_ -> return []
    , abstractStateRestore    = mempty
    , abstractStateNewEmpty   = \_stateDir -> return (mempty, return [])
    , abstractStateSize       = return 0
    }
  mappend = (<>)

instance Semigroup AbstractStateComponent where
  a <> b = AbstractStateComponent {
      abstractStateDesc       = abstractStateDesc a ++ "\n" ++ abstractStateDesc b
    , abstractStateCheckpoint = abstractStateCheckpoint a >> abstractStateCheckpoint b
    , abstractStateClose      = abstractStateClose a >> abstractStateClose b
    , abstractStateBackup     = \t -> liftM2 (++) (abstractStateBackup a t) (abstractStateBackup b t)
    , abstractStateRestore    = abstractStateRestore a <> abstractStateRestore b
    , abstractStateNewEmpty   = \stateDir -> do
                                 (a', cmpA) <- abstractStateNewEmpty a stateDir
                                 (b', cmpB) <- abstractStateNewEmpty b stateDir
                                 return (a' <> b', liftM2 (++) cmpA cmpB)
    , abstractStateSize       = liftM2 (+) (abstractStateSize a)
                                           (abstractStateSize b)
    }

queryState :: (MonadIO m, QueryEvent event)
           => StateComponent AcidState (EventState event)
           -> event
           -> m (EventResult event)
queryState = query' . stateHandle

updateState :: (MonadIO m, UpdateEvent event)
            => StateComponent AcidState (EventState event)
            -> event
            -> m (EventResult event)
updateState = update' . stateHandle


--------------------------------------------------------------------------------
-- Cache components                                                           --
--------------------------------------------------------------------------------

-- | A cache component encapsulates a cache, managed by a feature
data CacheComponent = CacheComponent {
    -- | Human readable description of the state component
    cacheDesc :: String

    -- | Get the current memory residency of the cache
  , getCacheMemSize :: IO Int
  }
