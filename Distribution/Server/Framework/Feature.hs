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
  , AbstractStateComponent(..)
  , abstractStateComponent
  , abstractStateComponent'
  , queryState
  , updateState
  , compareState
    -- * Cache components
  , CacheComponent(..)
    -- * Re-exports
  , BlobStorage
  ) where

import Distribution.Server.Framework.BackupRestore (RestoreBackup(..), AbstractRestoreBackup(..), BackupEntry, abstractRestoreBackup)
import Distribution.Server.Framework.Resource      (Resource)
import Distribution.Server.Framework.BlobStorage   (BlobStorage)
import Distribution.Server.Framework.MemSize

import Data.Monoid
import Control.Monad (liftM, liftM2)
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

    featurePostInit  = return (),

    featureState     = error $ "'featureState' not defined for feature '" ++ name ++ "'",
    featureCaches    = []
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature

--------------------------------------------------------------------------------
-- State components                                                           --
--------------------------------------------------------------------------------

-- | A state component encapsulates (part of) a feature's state
data StateComponent st = StateComponent {
    -- | Human readable description of the state component
    stateDesc    :: String
    -- | The underlying AcidState handle
  , acidState    :: AcidState st
    -- | Return the entire state
  , getState     :: IO st
    -- | Overwrite the state
  , putState     :: st -> IO ()
    -- | (Pure) backup function
  , backupState  :: st -> [BackupEntry]
    -- | (Pure) backup restore
  , restoreState :: RestoreBackup st
    -- | Clone the state component in the given state directory
  , resetState   :: FilePath -> IO (StateComponent st)
  }

-- | 'AbstractStateComponent' abstracts away from a particular type of
-- 'StateComponent'
data AbstractStateComponent = AbstractStateComponent {
    abstractStateDesc       :: String
  , abstractStateCheckpoint :: IO ()
  , abstractStateClose      :: IO ()
  , abstractStateBackup     :: IO [BackupEntry]
  , abstractStateRestore    :: AbstractRestoreBackup
  , abstractStateReset      :: FilePath -> IO (AbstractStateComponent, IO [String])
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

abstractStateComponent :: (Eq st, Show st, MemSize st) => StateComponent st -> AbstractStateComponent
abstractStateComponent = abstractStateComponent' compareState

abstractStateComponent' :: MemSize st => (st -> st -> [String]) -> StateComponent st -> AbstractStateComponent
abstractStateComponent' cmp st = AbstractStateComponent {
    abstractStateDesc       = stateDesc st
  , abstractStateCheckpoint = createCheckpoint (acidState st)
  , abstractStateClose      = closeAcidState (acidState st)
  , abstractStateBackup     = liftM (backupState st) (getState st)
  , abstractStateRestore    = abstractRestoreBackup (putState st) (restoreState st)
  , abstractStateReset      = \stateDir -> do
                                st' <- resetState st stateDir
                                let cmpSt = liftM2 cmp (getState st) (getState st')
                                return (abstractStateComponent' cmp st', cmpSt)
  , abstractStateSize       = liftM memSize (getState st)
  }

instance Monoid AbstractStateComponent where
  mempty = AbstractStateComponent {
      abstractStateDesc       = ""
    , abstractStateCheckpoint = return ()
    , abstractStateClose      = return ()
    , abstractStateBackup     = return []
    , abstractStateRestore    = mempty
    , abstractStateReset      = \_stateDir -> return (mempty, return [])
    , abstractStateSize       = return 0
    }
  a `mappend` b = AbstractStateComponent {
      abstractStateDesc       = abstractStateDesc a ++ "\n" ++ abstractStateDesc b
    , abstractStateCheckpoint = abstractStateCheckpoint a >> abstractStateCheckpoint b
    , abstractStateClose      = abstractStateClose a >> abstractStateClose b
    , abstractStateBackup     = liftM2 (++) (abstractStateBackup a) (abstractStateBackup b)
    , abstractStateRestore    = abstractStateRestore a `mappend` abstractStateRestore b
    , abstractStateReset      = \stateDir -> do
                                 (a', cmpA) <- abstractStateReset a stateDir
                                 (b', cmpB) <- abstractStateReset b stateDir
                                 return (a' `mappend` b', liftM2 (++) cmpA cmpB)
    , abstractStateSize       = liftM2 (+) (abstractStateSize a)
                                           (abstractStateSize b)
    }

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
