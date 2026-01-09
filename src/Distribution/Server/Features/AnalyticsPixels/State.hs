{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.AnalyticsPixels.State 
    ( AnalyticsPixel(..)
    , AnalyticsPixelsState(..)
    , initialAnalyticsPixelsState

    -- * State queries and updates
    , AnalyticsPixelsForPackage(..)
    , AddPackageAnalyticsPixel(..)
    , RemovePackageAnalyticsPixel(..)
    , GetAnalyticsPixelsState(..)
    , ReplaceAnalyticsPixelsState(..)
    ) where

import Distribution.Package (PackageName)

import Distribution.Server.Framework.MemSize (MemSize)
import Distribution.Server.Users.State ()

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.DeepSeq (NFData)
import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask, asks)

newtype AnalyticsPixel = AnalyticsPixel
    {
        analyticsPixelUrl :: Text
    }
    deriving (Show, Eq, Ord, NFData, MemSize)

newtype AnalyticsPixelsState = AnalyticsPixelsState
    {
        analyticsPixels :: Map PackageName (Set AnalyticsPixel)
    }
  deriving (Show, Eq, NFData, MemSize)

-- SafeCopy instances
$(deriveSafeCopy 0 'base ''AnalyticsPixel)
$(deriveSafeCopy 0 'base ''AnalyticsPixelsState)

--

initialAnalyticsPixelsState :: AnalyticsPixelsState
initialAnalyticsPixelsState = AnalyticsPixelsState
    {
        analyticsPixels = Map.empty
    }

analyticsPixelsForPackage :: PackageName -> Query AnalyticsPixelsState (Set AnalyticsPixel)
analyticsPixelsForPackage name = asks $ Map.findWithDefault Set.empty name . analyticsPixels

-- | Adds a 'AnalyticsPixel' to a 'Package'. Returns 'True' if the pixel was inserted, and 'False' if
-- the 'AnalyticsPixel' was already present.
addPackageAnalyticsPixel :: PackageName -> AnalyticsPixel -> Update AnalyticsPixelsState Bool
addPackageAnalyticsPixel name analyticsPixel = do
    state <- State.get
    let (successfullyInserted, pixels) = Map.alterF insertAnalyticsPixel name (analyticsPixels state)
    State.put (state { analyticsPixels = pixels })
    pure successfullyInserted
    where
        insertAnalyticsPixel :: Maybe (Set AnalyticsPixel) -> (Bool, Maybe (Set AnalyticsPixel))
        insertAnalyticsPixel Nothing = 
            (True, Just (Set.singleton analyticsPixel))
        insertAnalyticsPixel existingPixels@(Just pixels)
            | analyticsPixel `Set.member` pixels = 
                (False, existingPixels)
            | otherwise = 
                (True, Just (Set.insert analyticsPixel pixels))

-- | Removes a 'AnalyticsPixel' from a 'Package'.
removePackageAnalyticsPixel :: PackageName -> AnalyticsPixel -> Update AnalyticsPixelsState ()
removePackageAnalyticsPixel name analyticsPixel = do
    state <- State.get
    let pixels = Map.alter removeAnalyticsPixel name (analyticsPixels state)
    State.put (state { analyticsPixels = pixels })
    pure ()
    where
        removeAnalyticsPixel Nothing =
            Nothing
        removeAnalyticsPixel (Just pixels) =
            let pixels' = analyticsPixel `Set.delete` pixels in
                if Set.null pixels' then Nothing else Just pixels'

-- get and replace the entire state, for backups

getAnalyticsPixelsState :: Query AnalyticsPixelsState AnalyticsPixelsState
getAnalyticsPixelsState = ask

replaceAnalyticsPixelsState :: AnalyticsPixelsState -> Update AnalyticsPixelsState ()
replaceAnalyticsPixelsState = State.put

makeAcidic
  ''AnalyticsPixelsState
  [ 'getAnalyticsPixelsState
  , 'analyticsPixelsForPackage
  , 'replaceAnalyticsPixelsState
  , 'addPackageAnalyticsPixel
  , 'removePackageAnalyticsPixel
  ]
