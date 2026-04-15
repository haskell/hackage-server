{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.AnalyticsPixels.Types
    ( AnalyticsPixel(..)
    ) where

import Distribution.Server.Framework.MemSize (MemSize)

import Data.Text (Text)

import Control.DeepSeq (NFData)

newtype AnalyticsPixel = AnalyticsPixel
    {
        analyticsPixelUrl :: Text
    }
    deriving (Show, Eq, Ord, NFData, MemSize)
