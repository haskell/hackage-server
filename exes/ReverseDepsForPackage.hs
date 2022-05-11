{-# LANGUAGE OverloadedStrings #-}
module Main where

import Distribution.Server.Features.Core (packagesStateComponent)
import Distribution.Server.Features.Core.State (packageIndex)
import Distribution.Server.Features.ReverseDependencies.State (constructReverseIndex, getReverseCount)
import Distribution.Server.Framework.Feature (getState)

main :: IO ()
main = do
  component <- packagesStateComponent maxBound False "state"
  st <- getState component
  let idx = packageIndex st
  revIdx <- constructReverseIndex idx
  print =<< getReverseCount "ADPfusion" revIdx
