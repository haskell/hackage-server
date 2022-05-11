{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.Exit (die)
import Data.Maybe (isJust)
import Data.List (find)
import Control.Monad (when)
import Data.Map (toList)
import Distribution.Package
import Distribution.Version
import Distribution.Server.Features.Core (packagesStateComponent)
import Distribution.Server.Features.Core.State (packageIndex)
import Distribution.Server.Features.ReverseDependencies.State (constructReverseIndex, getDisplayInfo, perPackageReverse, dependsOnPkg)
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Features.PreferredVersions.State
import Distribution.Server.Framework.Feature (getState, queryState)

main :: IO ()
main = do
  component <- packagesStateComponent maxBound False "state"
  st <- getState component
  let idx = packageIndex st
  revIdx <- constructReverseIndex idx

  prefState <- preferredStateComponent False "state"
  prefVersions <- queryState prefState GetPreferredVersions
  let indexFunc = getDisplayInfo prefVersions idx
  print $ indexFunc "ghc-prim"
  print $ indexFunc "containers"
  revDeps <- perPackageReverse indexFunc idx revIdx "ghc-prim"
  -- Newer versions of 'containers' stopped depending on ghc-prim. So we expect 'containers' to not show up here.
  print (toList revDeps)
  print (dependsOnPkg idx (PackageIdentifier "ghc-prim" (mkVersion [0,6,0,1])) "containers")
  let found = isJust $ find (== ("containers", (mkVersion [0,6,0,1], Nothing))) (toList revDeps)
  when (not found) $ do
    print revDeps
    die "error!"

