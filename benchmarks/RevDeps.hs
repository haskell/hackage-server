{-# LANGUAGE ScopedTypeVariables , TypeApplications #-}
module Main where

import           Control.Monad (replicateM)
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.Vector as Vector
import           Distribution.Package (packageName)
import           Distribution.Server.Features.ReverseDependencies.State (constructReverseIndex, getDependenciesFlat)
import           Distribution.Server.Packages.PackageIndex as PackageIndex

import           Gauge.Benchmark (nfAppIO, bench)
import           Gauge.Main (defaultMain)
import           System.Random.Stateful

import           RevDepCommon (Package(..), packToPkgInfo, TestPackage(..))

randomPacks
  :: forall m g. StatefulGen g m
  => g
  -> Int
  -> Vector.Vector (Package TestPackage)
  -> m (Vector.Vector (Package TestPackage))
randomPacks gen limit generated | length generated < limit = do
  makeNewPack <- uniformM gen -- if not new pack, just make a new version of an existing
  toInsert <-
    if makeNewPack || generated == mempty
      then
        Package
          <$> pure (TestPackage (fromIntegral @Int @Word $ Vector.length generated))
          <*> uniformRM (0, 10) gen
          <*> pure mempty
      else do
        prevIdx <- uniformRM (0, length generated - 1) gen
        let Package { pName = prevName } = generated Vector.! prevIdx
            (prevNamePacks, nonPrevName) = Vector.partition ((== prevName) . pName) generated
        depPacks <-
          if mempty /= nonPrevName
             then do
               -- TODO this should have an expected amount of deps equal to what is actually on hackage. what is it?
               numOfDeps <- uniformRM (1, min (length nonPrevName - 1) 7) gen
               indicesMayDuplicate <- replicateM numOfDeps (uniformRM (0, length nonPrevName - 1) gen)
               let indices = nubOrd indicesMayDuplicate
               pure $ map (nonPrevName Vector.!) indices
             else
               pure []
        let
          newVersion =
            if mempty /= prevNamePacks
               then 1 + maximum (fmap pVersion prevNamePacks)
               else 0
        pure $
          Package
            { pName = prevName
            , pVersion = newVersion
            , pDeps = map pName depPacks
            }
  let added = generated <> pure toInsert
  randomPacks gen limit added
randomPacks _ _ generated = pure generated

main :: IO ()
main = do
  packs :: Vector.Vector (Package TestPackage) <- randomPacks globalStdGen 20000 mempty
  let idx = PackageIndex.fromList $ map packToPkgInfo (Vector.toList packs)
  let revs = constructReverseIndex idx
  let numPacks = length packs
  defaultMain $
    (:[]) $
    bench "get transitive dependencies for one randomly selected package" $
    flip nfAppIO revs $ \revs' -> do
      select <- uniformRM (0, numPacks - 1) globalStdGen
      -- TODO why are there so many transitive deps?
      pure $ length $
        getDependenciesFlat
        (packageName $ packToPkgInfo (packs Vector.! select))
        revs'
