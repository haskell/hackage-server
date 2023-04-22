{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeApplications, ScopedTypeVariables #-}
module Main where

import qualified Data.Array as Arr
import qualified Data.Bimap as Bimap
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity(..))
import           Data.List (partition, foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

import Distribution.Package (PackageIdentifier(..), mkPackageName, packageId, packageName)
import Distribution.Server.Features.PreferredVersions.State (PreferredVersions(..), VersionStatus(NormalVersion), PreferredInfo(..))
import Distribution.Server.Features.ReverseDependencies (ReverseFeature(..), ReverseCount(..), reverseFeature)
import Distribution.Server.Features.ReverseDependencies.State (ReverseIndex(..), addPackage, constructReverseIndex, emptyReverseIndex, getDependenciesFlat, getDependencies, getDependenciesFlatRaw, getDependenciesRaw)
import Distribution.Server.Features.UserNotify (NotifyPref(..), NotifyTriggerBounds(..), defaultNotifyPrefs, dependencyReleaseEmails)
import Distribution.Server.Framework.Hook (newHook)
import Distribution.Server.Framework.MemState (newMemStateWHNF)
import Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Version (mkVersion, version0)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import           Hedgehog ((===), Group(Group), MonadGen, Property, PropertyT, checkSequential, forAll, property)

import RevDepCommon (Package(..), TestPackage(..), mkPackage, packToPkgInfo)

mtlBeelineLens :: [PkgInfo]
mtlBeelineLens =
  [ mkPackage "base" [4,15] []
  , mkPackage "mtl" [2,3] ["base"]
  -- Note that this example is a bit unrealistic
  -- since these two do not depend on base...
  , mkPackage "beeline" [0] ["mtl"]
  , mkPackage "lens" [0] ["mtl"]
  ]

twoPackagesWithNoDepsOutOfRange :: [PkgInfo]
twoPackagesWithNoDepsOutOfRange =
  [ mkPackage "base" [4,14] []
  , mkPackage "mtl" [2,3] ["base < 4.15"]
  ]

newBaseReleased :: [PkgInfo]
newBaseReleased =
  [ mkPackage "base" [4,14] []
  , mkPackage "base" [4,15] []
  , mkPackage "mtl" [2,3] ["base < 4.15"]
  ]

twoNewBasesReleased :: [PkgInfo]
twoNewBasesReleased =
  [ mkPackage "base" [4,14] []
  , mkPackage "base" [4,15] []
  , mkPackage "base" [4,16] []
  , mkPackage "mtl" [2,3] ["base < 4.15"]
  ]

mkRevFeat :: [PkgInfo] -> IO ReverseFeature
mkRevFeat pkgs = do
  let
    idx = PackageIndex.fromList pkgs
    preferredVersions =
      PreferredVersions
        { preferredMap = mempty
        , deprecatedMap = mempty
        , migratedEphemeralPrefs = False
        }
  updateReverse <- newHook
  let constructed = constructReverseIndex idx
  memState <- newMemStateWHNF constructed
  pure $
    reverseFeature
    (pure idx)
    (pure preferredVersions)
    memState
    updateReverse

allTests :: TestTree
allTests = testGroup "ReverseDependenciesTest"
  [ testCase "with set [beeline->mtl] and querying for mtl, we get beeline" $ do
      let pkgs =
            [ mkPackage "base" [4,15] []
            , mkPackage "mtl" [2,3] ["base"]
            , mkPackage "beeline" [0] ["mtl"]
            ]
      ReverseFeature{revPackageName} <- mkRevFeat pkgs
      res <- revPackageName "mtl"
      let ref = Map.fromList [("beeline", (version0, Just NormalVersion))]
      assertEqual "reverse dependencies must be [beeline]" ref res
  , testCase "revPackageName selects only the version with an actual dependency, even if it is not the newest" $ do
      let pkgs =
            [ mkPackage "base" [4,15] []
            , mkPackage "mtl" [2,3] ["base"]
            , mkPackage "mtl-tf" [9000] ["base"]
            , mkPackage "beeline" [0] ["mtl"]
            , mkPackage "beeline" [1] ["mtl-tf"]
            ]
      ReverseFeature{revPackageName} <- mkRevFeat pkgs
      res <- revPackageName "mtl"
      let ref = Map.fromList [("beeline", (mkVersion [0], Nothing))]
      assertEqual "reverse dependencies must be [beeline v0]" ref res
  , testCase "revPackageId does select old version when queried with old reverse dependency" $ do
      let mtl = mkPackage "mtl" [2,3] ["base"]
          pkgs =
            [ mkPackage "base" [4,15] []
            , mtl
            , mkPackage "mtl-tf" [9000] ["base"]
            , mkPackage "beeline" [0] ["mtl"]
            , mkPackage "beeline" [1] ["mtl-tf"]
            ]
      ReverseFeature{revPackageId} <- mkRevFeat pkgs
      res <- revPackageId (packageId mtl)
      -- Nothing because it is not the 'best version'
      let ref = Map.fromList [("beeline", (mkVersion [0], Nothing))]
      assertEqual "reverse dependencies must be [beeline v0]" ref res
  , testCase "revPackageName can find multiple packages" $ do
      let pkgs =
            [ mkPackage "base" [4,15] []
            , mkPackage "mtl" [2,3] ["base"]
            , mkPackage "beeline" [0] ["mtl"]
            , mkPackage "mario" [0] ["mtl"]
            ]
      ReverseFeature{revPackageName} <- mkRevFeat pkgs
      res <- revPackageName "mtl"
      let ref = Map.fromList [ ("beeline", (mkVersion [0], Just NormalVersion))
                             , ("mario",   (mkVersion [0], Just NormalVersion))
                             ]
      assertEqual "reverse dependencies must be [beeline v0, mario v0]" ref res
  , testCase "with set [beeline->mtl->base, lens->mtl->base], revPackageFlat 'base' finds [beeline, lens, mtl]" $ do
      ReverseFeature{revPackageFlat} <- mkRevFeat mtlBeelineLens
      res <- revPackageFlat "base"
      let ref = [ ("beeline", 0), ("lens", 0), ("mtl", 2) ]
      assertEqual "reverse dependencies must be [beeline v0, mario v0]" ref res
  , testCase "with set [beeline->mtl->base, lens->mtl->base], revPackageStats 'base' returns 1,3" $ do
      ReverseFeature{revPackageStats} <- mkRevFeat mtlBeelineLens
      res <- revPackageStats "base"
      let ref = ReverseCount { directCount = 1, totalCount = 3 }
      assertEqual "must be directCount=1, totalCount=3" ref res
  , testCase "with set [beeline->mtl->base, lens->mtl->base], queryReverseDeps 'base' returns [mtl],[beeline,lens]" $ do
      ReverseFeature{queryReverseDeps} <- mkRevFeat mtlBeelineLens
      res <- queryReverseDeps "base"
      let ref = (["mtl"], ["beeline", "lens"])
      assertEqual "must be direct=[mtl], indirect=[beeline,lens]" ref res
  , testCase "with set [beeline->mtl->base, lens->mtl->base], revCountForAllPackages returns [(base,1,3),(mtl,2,2),(beeline,0,0),(lens,0,0)]" $ do
      ReverseFeature{revCountForAllPackages} <- mkRevFeat mtlBeelineLens
      res <- revCountForAllPackages
      let ref = [("beeline",ReverseCount 0 0),("lens",ReverseCount 0 0),("base",ReverseCount 1 3),("mtl",ReverseCount 2 2)]
      assertEqual "must match reference" ref res
  , testCase "revDisplayInfo" $ do
      ReverseFeature{revDisplayInfo} <- mkRevFeat mtlBeelineLens
      res <- revDisplayInfo
      assertEqual "beeline preferred is old" (PreferredInfo [] [] Nothing, [mkVersion [0]]) (res "beeline")
  , testCase "dependencyReleaseEmails sends notification" $ do
      let userSetIdForPackage arg | arg == mkPackageName "mtl" = Identity (UserIdSet.fromList [UserId 0])
                                  | otherwise = error "should only get user ids for mtl"
          notifyPref triggerBounds =
            defaultNotifyPrefs
              { notifyDependencyForMaintained = True
              , notifyOptOut = False
              , notifyDependencyTriggerBounds = triggerBounds
              }
          pref triggerBounds (UserId 0) = Identity (Just $ notifyPref triggerBounds)
          pref _ _ = error "should only get preferences for UserId 0"
          refNotification base = Map.fromList
            [
              ( (UserId 0, base)
              , [PackageIdentifier (mkPackageName "mtl") (mkVersion [2,3])]
              )
            ]
          base4_14 = PackageIdentifier "base" (mkVersion [4,14])
          base4_15 = PackageIdentifier "base" (mkVersion [4,15])
          base4_16 = PackageIdentifier "base" (mkVersion [4,16])
          runWithPref preferences index pkg = runIdentity $
            dependencyReleaseEmails userSetIdForPackage index (constructReverseIndex index) preferences pkg
      assertEqual
        "dependencyReleaseEmails(trigger=NewIncompatibility) shouldn't generate a notification when there are packages, but none are behind"
        mempty
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList twoPackagesWithNoDepsOutOfRange) base4_14)
      assertEqual
        "dependencyReleaseEmails(trigger=NewIncompatibility) should generate a notification when package is a single base version behind"
        (refNotification base4_15)
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList newBaseReleased) base4_15)
      assertEqual
        "dependencyReleaseEmails(trigger=BoundsOutOfRange) should generate a notification when package is a single base version behind"
        (refNotification base4_15)
        (runWithPref (pref BoundsOutOfRange) (PackageIndex.fromList newBaseReleased) base4_15)
      assertEqual
        "dependencyReleaseEmails(trigger=NewIncompatibility) shouldn't generate a notification when package is two base versions behind"
        mempty
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList twoNewBasesReleased) base4_16)
      assertEqual
        "dependencyReleaseEmails(trigger=BoundsOutOfRange) should generate a notification when package is two base versions behind"
        (refNotification base4_16)
        (runWithPref (pref BoundsOutOfRange) (PackageIndex.fromList twoNewBasesReleased) base4_16)
      assertEqual
        "dependencyReleaseEmails(trigger=NewIncompatibility) should generate a notification when package is a single base version behind"
        (refNotification base4_15)
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList newBaseReleased) base4_15)
  , testCase "hedgehogTests" $ do
      res <- hedgehogTests
      assertEqual "hedgehog test pass" True res
  ]

genPacks :: PropertyT IO [Package TestPackage]
genPacks = do
  numPacks <- forAll $ Gen.int (Range.linear 1 10)
  allowMultipleVersions <- forAll Gen.bool -- remember that this shrinks to False
  packs <- forAll $ packsUntil allowMultipleVersions numPacks mempty
  pure packs

prop_constructRevDeps :: Property
prop_constructRevDeps = property $ do
  packs <- genPacks
  let idx = PackageIndex.fromList $ map packToPkgInfo packs
  let ReverseIndex foldedRevDeps foldedMap foldedDeps = foldl' (packageFolder idx) emptyReverseIndex packs
  let (ReverseIndex constructedRevDeps constructedMap constructedDeps) = constructReverseIndex idx
  for_ (PackageIndex.allPackageNames idx) $ \name -> do
    foundFolded :: Int <- Bimap.lookup name foldedMap
    foundConstructed :: Int <- Bimap.lookup name constructedMap
    -- they are not nessarily equal, since they may have been added in a different order!
    -- so this doesn't necessarily hold:
    -- foundFolded === foundConstructed

    -- but they should have the same deps
    foldedPackNames <- mapM (`Bimap.lookupR` foldedMap) (foldedRevDeps Arr.! foundFolded)
    constructedPackNames <- mapM (`Bimap.lookupR` constructedMap) (constructedRevDeps Arr.! foundConstructed)
    Set.fromList foldedPackNames === Set.fromList constructedPackNames

    foldedDeps === constructedDeps

prop_statsEqualsDeps :: Property
prop_statsEqualsDeps = property $ do
  packs <- genPacks
  let packages = map packToPkgInfo packs
  let revs = constructReverseIndex $ PackageIndex.fromList packages
  pkginfo <- forAll $ Gen.element packages
  let name = packageName pkginfo
  let directSet = getDependenciesRaw name revs
      totalSet = getDependenciesFlatRaw name revs
      directNames = getDependencies name revs
      totalNames = getDependenciesFlat name revs
  length directSet === length directNames
  length totalSet === length totalNames

packageFolder :: Show b => PackageIndex PkgInfo -> ReverseIndex -> Package b -> ReverseIndex
packageFolder index revindex (Package name _version deps) = addPackage index (mkPackageName $ show name) (map (mkPackageName . show) deps) revindex


genPackage :: forall m b. (MonadGen m, Enum b, Bounded b, Ord b) => b -> [Package b] -> m (Package b)
genPackage newName available = do
  pVersion <- Gen.int (Range.linear 0 10)
  depPacks :: [Package b] <- Gen.subsequence available
  pure $ Package {pName = newName, pVersion, pDeps = map pName depPacks }

packsUntil :: forall m b. (Ord b, Bounded b, MonadGen m, Enum b) => Bool -> Int -> [Package b] -> m [Package b]
packsUntil allowMultipleVersions limit generated | length generated < limit = do
  makeNewPack <- Gen.bool -- if not new pack, just make a new version of an existing
  toInsert <-
    if makeNewPack || generated == mempty || not allowMultipleVersions
      then
        genPackage (toEnum $ length generated) generated
      else do
        Package { pName = prevName } <- Gen.element generated
        let (prevNamePacks, nonPrevName) = partition ((== prevName) . pName) generated
        depPacks :: [Package b] <- Gen.subsequence nonPrevName
        let newVersion = 1 + maximum (map pVersion prevNamePacks)
        pure $ Package {pName = prevName, pVersion = newVersion, pDeps = map pName depPacks}
  let added = generated ++ [toInsert]
  packsUntil allowMultipleVersions limit added
packsUntil _ _ generated = pure generated

hedgehogTests :: IO Bool
hedgehogTests =
  checkSequential $ Group "hedgehogTests"
    [ ("prop_constructRevDeps", prop_constructRevDeps)
    , ("prop_statsEqualsDeps",  prop_statsEqualsDeps)
    ]

main :: IO ()
main = defaultMain allTests
