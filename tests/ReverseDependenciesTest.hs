{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State as State
import qualified Data.Array as Arr
import qualified Data.Bimap as Bimap
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteStringL
import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity(..))
import           Data.List (partition, foldl')
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Time as Time
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Network.Mail.Mime as Mail
import           Network.URI (parseURI)
import           System.Random (mkStdGen)

import Distribution.Package (PackageIdentifier(..), mkPackageName, packageId, packageName)
import Distribution.Server.Features.PreferredVersions.State (PreferredVersions(..), VersionStatus(NormalVersion), PreferredInfo(..))
import Distribution.Server.Features.ReverseDependencies (ReverseFeature(..), ReverseCount(..), reverseFeature)
import Distribution.Server.Features.ReverseDependencies.State (ReverseIndex(..), addPackage, constructReverseIndex, emptyReverseIndex, getDependenciesFlat, getDependencies, getDependenciesFlatRaw, getDependenciesRaw)
import Distribution.Server.Features.Tags (Tag(..))
import Distribution.Server.Features.UserDetails (AccountDetails(..), UserDetailsFeature(..), dbQueryUserDetails)
import Distribution.Server.Features.UserNotify
  ( Notification(..)
  , NotifyMaintainerUpdateType(..)
  , NotifyData(..)
  , NotifyPref(..)
  , NotifyRevisionRange(..)
  , NotifyTriggerBounds(..)
  , defaultNotifyPrefs
  , getNotificationEmails
  , getUserNotificationsOnRelease
  , importNotifyPref
  , notifyDataToCSV
  )
import Distribution.Server.Framework.BackupRestore (runRestore)
import Distribution.Server.Framework.Hook (newHook)
import Distribution.Server.Framework.MemState (newMemStateWHNF)
import Distribution.Server.Framework.ServerEnv (ServerEnv(..))
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (CabalFileText(..), PkgInfo(..))
import Distribution.Server.Framework.Templating
import Distribution.Server.Users.Types
  ( PasswdHash(..)
  , UserAuth(..)
  , UserId(..)
  , UserName(..)
  )
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import qualified Distribution.Server.Users.Users as Users
import Distribution.Version (mkVersion, version0)

import Test.Tasty (TestName, TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import           Hedgehog
  ( (===)
  , Group(Group)
  , MonadGen
  , Property
  , PropertyT
  , Range
  , checkSequential
  , evalIO
  , forAll
  , property
  , withTests
  )

import RevDepCommon (Package(..), TestPackage(..), mkPackage, mkPackageWithCabalFileSuffix, packToPkgInfo)

import Data.String (fromString)
import Distribution.Server.Features.Database
import Distribution.Server.Features.UserDetails.State
import qualified Database.SQLite.Simple
import Database.Beam
import Control.Exception (bracket)

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

newBaseReleasedMultiple :: [PkgInfo]
newBaseReleasedMultiple =
  [ mkPackage "base" [4,14] []
  , mkPackage "base" [4,15] []
  , mkPackage "mtl" [2,3] ["base < 4.15"]
  , mkPackage "mtl2" [2,3] ["base < 4.15"]
  ]

newVersionOfOldBase :: [PkgInfo]
newVersionOfOldBase =
  [ mkPackage "base" [4,14] []
  , mkPackage "base" [4,14,1] []
  , mkPackage "base" [4,15] []
  , mkPackage "mtl" [2,3] ["base >= 4.15"]
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
  , testCase "with set [beeline->mtl, beeline2->mtl] and querying for mtl, we get [beeline, beeline2]" $ do
      let pkgs =
            [ mkPackage "base" [4,15] []
            , mkPackage "mtl" [2,3] ["base"]
            , mkPackage "beeline" [0] ["mtl"]
            , mkPackage "beeline2" [0] ["mtl"]
            ]
      ReverseFeature{revPackageName} <- mkRevFeat pkgs
      res <- revPackageName "mtl"
      let ref = Map.fromList [("beeline", (version0, Just NormalVersion)), ("beeline2", (version0, Just NormalVersion))]
      assertEqual "reverse dependencies must be [beeline, beeline2]" ref res
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
  , testCase "getUserNotificationsOnRelease sends notification" $ do
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
          userNotification = Map.fromList
            [
              ( UserId 0
              , [PackageIdentifier (mkPackageName "mtl") (mkVersion [2,3])]
              )
            ]
          base4_14 = PackageIdentifier "base" (mkVersion [4,14])
          base4_14_1 = PackageIdentifier "base" (mkVersion [4,14,1])
          base4_15 = PackageIdentifier "base" (mkVersion [4,15])
          base4_16 = PackageIdentifier "base" (mkVersion [4,16])
          runWithPref preferences index pkg = runIdentity $
            getUserNotificationsOnRelease userSetIdForPackage index (constructReverseIndex index) preferences pkg
          runWithPrefAlsoMtl2 preferences index pkg = runIdentity $
            getUserNotificationsOnRelease userSet index (constructReverseIndex index) preferences pkg
              where
              userSet arg | arg == mkPackageName "mtl" = Identity (UserIdSet.fromList [UserId 0])
                          | arg == mkPackageName "mtl2" = Identity (UserIdSet.fromList [UserId 0])
                          | otherwise = error "should only get user ids for mtl and mtl2"
      assertEqual
        "getUserNotificationsOnRelease(trigger=NewIncompatibility) shouldn't generate a notification when there are packages, but none are behind"
        mempty
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList twoPackagesWithNoDepsOutOfRange) base4_14)
      assertEqual
        "getUserNotificationsOnRelease(trigger=NewIncompatibility) should generate a notification when package is a single base version behind"
        userNotification
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList newBaseReleased) base4_15)
      assertEqual
        "getUserNotificationsOnRelease(trigger=NewIncompatibility) should generate a notification for two packages that are a single base version behind"
        (Just $
           Set.fromList
             [ PackageIdentifier (mkPackageName "mtl") (mkVersion [2,3])
             , PackageIdentifier (mkPackageName "mtl2") (mkVersion [2,3])
             ]
        )
        ( fmap Set.fromList
          . Map.lookup (UserId 0)
          $ runWithPrefAlsoMtl2 (pref NewIncompatibility) (PackageIndex.fromList newBaseReleasedMultiple) base4_15
        )
      assertEqual
        "getUserNotificationsOnRelease(trigger=BoundsOutOfRange) should generate a notification when package is a single base version behind"
        userNotification
        (runWithPref (pref BoundsOutOfRange) (PackageIndex.fromList newBaseReleased) base4_15)
      assertEqual
        "getUserNotificationsOnRelease(trigger=NewIncompatibility) shouldn't generate a notification when package is two base versions behind"
        mempty
        (runWithPref (pref NewIncompatibility) (PackageIndex.fromList twoNewBasesReleased) base4_16)
      assertEqual
        "getUserNotificationsOnRelease(trigger=BoundsOutOfRange) should generate a notification when package is two base versions behind"
        userNotification
        (runWithPref (pref BoundsOutOfRange) (PackageIndex.fromList twoNewBasesReleased) base4_16)
      assertEqual
        "getUserNotificationsOnRelease(trigger=BoundsOutOfRange) shouldn't generate a notification when the new package is for an old release series"
        mempty
        (runWithPref (pref BoundsOutOfRange) (PackageIndex.fromList newVersionOfOldBase) base4_14_1)
      assertEqual
        "getUserNotificationsOnRelease(trigger=BoundsOutOfRange) should only generate a notification when the new version is forbidden across all branches"
        mempty -- The two branches below should get OR'd and therefore the dependency is not out of bounds
        (runWithPref
          (pref BoundsOutOfRange)
          (PackageIndex.fromList
            [ mkPackage "base" [4,14] []
            , mkPackage "base" [4,15] []
            , mkPackageWithCabalFileSuffix "mtl" [2,3]
                "library\n\
                \  if arch(arm)\n\
                \    build-depends: base >= 4.14 && < 4.15\n\
                \  else\n\
                \    build-depends: base >= 4.15 && < 4.16"
            ])
          base4_15)
  , getNotificationEmailsTests
  , testCase "hedgehogTests" $ do
      res <- hedgehogTests
      assertEqual "hedgehog test pass" True res
  ]

setupTestDatabase :: IO (Database.SQLite.Simple.Connection, DatabaseFeature)
setupTestDatabase = do
  conn <- Database.SQLite.Simple.open ":memory:"
  Database.SQLite.Simple.execute_ conn initDbSql
  pure (conn, DatabaseFeature { 
      databaseFeatureInterface = undefined, -- not needed for these tests
      withTransaction = \transaction -> 
            liftIO $ Database.SQLite.Simple.withTransaction conn $
                          runTransaction
                            transaction
                            (SqlLiteConnection conn)
  })

tearDownTestDatabase :: (Database.SQLite.Simple.Connection, DatabaseFeature) -> IO ()
tearDownTestDatabase (conn, _) = Database.SQLite.Simple.close conn
  
withTestDatabase :: (IO DatabaseFeature -> TestTree) -> TestTree
withTestDatabase action = do
  withResource setupTestDatabase tearDownTestDatabase
    (\ ioResource -> action (snd <$> ioResource))

getNotificationEmailsTests :: TestTree
getNotificationEmailsTests =
  testGroup "getNotificationEmails"
    [ testProperty "All general notifications batched in one email" . withTests 30 . property $ do
        notifs <- forAll $ Gen.list (Range.linear 1 10) $ Gen.filterT isGeneral genNotification
        emails <- evalIO $ bracket setupTestDatabase tearDownTestDatabase
                    (\ (_, database) -> do
                      withTransaction database seedDatabase
                      getNotificationEmailsMocked database $ map (userWatcher,) notifs
                    )
        length emails === 1
    , testGolden "Render NotifyNewVersion" "getNotificationEmails-NotifyNewVersion.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyNewVersion
            { notifyPackageInfo =
                PkgInfo
                  { pkgInfoId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
                  , pkgMetadataRevisions = Vector.singleton (CabalFileText "", (timestamp, userActor))
                  , pkgTarballRevisions = mempty
                  }
            }
    , testGolden "Render NotifyNewRevision" "getNotificationEmails-NotifyNewRevision.golden" $ \database -> do
        withTransaction database seedDatabase
        let mkRev rev = (CabalFileText "", (rev, userActor))
            rev0 = (0 * Time.nominalDay) `Time.addUTCTime` timestamp
            rev1 = (1 * Time.nominalDay) `Time.addUTCTime` timestamp
            rev2 = (2 * Time.nominalDay) `Time.addUTCTime` timestamp
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyNewRevision
            { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
            , notifyRevisions = map (, userActor) [rev1, rev2]
            }
    , testGolden "Render NotifyMaintainerUpdate-MaintainerAdded" "getNotificationEmails-NotifyMaintainerUpdate-MaintainerAdded.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyMaintainerUpdate
            { notifyMaintainerUpdateType = MaintainerAdded
            , notifyUserActor = userActor
            , notifyUserSubject = userSubject
            , notifyPackageName = "base"
            , notifyReason = "User is cool"
            , notifyUpdatedAt = timestamp
            }
    , testGolden "Render NotifyMaintainerUpdate-MaintainerRemoved" "getNotificationEmails-NotifyMaintainerUpdate-MaintainerRemoved.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyMaintainerUpdate
            { notifyMaintainerUpdateType = MaintainerRemoved
            , notifyUserActor = userActor
            , notifyUserSubject = userSubject
            , notifyPackageName = "base"
            , notifyReason = "User is no longer cool"
            , notifyUpdatedAt = timestamp
            }
    , testGolden "Render NotifyDocsBuild-success" "getNotificationEmails-NotifyDocsBuild-success.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyDocsBuild
            { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
            , notifyBuildSuccess = True
            }
    , testGolden "Render NotifyDocsBuild-failure" "getNotificationEmails-NotifyDocsBuild-failure.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyDocsBuild
            { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
            , notifyBuildSuccess = False
            }
    , testGolden "Render NotifyUpdateTags" "getNotificationEmails-NotifyUpdateTags.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail . getNotificationEmailMocked database userWatcher $
          NotifyUpdateTags
            { notifyPackageName = "base"
            , notifyAddedTags = Set.fromList . map Tag $ ["bsd3", "library", "prelude"]
            , notifyDeletedTags = Set.fromList . map Tag $ ["example", "bad", "foo"]
            }
    , testGolden "Render NotifyDependencyUpdate-Always" "getNotificationEmails-NotifyDependencyUpdate-Always.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail
          . getNotificationEmail
              testServerEnv
              database
              testUserDetailsFeature
              allUsers
              userWatcher
          $ NotifyDependencyUpdate
              { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
              , notifyWatchedPackages = [PackageIdentifier "mtl" (mkVersion [2, 3])]
              , notifyTriggerBounds = Always
              }
    , testGolden "Render NotifyDependencyUpdate-NewIncompatibility" "getNotificationEmails-NotifyDependencyUpdate-NewIncompatibility.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail
          . getNotificationEmail
              testServerEnv
              database
              testUserDetailsFeature
              allUsers
              userWatcher
          $ NotifyDependencyUpdate
              { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
              , notifyWatchedPackages = [PackageIdentifier "mtl" (mkVersion [2, 3])]
              , notifyTriggerBounds = NewIncompatibility
              }
    , testGolden "Render NotifyDependencyUpdate-BoundsOutOfRange" "getNotificationEmails-NotifyDependencyUpdate-BoundsOutOfRange.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail
          . getNotificationEmail
              testServerEnv
              database
              testUserDetailsFeature
              allUsers
              userWatcher
          $ NotifyDependencyUpdate
              { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
              , notifyWatchedPackages = [PackageIdentifier "mtl" (mkVersion [2, 3])]
              , notifyTriggerBounds = BoundsOutOfRange
              }
    , testGolden "Render NotifyVouchingCompleted" "getNotificationEmails-NotifyVouchingCompleted.golden" $ \database -> do
        withTransaction database seedDatabase
        fmap renderMail $ getNotificationEmailMocked database userWatcher NotifyVouchingCompleted
    , testGolden "Render general notifications in single batched email" "getNotificationEmails-batched.golden" $ \database -> do
        withTransaction database seedDatabase
        emails <-
          getNotificationEmailsMocked database . map (userWatcher,) $
            [ NotifyNewRevision
                { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
                , notifyRevisions = [(timestamp, userActor)]
                }
            , NotifyDocsBuild
                { notifyPackageId = PackageIdentifier "base" (mkVersion [4, 18, 0, 0])
                , notifyBuildSuccess = True
                }
            , NotifyUpdateTags
                { notifyPackageName = "base"
                , notifyAddedTags = Set.fromList [Tag "newtag"]
                , notifyDeletedTags = Set.fromList [Tag "oldtag"]
                }
            ]
        case emails of
          [email] -> pure $ renderMail email
          _ -> error $ "Emails were not batched: " ++ show emails
    ]
  where
    -- If adding a new constructor here, make sure to do the following:
    --   * update genNotification
    --   * add a golden test above
    --   * add the golden file to hackage-server.cabal
    _allNotificationTypes = \case
      NotifyNewVersion{} -> ()
      NotifyNewRevision{} -> ()
      NotifyMaintainerUpdate{} -> ()
      NotifyDocsBuild{} -> ()
      NotifyUpdateTags{} -> ()
      NotifyDependencyUpdate{} -> ()
      NotifyVouchingCompleted{} -> ()

    isGeneral = \case
      NotifyNewVersion{} -> True
      NotifyNewRevision{} -> True
      NotifyMaintainerUpdate{} -> True
      NotifyDocsBuild{} -> True
      NotifyUpdateTags{} -> True
      NotifyDependencyUpdate{} -> False
      NotifyVouchingCompleted{} -> True

    -- userWatcher = user getting the notification
    -- userActor   = user that did the action
    -- userSubject = user the action is about
    ((userWatcher, userActor, userSubject), allUsers) =
      (`State.runState` Users.emptyUsers) $ do
        let addUser name = State.StateT $ \users0 ->
              case Users.addUserEnabled (UserName name) (UserAuth $ PasswdHash "") users0 of
                Right (users1, uid) -> pure (uid, users1)
                Left _ -> error $ "Got duplicate username: " <> name
        (,,)
          <$> addUser "user-watcher"
          <*> addUser "user-actor"
          <*> addUser "user-subject"

    seedDatabase :: Transaction ()
    seedDatabase = do
      Distribution.Server.Features.Database.runInsert $
        insert (_tblAccountDetails hackageDb) $
          insertValues
            [ AccountDetailsRow
                { _adUserId = (\(UserId v) -> fromIntegral v) userWatcher
                , _adName = "user-watcher"
                , _adContactEmail = "user-watcher@example.com"
                , _adKind = Nothing
                , _adAdminNotes = ""
                }
            ]

    getNotificationEmail env database details users uid notif =
      getNotificationEmails env database details users (mockTemplates ["datafiles/templates/UserNotify"] ["endorsements-complete.txt"]) [(uid, notif)] >>= \case
        [email] -> pure email
        _ -> error "Did not get exactly one email"

    testServerEnv =
      ServerEnv
        { serverBaseURI = fromJust $ parseURI "https://hackage.haskell.org"
        }

    testUserDetailsFeature =
      UserDetailsFeature
        { userDetailsFeatureInterface = undefined,
          queryUserDetails = dbQueryUserDetails,
          updateUserDetails = undefined
        }
    getNotificationEmailsMocked database =
      getNotificationEmails
        testServerEnv
        database
        testUserDetailsFeature
        allUsers
        (mockTemplates ["datafiles/templates/UserNotify"] ["endorsements-complete.txt"])
    getNotificationEmailMocked database =
      getNotificationEmail
        testServerEnv
        database
        testUserDetailsFeature
        allUsers

    renderMail = fst . Mail.renderMail (mkStdGen 0)
    timestamp = Time.UTCTime (Time.fromGregorian 2020 1 1) 0

    genNotification =
      Gen.choice
        [ NotifyNewVersion
            <$> genPkgInfo
        , NotifyNewRevision
            <$> genPackageId
            <*> Gen.list (Range.linear 1 5) genUploadInfo
        , NotifyMaintainerUpdate
            <$> Gen.element [MaintainerAdded, MaintainerRemoved]
            <*> genNonExistentUserId
            <*> genNonExistentUserId
            <*> genPackageName
            <*> Gen.text (Range.linear 1 20) Gen.unicode
            <*> genUTCTime
        , NotifyDocsBuild
            <$> genPackageId
            <*> Gen.bool
        , NotifyUpdateTags
            <$> genPackageName
            <*> Gen.set (Range.linear 1 5) genTag
            <*> Gen.set (Range.linear 1 5) genTag
        , NotifyDependencyUpdate
            <$> genPackageId
            <*> Gen.list (Range.linear 1 10) genPackageId
            <*> Gen.element [Always, NewIncompatibility, BoundsOutOfRange]
        , pure NotifyVouchingCompleted
        ]

    genPackageName = mkPackageName <$> Gen.string (Range.linear 1 30) Gen.unicode
    genVersion = mkVersion <$> Gen.list (Range.linear 1 4) (Gen.int $ Range.linear 0 50)
    genPackageId = PackageIdentifier <$> genPackageName <*> genVersion
    genCabalFileText = CabalFileText . ByteStringL.fromStrict <$> Gen.utf8 (Range.linear 0 50000) Gen.unicode
    genNonExistentUserId = UserId <$> Gen.int (Range.linear (-1000) (-1))
    genUploadInfo = (,) <$> genUTCTime <*> genNonExistentUserId
    genTag = Tag <$> Gen.string (Range.linear 1 10) Gen.unicode
    genPkgInfo =
      PkgInfo
        <$> genPackageId
        <*> genVec (Range.linear 1 5) ((,) <$> genCabalFileText <*> genUploadInfo)
        <*> pure Vector.empty -- ignoring pkgTarballRevisions for now

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

genRevisionRange :: MonadGen m => m NotifyRevisionRange
genRevisionRange = Gen.enumBounded

genDependencyTriggerBounds :: MonadGen m => m NotifyTriggerBounds
genDependencyTriggerBounds = Gen.enumBounded

genUidPref :: MonadGen m => m (UserId, NotifyPref)
genUidPref = do
  uid <- UserId <$> Gen.int (Range.linear 0 100)
  pref <-
    NotifyPref
      <$> Gen.bool
      <*> genRevisionRange
      <*> Gen.bool
      <*> Gen.bool
      <*> Gen.bool
      <*> Gen.bool
      <*> Gen.bool
      <*> genDependencyTriggerBounds
  pure (uid, pref)

prop_csvBackupRoundtrips :: Property
prop_csvBackupRoundtrips = property $ do
  prefMap <- forAll $ Gen.map (Range.linear 0 10) genUidPref
  let csv = notifyDataToCSV (error "unused backupType") (NotifyData (prefMap, error "unused timestamp"))
  Right listOfMappings <- liftIO $ runRestore (error "unused blobStores") (importNotifyPref csv)
  prefMap === Map.fromList listOfMappings

hedgehogTests :: IO Bool
hedgehogTests =
  checkSequential $ Group "hedgehogTests"
    [ ("prop_constructRevDeps", prop_constructRevDeps)
    , ("prop_statsEqualsDeps",  prop_statsEqualsDeps)
    , ("prop_csvBackupRoundtrips", prop_csvBackupRoundtrips)
    ]

testGolden :: TestName -> FilePath -> (DatabaseFeature -> IO Lazy.ByteString) -> TestTree
testGolden name fp body = 
  withTestDatabase $ \getDatabase ->
    goldenVsString name ("tests/golden/ReverseDependenciesTest/" <> fp) (do
      database <- getDatabase
      body database
    )
  

main :: IO ()
main = defaultMain allTests

{----- Utilities -----}

genUTCTime :: MonadGen m => m Time.UTCTime
genUTCTime =
  Time.UTCTime
    <$> genDay
    <*> genDiffTime
  where
    genDay =
      Time.fromGregorian
        <$> Gen.integral (Range.linear 0 3000)
        <*> Gen.int (Range.linear 1 12)
        <*> Gen.int (Range.linear 1 31)
    genDiffTime = realToFrac <$> Gen.double (Range.linearFrac 0 86400)

genVec :: MonadGen m => Range Int -> m a -> m (Vector a)
genVec r = fmap Vector.fromList . Gen.list r
