-- | Test that Hackage accepts or refuses certain packages.

module Main
  ( main
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Time (getCurrentTime)
import Data.List (isInfixOf)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip

import Distribution.Server.Packages.Unpack
import Distribution.Server.Packages.UnpackTest

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, Assertion, HasCallStack)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "PackageTests"
    [ testGroup "Tar file permissions" tarPermissions
    , testGroup "Cabal package integrity tests" cabalPackageCheckTests
    ]

---------------------------------------------------------------------------
-- * File permission tests

tarPermissions :: [TestTree]
tarPermissions =
    [ testCase
        "Good Permissions"
        (testPermissions "tests/permissions-tarballs/good-perms.tar.gz" goodMangler)
    , testCase
        "Bad File Permissions"
        (testPermissions "tests/permissions-tarballs/bad-file-perms.tar.gz" badFileMangler)
    , testCase
        "Bad Dir Permissions"
        (testPermissions "tests/permissions-tarballs/bad-dir-perms.tar.gz" badDirMangler)
    ]

goodMangler :: (Tar.GenEntry content tarPath linkTarget -> Maybe CombinedTarErrs)
goodMangler = const Nothing

badFileMangler :: (Tar.GenEntry content FilePath linkTarget -> Maybe CombinedTarErrs)
badFileMangler entry =
  case Tar.entryContent entry of
    (Tar.NormalFile _ _) -> Just $ PermissionsError (Tar.entryTarPath entry) 0o600
    _ -> Nothing

badDirMangler :: (Tar.GenEntry content FilePath linkTarget -> Maybe CombinedTarErrs)
badDirMangler entry =
  case Tar.entryContent entry of
    Tar.Directory -> Just $ PermissionsError (Tar.entryTarPath entry) 0o700
    _ -> Nothing

---------------------------------------------------------------------------
-- * Package integry tests

cabalPackageCheckTests :: [TestTree]
cabalPackageCheckTests =
  -- Failing tests
    [ testCase "Missing ./configure script" missingConfigureScriptTest
    , testCase "Bad spec-version" badSpecVer
  -- Successful tests
    , testCase "Missing directories in tar file" missingDirsInTarFileTest
    , testCase "Accept GHC 9.2 LANGUAGE extensions" acceptGHC902LanguageExtensions
    ]

---------------------------------------------------------------------------
-- ** Tests that must fail

-- | If @build-type: Configure@, then there must be a @./configure@ script.

missingConfigureScriptTest :: Assertion
missingConfigureScriptTest =
  do tar <- tarGzFile "missing-configure-0.1.0.0"
     now <- getCurrentTime
     case unpackPackage now "missing-configure-0.1.0.0.tar.gz" tar of
       Right _ -> assertFailure "error: unexpected success"
       Left err ->
         assertBool
           ("Error found, but not about missing ./configure: " ++ err)
           ("The 'build-type' is 'Configure'" `isInfixOf` err)

-- | The @cabal-version@ must be valid.

badSpecVer :: Assertion
badSpecVer =
  do tar <- tarGzFile "bad-specver-package-0"
     now <- getCurrentTime
     case unpackPackage now "bad-specver-package-0.tar.gz" tar of
       Right _ -> assertFailure "error: unexpected success"
       Left err ->
         assertBool
           ("Error found, but not about invalid spec version: " ++ err)
           ("cabal spec version" `isInfixOf` err)

---------------------------------------------------------------------------
-- ** Tests that must succeed

-- | Some tar files in hackage are missing directory entries.
-- Ensure that they can be verified even without the directory entries.

missingDirsInTarFileTest :: Assertion
missingDirsInTarFileTest =
  successTestTGZ pkg =<< do keepOnlyFiles <$> tarGzFile pkg
  where
  pkg = "correct-package-0.1.0.0"

-- | Hackage should accept GHC 9.2 language extensions (issue #1030).

acceptGHC902LanguageExtensions :: Assertion
acceptGHC902LanguageExtensions = successTest "LANGUAGE-GHC-9.2"

---------------------------------------------------------------------------
-- * Auxiliary functions to construct tests

-- | A generic successful test, given a directory with the package contents.
--
-- Note: the 'HasCallStack' constraint ensures that the assertion failure
-- is thrown at the invocation site of this function.
--
successTest
  :: HasCallStack
  => String        -- ^ The directory which is also the package name.
  -> Assertion
successTest pkg = successTestTGZ pkg =<< tarGzFile pkg

-- | A successful test, given the package name and its @.tgz@ stream.
--
-- Note: the 'HasCallStack' constraint ensures that the assertion failure
-- is thrown at the invocation site of this function.
--
successTestTGZ
  :: HasCallStack
  => String        -- ^ The package name which is also the stem of the @.tgz@ file.
  -> ByteString    -- ^ The content of the @.tgz@ archive.
  -> Assertion
successTestTGZ pkg tar = do
  now <- getCurrentTime
  case unpackPackage now (pkg ++ ".tar.gz") tar of
    Right _ -> return ()
    Left err ->
      assertFailure $ "Expected success, but got: " ++ show err

---------------------------------------------------------------------------
-- * Tar utilities

tarGzFile :: String -> IO ByteString
tarGzFile name = do
  entries <- Tar.pack' "tests/unpack-checks" [name]
  tarcontents <- Tar.write' entries
  return (GZip.compress tarcontents)

-- | Remove all Tar.Entries that are not files.
keepOnlyFiles :: ByteString -> ByteString
keepOnlyFiles = GZip.compress . Tar.write . f . Tar.read . GZip.decompress
  where
    f = reverse . Tar.foldEntries step [] (error . show)
    step e acc =
      case Tar.entryContent e of
        Tar.NormalFile {} -> e : acc
        _ -> acc
