module Main
  ( main
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Time (getCurrentTime)
import Data.List (isInfixOf)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Test.HUnit as HUnit

import Distribution.Server.Packages.Unpack
import Distribution.Server.Packages.UnpackTest

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, Assertion)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "PackageTests"
    [ testGroup "Tar file permissions" tarPermissions
    , testGroup "Cabal package integrity tests" cabalPackageCheckTests]

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
        (testPermissions "tests/permissions-tarballs/bad-dir-perms.tar.gz" badDirMangler)]

goodMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
goodMangler = const Nothing

badFileMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
badFileMangler entry =
  case Tar.entryContent entry of
    (Tar.NormalFile _ _) -> Just $ PermissionsError (Tar.entryPath entry) 0o600
    _ -> Nothing

badDirMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
badDirMangler entry =
  case Tar.entryContent entry of
    Tar.Directory -> Just $ PermissionsError (Tar.entryPath entry) 0o700
    _ -> Nothing

cabalPackageCheckTests :: [TestTree]
cabalPackageCheckTests =
    [ testCase "Missing ./configure script" missingConfigureScriptTest
    , testCase "Missing directories in tar file" missingDirsInTarFileTest
    , testCase "Bad spec-version" badSpecVer
    ]

missingConfigureScriptTest :: Assertion
missingConfigureScriptTest =
  do tar <- tarGzFile "missing-configure-0.1.0.0"
     now <- getCurrentTime
     case unpackPackage now "missing-configure-0.1.0.0.tar.gz" tar of
       Right _ -> HUnit.assertFailure "expected error"
       Left err ->
         HUnit.assertBool
           ("Error found, but not about missing ./configure: " ++ err)
           ("The 'build-type' is 'Configure'" `isInfixOf` err)

badSpecVer :: Assertion
badSpecVer =
  do tar <- tarGzFile "bad-specver-package-0"
     now <- getCurrentTime
     case unpackPackage now "bad-specver-package-0.tar.gz" tar of
       Right _ -> HUnit.assertFailure "expected error"
       Left err ->
         HUnit.assertBool
           ("Error found, but not about invalid spec version: " ++ err)
           ("cabal spec version" `isInfixOf` err)

-- | Some tar files in hackage are missing directory entries.
-- Ensure that they can be verified even without the directory entries.
missingDirsInTarFileTest :: Assertion
missingDirsInTarFileTest =
  do tar <- fmap keepOnlyFiles (tarGzFile "correct-package-0.1.0.0")
     now <- getCurrentTime
     case unpackPackage now "correct-package-0.1.0.0.tar.gz" tar of
       Right _ -> return ()
       Left err ->
         HUnit.assertFailure ("Excpected success but got: " ++ show err)

tarGzFile :: String -> IO ByteString
tarGzFile name = do
  entries <- Tar.pack "tests/unpack-checks" [name]
  return (GZip.compress (Tar.write entries))

-- | Remove all Tar.Entries that are not files.
keepOnlyFiles :: ByteString -> ByteString
keepOnlyFiles = GZip.compress . Tar.write . f . Tar.read . GZip.decompress
  where
    f = reverse . Tar.foldEntries step [] (error . show)
    step e acc =
      case Tar.entryContent e of
        Tar.NormalFile {} -> e : acc
        _ -> acc
