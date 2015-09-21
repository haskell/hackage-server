module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Test.HUnit as HUnit

import Distribution.Server.Packages.Unpack
import Distribution.Server.Packages.UnpackTest

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ( Test(..) )

goodMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
goodMangler = const Nothing
badFileMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
badFileMangler entry = case Tar.entryContent entry of
    (Tar.NormalFile _ _) -> Just $ PermissionsError (Tar.entryPath entry) 0o600
    _ -> Nothing
badDirMangler :: (Tar.Entry -> Maybe CombinedTarErrs)
badDirMangler entry = case Tar.entryContent entry of
    (Tar.Directory) -> Just $ PermissionsError (Tar.entryPath entry) 0o700
    _ -> Nothing

goodTest :: HUnit.Test
goodTest = testPermissions "tests/permissions-tarballs/good-perms.tar.gz" goodMangler
badFileTest :: HUnit.Test
badFileTest = testPermissions "tests/permissions-tarballs/bad-file-perms.tar.gz" badFileMangler
badDirTest :: HUnit.Test
badDirTest = testPermissions "tests/permissions-tarballs/bad-dir-perms.tar.gz" badDirMangler

tests :: HUnit.Test
tests = TestList [
      TestLabel "Good Permissions" goodTest
    , TestLabel "Bad File Permissions" badFileTest
    , TestLabel "Bad Dir Permissions" badDirTest ]

main :: IO ()
main = defaultMain $ hUnitTestToTests tests
