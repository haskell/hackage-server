{-# LANGUAGE StandaloneDeriving #-}
module Distribution.Server.Packages.UnpackTest (
  testPermissions,
  ) where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL

import Distribution.Server.Packages.Unpack

import Control.Monad (when)
import Test.HUnit

deriving instance Eq e => Eq (Tar.Entries e)
deriving instance Eq Tar.Entry
deriving instance Eq Tar.PortabilityError
deriving instance Eq Tar.FileNameError
deriving instance Eq Tar.FormatError
deriving instance Eq CombinedTarErrs

-- | Test that check permissions does the right thing
testPermissions :: FilePath ->  -- ^ .tar.gz file to test
  (Tar.Entry -> Maybe CombinedTarErrs) ->  -- ^ Converter to create errors if necessary
  Test
testPermissions tarPath mangler = TestCase $ do
    entries <- return . Tar.read . GZip.decompress =<< BL.readFile tarPath
    let mappedEntries = Tar.foldEntries Tar.Next Tar.Done (Tar.Fail . FormatError) entries
    when ((checkEntries mangler mappedEntries) /= checkUselessPermissions mappedEntries) $
        assertFailure ("Permissions check did not match expected for: " ++ tarPath)
