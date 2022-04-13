{-# LANGUAGE StandaloneDeriving #-}
module Distribution.Server.Packages.UnpackTest (
  testPermissions,
  ) where

import Control.Monad (when)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Check as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL

import Distribution.Server.Packages.Unpack

import Test.HUnit (Assertion, assertFailure)

deriving instance Eq Tar.PortabilityError
deriving instance Eq Tar.FileNameError
deriving instance Eq CombinedTarErrs

-- | Test that check permissions does the right thing
testPermissions :: FilePath                              -- ^ .tar.gz file to test
                -> (Tar.Entry -> Maybe CombinedTarErrs)  -- ^ Converter to create errors if necessary
                -> Assertion
testPermissions tarPath mangler = do
    entries <- Tar.read . GZip.decompress <$> BL.readFile tarPath
    let mappedEntries = Tar.foldEntries Tar.Next Tar.Done (Tar.Fail . FormatError) entries
    when (checkEntries mangler mappedEntries /= checkUselessPermissions mappedEntries) $
        assertFailure ("Permissions check did not match expected for: " ++ tarPath)
