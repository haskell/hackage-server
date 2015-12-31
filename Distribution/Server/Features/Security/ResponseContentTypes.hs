-- |  Response types used by the security feature
{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Security.ResponseContentTypes (
    TUFFile(..)
  , Timestamp
  , Snapshot
  , Root
  , Mirrors
  ) where

-- stdlib
import Happstack.Server
import Control.DeepSeq
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Time
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Framework.ResponseContentTypes
import Distribution.Server.Features.Security.SHA256

-- hackage-security
import qualified Hackage.Security.Server as Sec

-- | TUF file
data TUFFile a = TUFFile {
    tufFileContent    :: !BS.Lazy.ByteString
  , tufFileLength     :: !Int
  , tufFileHashMD5    :: !MD5Digest
  , tufFileHashSHA256 :: !SHA256Digest
  , tufFileModified   :: !UTCTime
  , tufFileExpires    :: !UTCTime
  }

type Timestamp = TUFFile Sec.Timestamp
type Snapshot  = TUFFile Sec.Snapshot
type Root      = TUFFile Sec.Root
type Mirrors   = TUFFile Sec.Mirrors

instance NFData (TUFFile a) where
  rnf (TUFFile a b c d e f) = rnf (a, b, c, d, e, f)

instance ToMessage (TUFFile a) where
  toResponse TUFFile{..} =
    mkResponseLen tufFileContent tufFileLength [
        ("Content-Type", "text/json")
      , ("Content-MD5",   show tufFileHashMD5)
      , ("Last-modified", formatLastModifiedTime tufFileModified)
      ]
