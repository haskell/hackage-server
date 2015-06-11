module Distribution.Server.Features.Security.ResponseContentTypes (
    Timestamp(..)
  , Snapshot(..)
  , Root(..)
  , Mirrors(..)
  ) where

-- stdlib
import Control.DeepSeq
import Happstack.Server
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Framework.ResponseContentTypes (mkResponseLen)

-- hackage-security
import qualified Data.Digest.Pure.SHA as SHA

-- TODOs:
--
-- * See note for TarballUncompressed about strictness and SHA.Digest
-- * MD5/Last-modified headers?

{-------------------------------------------------------------------------------
  Response types used by the security feature
-------------------------------------------------------------------------------}

data Timestamp = Timestamp {
    timestampContent :: !BS.Lazy.ByteString
  , timestampLength  :: !Int
  }

data Snapshot = Snapshot {
    snapshotContent    :: !BS.Lazy.ByteString
  , snapshotLength     :: !Int
  , snapshotHashSHA256 :: !(SHA.Digest SHA.SHA256State)
  }

data Root = Root {
    rootContent    :: !BS.Lazy.ByteString
  , rootLength     :: !Int
  , rootHashSHA256 :: !(SHA.Digest SHA.SHA256State)
  }

data Mirrors = Mirrors {
    mirrorsContent    :: !BS.Lazy.ByteString
  , mirrorsLength     :: !Int
  , mirrorsHashSHA256 :: !(SHA.Digest SHA.SHA256State)
  }

{-------------------------------------------------------------------------------
  NFData instances
-------------------------------------------------------------------------------}

instance NFData Snapshot where
  rnf (Snapshot a b _c) = rnf (a, b)

instance NFData Timestamp where
  rnf (Timestamp a b) = rnf (a, b)

instance NFData Root where
  rnf (Root a b _c) = rnf (a, b)

instance NFData Mirrors where
  rnf (Mirrors a b _c) = rnf (a, b)

{-------------------------------------------------------------------------------
  ToMessage instances
-------------------------------------------------------------------------------}

instance ToMessage Timestamp where
  toResponse (Timestamp bs len) =
    mkResponseLen bs len [
        ("Content-Type", "text/json")
      ]

instance ToMessage Snapshot where
  toResponse (Snapshot bs len _sha256) =
    mkResponseLen bs len [
        ("Content-Type", "text/json")
      ]

instance ToMessage Root where
  toResponse (Root bs len _sha256) =
    mkResponseLen bs len [
        ("Content-Type", "text/json")
      ]

instance ToMessage Mirrors where
  toResponse (Mirrors bs len _sha256) =
    mkResponseLen bs len [
        ("Content-Type", "text/json")
      ]
