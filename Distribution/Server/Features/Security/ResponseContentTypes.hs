-- |  Response types used by the security feature
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.Security.ResponseContentTypes (
    TUFFile(..)
  , mkTUFFile
  , IsTUFFile(..)
  , Timestamp(..)
  , Snapshot(..)
  , Root(..)
  , Mirrors(..)
  ) where

-- stdlib
import Happstack.Server
import Control.DeepSeq
import Data.Typeable
import Data.SafeCopy
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.MD5
import Distribution.Server.Features.Security.SHA256
import Distribution.Server.Framework.ResponseContentTypes
import Distribution.Server.Framework.MemSize
import Text.JSON.Canonical (Int54)

-- | Serialized TUF file along with some metadata necessary to serve the file
data TUFFile = TUFFile {
    _tufFileContent    :: !BS.Lazy.ByteString
  , _tufFileLength     :: !Int54
  , _tufFileHashMD5    :: !MD5Digest
  , _tufFileHashSHA256 :: !SHA256Digest
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''TUFFile

instance NFData TUFFile where
  rnf (TUFFile a b c d) = rnf (a, b, c, d)

instance MemSize TUFFile where
  memSize (TUFFile a b c d) = memSize4 a b c d

instance ToMessage TUFFile where
  toResponse file =
    mkResponseLen (tufFileContent file) (fromIntegral (tufFileLength file)) [
        ("Content-Type", "application/json")
      , ("Content-MD5",   formatMD5Digest (tufFileHashMD5 file))
      ]

instance HasFileInfo TUFFile where
  fileInfo file = FileInfo (tufFileLength file) (tufFileHashSHA256 file) (Just $ tufFileHashMD5 file)

mkTUFFile :: BS.Lazy.ByteString -> TUFFile
mkTUFFile content =
    TUFFile {
      _tufFileContent    = content,
      _tufFileLength     = fromIntegral $ BS.Lazy.length content,
      _tufFileHashMD5    = md5     content,
      _tufFileHashSHA256 = sha256  content
    }

{-------------------------------------------------------------------------------
  Wrappers around TUFFile

  (We originally had a phantom type argument here indicating what kind of TUF
  file this is, but that lead to problems with 'deriveSafeCopy'; now we use a
  bunch of newtype wrappers instead.)
-------------------------------------------------------------------------------}

class IsTUFFile a where
  tufFileContent    :: a -> BS.Lazy.ByteString
  tufFileLength     :: a -> Int54
  tufFileHashMD5    :: a -> MD5Digest
  tufFileHashSHA256 :: a -> SHA256Digest

instance IsTUFFile TUFFile where
  tufFileContent    TUFFile{..} = _tufFileContent
  tufFileLength     TUFFile{..} = _tufFileLength
  tufFileHashMD5    TUFFile{..} = _tufFileHashMD5
  tufFileHashSHA256 TUFFile{..} = _tufFileHashSHA256

newtype Timestamp = Timestamp { timestampFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, MemSize,
            ToMessage, IsTUFFile, HasFileInfo)

newtype Snapshot = Snapshot { snapshotFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, MemSize,
            ToMessage, IsTUFFile, HasFileInfo)

newtype Root = Root { rootFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, MemSize,
            ToMessage, IsTUFFile, HasFileInfo)

newtype Mirrors = Mirrors { mirrorsFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, MemSize,
            ToMessage, IsTUFFile, HasFileInfo)

deriveSafeCopy 0 'base ''Timestamp
deriveSafeCopy 0 'base ''Snapshot
deriveSafeCopy 0 'base ''Root
deriveSafeCopy 0 'base ''Mirrors
