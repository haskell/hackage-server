-- |  Response types used by the security feature
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.Security.ResponseContentTypes (
    TUFFile(..)
  , IsTUFFile(..)
  , Timestamp(..)
  , Snapshot(..)
  , Root(..)
  , Mirrors(..)
  ) where

-- stdlib
import Happstack.Server
import Control.DeepSeq
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Time
import Data.Typeable
import Data.SafeCopy
import qualified Data.ByteString.Lazy as BS.Lazy

-- hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Orphans ()
import Distribution.Server.Features.Security.SHA256
import Distribution.Server.Framework.ResponseContentTypes
import Text.JSON.Canonical (Int54)

-- | Serialized TUF file along with some metadata necessary to serve the file
data TUFFile = TUFFile {
    _tufFileContent    :: !BS.Lazy.ByteString
  , _tufFileLength     :: !Int54
  , _tufFileHashMD5    :: !MD5Digest
  , _tufFileHashSHA256 :: !SHA256Digest
  , _tufFileModified   :: !UTCTime
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''TUFFile

instance NFData TUFFile where
  rnf (TUFFile a b c d e) = rnf (a, b, c, d, e)

instance ToMessage TUFFile where
  toResponse file =
    mkResponseLen (tufFileContent file) (fromIntegral (tufFileLength file)) [
        ("Content-Type", "application/json")
      , ("Content-MD5",   show (tufFileHashMD5 file))
      , ("Last-modified", formatLastModifiedTime (tufFileModified file))
      ]

instance HasFileInfo TUFFile where
  fileInfo file = FileInfo (tufFileLength file) (tufFileHashSHA256 file)

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
  tufFileModified   :: a -> UTCTime

instance IsTUFFile TUFFile where
  tufFileContent    TUFFile{..} = _tufFileContent
  tufFileLength     TUFFile{..} = _tufFileLength
  tufFileHashMD5    TUFFile{..} = _tufFileHashMD5
  tufFileHashSHA256 TUFFile{..} = _tufFileHashSHA256
  tufFileModified   TUFFile{..} = _tufFileModified

newtype Timestamp = Timestamp { timestampFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, ToMessage, IsTUFFile, HasFileInfo)

newtype Snapshot = Snapshot { snapshotFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, ToMessage, IsTUFFile, HasFileInfo)

newtype Root = Root { rootFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, ToMessage, IsTUFFile, HasFileInfo)

newtype Mirrors = Mirrors { mirrorsFile :: TUFFile }
  deriving (Typeable, Show, Eq, NFData, ToMessage, IsTUFFile, HasFileInfo)

deriveSafeCopy 0 'base ''Timestamp
deriveSafeCopy 0 'base ''Snapshot
deriveSafeCopy 0 'base ''Root
deriveSafeCopy 0 'base ''Mirrors
