-- | Extract file info from response types
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Distribution.Server.Features.Security.FileInfo (
    FileInfo(..)
  , HasFileInfo(..)
  , secFileInfo
  , toSecFileInfo
  ) where

-- stdlib
import Data.Typeable
import Data.SafeCopy
import qualified Data.Map as Map

-- hackage
import Distribution.Server.Features.Security.SHA256
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.ResponseContentTypes
import Distribution.Server.Packages.Types

-- hackage-security
import qualified Hackage.Security.Server as Sec

{-------------------------------------------------------------------------------
  Extract file info
-------------------------------------------------------------------------------}

-- | Simplified form of the FileInfo used in hackage-security
data FileInfo = FileInfo {
    fileInfoLength :: Int
  , fileInfoSHA256 :: SHA256Digest
  }
  deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''FileInfo

instance MemSize FileInfo where
  memSize FileInfo{..} =
    memSize2
      fileInfoLength
      fileInfoSHA256

class HasFileInfo a where
  fileInfo :: a -> FileInfo

instance HasFileInfo TarballUncompressed where
  fileInfo TarballUncompressed{..} = FileInfo tarLength tarHashSHA256

instance HasFileInfo TarballCompressed where
  fileInfo TarballCompressed{..} = FileInfo tarGzLength tarGzHashSHA256

instance HasFileInfo BlobInfo where
  fileInfo BlobInfo{..} = FileInfo blobInfoLength blobInfoHashSHA256

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

secFileInfo :: HasFileInfo a => a -> Sec.FileInfo
secFileInfo = toSecFileInfo . fileInfo

-- | Translate our 'FileInfo' to the one from hackage-security
toSecFileInfo :: FileInfo -> Sec.FileInfo
toSecFileInfo FileInfo{..} = Sec.FileInfo {
      fileInfoLength = Sec.FileLength fileInfoLength
    , fileInfoHashes = Map.fromList [hashSHA256 fileInfoSHA256]
    }

hashSHA256 :: SHA256Digest -> (Sec.HashFn, Sec.Hash)
hashSHA256 digest = (Sec.HashFnSHA256, Sec.Hash $ show digest)
