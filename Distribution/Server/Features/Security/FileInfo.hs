-- | Extract file info from response types
{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Security.FileInfo (
    FileInfo(..)
  , mkFileInfo
  ) where

-- stdlib
import qualified Data.Map as Map

-- hackage
import Distribution.Server.Packages.Types
import Distribution.Server.Framework.ResponseContentTypes
import Distribution.Server.Features.Security.SHA256

-- hackage-security
import qualified Hackage.Security.Server as Sec

{-------------------------------------------------------------------------------
  Extract file info
-------------------------------------------------------------------------------}

class FileInfo a where
  fileInfo :: a -> Sec.FileInfo

instance FileInfo TarballUncompressed where
  fileInfo TarballUncompressed{..} = mkFileInfo tarLength tarHashSHA256

instance FileInfo TarballCompressed where
  fileInfo TarballCompressed{..} = mkFileInfo tarGzLength tarGzHashSHA256

instance FileInfo BlobInfo where
  fileInfo BlobInfo{..} = mkFileInfo blobInfoLength blobInfoHashSHA256

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkFileInfo :: Int -> SHA256Digest -> Sec.FileInfo
mkFileInfo len digest = Sec.FileInfo {
      fileInfoLength = Sec.FileLength len
    , fileInfoHashes = Map.fromList [hashSHA256 digest]
    }

hashSHA256 :: SHA256Digest -> (Sec.HashFn, Sec.Hash)
hashSHA256 digest = (Sec.HashFnSHA256, Sec.Hash $ show digest)
