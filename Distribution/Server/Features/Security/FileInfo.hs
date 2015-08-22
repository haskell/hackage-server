-- | Extract file info from response types
{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Security.FileInfo (FileInfo(..)) where

-- stdlib
import qualified Data.Map as Map

-- hackage
import Distribution.Server.Packages.Types
import Distribution.Server.Framework.ResponseContentTypes
import Distribution.Server.Features.Security.ResponseContentTypes

-- hackage-security
import qualified Hackage.Security.Server as Sec
import qualified Data.Digest.Pure.SHA    as SHA

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

instance FileInfo (TUFFile a) where
  fileInfo TUFFile{..} = mkFileInfo tufFileLength tufFileHashSHA256

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkFileInfo :: Int -> SHA.Digest SHA.SHA256State -> Sec.FileInfo
mkFileInfo len sha256 = Sec.FileInfo {
      fileInfoLength = Sec.FileLength len
    , fileInfoHashes = Map.fromList [hashSHA256 sha256]
    }

hashSHA256 :: SHA.Digest SHA.SHA256State -> (Sec.HashFn, Sec.Hash)
hashSHA256 hash = (Sec.HashFnSHA256, Sec.Hash $ SHA.showDigest hash)
