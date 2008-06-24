module Distribution.Server.BlobStorage where

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Data.Digest.Pure.MD5
         ( MD5Digest, md5 )

newtype BlobId = BlobId MD5Digest

newtype BlobStorage = BlobStorage FilePath -- ^ location of the store

add :: BlobStorage -> ByteString -> IO BlobId
add = undefined
  -- It should:
  --   stream the incomming bytestring into a temp file
  --   take the md5sum
  --   rename it into the target file with the md5sum name
  --   return the id

fetch :: BlobStorage -> BlobId -> IO ByteString
fetch (BlobStorage store) (BlobId hash) =
  BS.readFile (store </> show hash)
