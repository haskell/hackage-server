-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Util.Serve
-- Copyright   :  (c) 2008 David Himmelstrup
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Server.Util.Serve where

--import Happstack.Server hiding (path)
--import Happstack.Server.HTTP.FileServe (mimeTypes)
--import qualified Codec.Archive.Tar as Tar

--import Data.Maybe (listToMaybe)
--import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
--import qualified Codec.Compression.GZip as GZip
import System.FilePath
import Happstack.Server.SimpleHTTP

serveTarball :: [FilePath]
             -> FilePath
             -> Request
             -> ByteString
             -> Web Response
serveTarball _ _ _ _ = noHandle
{-
serveTarball _indices _offset rq _tarball | rqMethod rq /= GET = noHandle
serveTarball indices offset rq tarball
    = do let entries = Tar.read (GZip.decompress tarball)
             path    = joinPath (rqPaths rq)
             mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes
             fileMap = Tar.foldEntries (\entry -> Map.insert (Tar.entryPath entry) entry) Map.empty error entries
         case listToMaybe [ (val,key) | key <- path:indices, Just val <- [Map.lookup (offset </> key) fileMap]] of
           Nothing -> noHandle
           Just (entry,key) -> ok $ Response
                                { rsCode = 200
                                , rsHeaders = mkHeaders $ [("Content-Length", show (fileSize entry))
                                                          ,("Content-Type", mime key)
                                                          ]
                                , rsFlags = nullRsFlags { rsfContentLength = False }
                                , rsBody  = fileContent entry
                                , rsValidator = Nothing
                                }
-}
