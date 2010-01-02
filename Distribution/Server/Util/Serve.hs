-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Util.Serve
-- Copyright   :  (c) 2008 David Himmelstrup
--                (c) 2009 Antoine Latter
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Server.Util.Serve where

import Distribution.Server.Util.Happstack
import Happstack.Server hiding (path)
import qualified Codec.Archive.Tar as Tar

import Data.ByteString.Lazy (ByteString)
import qualified Codec.Compression.GZip as GZip
import System.FilePath
import Control.Monad
import Data.List (foldl')

import qualified Data.Set as Set


-- | Server the contents of a gzipped tar
-- file. This is not a sustainable implementation,
-- but it gives us something to test with.
serveTarball :: [FilePath]
             -> FilePath
             -> ByteString
             -> ServerPart Response
serveTarball indices offset tarball = do
    action GET $ remainingPath $ \paths -> do

    -- first we come up with the set of paths in the tarball that
    -- would match our request
    let validPaths :: Set.Set String
        validPaths = foldl' f (Set.singleton $ joinPath $ offset:paths) indices

        f set index = Set.insert (joinPath $ offset:paths ++ [index]) set

        -- unpack the tarball
        entries = Tar.read . GZip.decompress $ tarball

        -- handle entry takes an entry to the
        -- information we want from it's contents,
        -- but only if it matches one of our valid
        -- paths
        handleEntry e =
            case Tar.entryContent e of
              Tar.NormalFile fBytes fSize
                  -> let fPath = Tar.entryPath e
                     in if Set.member fPath validPaths
                        then return (fBytes, fSize, fPath)
                        else mzero
              _ -> mzero

    -- process the entries finding the first match
    (fBytes, fSize, fPath) <- foldEntriesM handleEntry (const mzero) entries

    -- send back the matching response
    ok $ Response
           { rsCode = 200
           , rsHeaders = mkHeaders $
                         [("Content-Length", show fSize)
                         ,("Content-Type", mime fPath)
                         ]
           , rsFlags = nullRsFlags { rsfContentLength = False }
           , rsBody  = fBytes
           , rsValidator = Nothing
           }

 where action act m = methodOnly act >> m

-- Traverse tar-entries in monad-plus.
-- This works great if mplus is short circuiting
-- on the left.
foldEntriesM :: MonadPlus m
                => (Tar.Entry -> m a)
                -> (String -> m a)
                -> Tar.Entries
                -> m a
foldEntriesM next fail' entries = fold entries
 where
   fold (Tar.Next e es) = next e `mplus` fold es
   fold Tar.Done        = mzero
   fold (Tar.Fail err)  = fail' err
