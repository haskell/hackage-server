-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Util.ServeTarball
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
module Distribution.Server.Util.ServeTarball
    ( serveTarball
    , serveTarEntry
    , readTarIndex
    ) where

import Happstack.Server.Types
import Happstack.Server.Monads
import Happstack.Server.Routing (method)
import Happstack.Server.Response
import Happstack.Server.FileServe as Happstack (mimeTypes)
import Distribution.Server.Util.Happstack (remainingPath)
import Distribution.Server.Pages.Template (hackagePage)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex)

import qualified Text.XHtml.Strict as XHtml
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import System.FilePath
import Control.Exception (evaluate)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (msum, mzero)
import System.IO


-- | Server the contents of a tar file
-- file. This is not a sustainable implementation,
-- but it gives us something to test with.
serveTarball :: MonadIO m
             => [FilePath] -- indices
             -> FilePath   -- prefix of paths in tar
             -> FilePath   -- tarball
             -> TarIndex   -- index for tarball
             -> ServerPartT m Response
serveTarball indices offset tarball tarIndex = do
    action GET $ remainingPath $ \paths -> do

    -- first we come up with the set of paths in the tarball that
    -- would match our request
    let validPaths :: [FilePath]
        validPaths = (joinPath $ offset:paths) : map f indices
        f index = joinPath $ offset:paths ++ [index]

    msum $ concat
     [ serveFiles validPaths
     , serveDirs paths validPaths
     ]

 where serveFiles paths
           = flip map paths $ \path ->
             case TarIndex.lookup tarIndex path of
               Just (TarIndex.TarFileEntry off)
                   -> do
                 tfe <- liftIO $ serveTarEntry tarball off path
                 ok (toResponse tfe)
               _ -> mzero

       action act m = method act >> m

       serveDirs prefix paths
           = flip map paths $ \path ->
             case TarIndex.lookup tarIndex path of
               Just (TarIndex.TarDir fs)
                   -> do ok $ toResponse $ renderDirIndex prefix fs
               _ -> mzero

renderDirIndex :: [FilePath] -> [FilePath] -> XHtml.Html
renderDirIndex paths entries = hackagePage "Directory Listing"
    [ (XHtml.anchor XHtml.! [XHtml.href (mk_prefix e)] XHtml.<< e)
      XHtml.+++ XHtml.br
    | e <- entries ]
  where -- We need to munge the paths to match the path prefix:
        mk_prefix | null paths = id
                  | otherwise  = (last paths </>)

serveTarEntry :: FilePath -> Int -> FilePath -> IO Response
serveTarEntry tarfile off fname = do
  htar <- openFile tarfile ReadMode
  hSeek htar AbsoluteSeek (fromIntegral (off * 512))
  header <- BS.hGet htar 512
  case Tar.read header of
    (Tar.Next Tar.Entry{Tar.entryContent = Tar.NormalFile _ size} _) -> do
         body <- BS.hGet htar (fromIntegral size)
         let extension = case takeExtension fname of
                           ('.':ext) -> ext
                           ext       -> ext
             mimeType = Map.findWithDefault "text/plain" extension mimeTypes'
             response = ((setHeader "Content-Length" (show size)) .
                         (setHeader "Content-Type" mimeType)) $
                         resultBS 200 body
         return response
    _ -> fail "oh noes!!"

-- | Extended mapping from file extension to mime type
mimeTypes' :: Map.Map String String
mimeTypes' = Happstack.mimeTypes `Map.union` Map.fromList
  [("xhtml", "application/xhtml+xml")]

readTarIndex :: FilePath -> IO TarIndex
readTarIndex file = do
  tar <- BS.readFile file
  let entries = Tar.read tar
  case extractInfo entries of
    Just info -> evaluate (TarIndex.construct info)
    Nothing   -> fail "bad tar file"

type Block = Int

extractInfo :: Tar.Entries e -> Maybe [(FilePath, Block)]
extractInfo = go 0 []
  where
    go _ es' (Tar.Done)      = Just es'
    go _ _   (Tar.Fail _)    = Nothing
    go n es' (Tar.Next e es) = go n' ((Tar.entryPath e, n) : es') es
      where
        n' = n + 1
               + case Tar.entryContent e of
                   Tar.NormalFile     _   size -> blocks size
                   Tar.OtherEntryType _ _ size -> blocks size
                   _                           -> 0
        blocks s = 1 + ((fromIntegral s - 1) `div` 512)

