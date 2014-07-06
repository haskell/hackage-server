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
    , constructTarIndexFromFile
    , constructTarIndex
    ) where

import Happstack.Server.Types
import Happstack.Server.Monads
import Happstack.Server.Routing (method)
import Happstack.Server.Response
import Happstack.Server.FileServe as Happstack (mimeTypes)
import Distribution.Server.Util.Happstack (remainingPath, ETag(..), formatETag)
import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Framework.ResponseContentTypes as Resource

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex)

import qualified Text.XHtml.Strict as XHtml
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import System.FilePath
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (msum, mzero)
import System.IO

-- | Serve the contents of a tar file
-- file. TODO: This is not a sustainable implementation,
-- but it gives us something to test with.
serveTarball :: MonadIO m
             => [FilePath] -- dir index file names (e.g. ["index.html"])
             -> FilePath   -- root dir in tar to serve
             -> FilePath   -- the tarball
             -> TarIndex   -- index for tarball
             -> ETag       -- the etag
             -> ServerPartT m Response
serveTarball indices tarRoot tarball tarIndex etag = do
    rq <- askRq
    action GET $ remainingPath $ \paths -> do

      -- first we come up with the set of paths in the tarball that
      -- would match our request
      let validPaths :: [FilePath]
          validPaths = (joinPath $ tarRoot:paths)
                     : [joinPath $ tarRoot:paths ++ [index] | index <- indices]

      msum $ concat
       [ serveFiles validPaths
       , serveDirs (rqUri rq) validPaths
       ]
  where
    serveFiles paths
           = flip map paths $ \path ->
             case TarIndex.lookup tarIndex path of
               Just (TarIndex.TarFileEntry off)
                   -> do
                 tfe <- liftIO $ serveTarEntry tarball off path etag
                 ok (toResponse tfe)
               _ -> mzero

    action act m = method act >> m

    serveDirs fullPath paths
           = flip map paths $ \path ->
             case TarIndex.lookup tarIndex path of
               Just (TarIndex.TarDir fs)
                 | not (hasTrailingPathSeparator fullPath)
                 -> seeOther (addTrailingPathSeparator fullPath) (toResponse ())

                 | otherwise
                 -> ok $ setHeader "ETag" (formatETag etag) $
                         toResponse $ Resource.XHtml $ renderDirIndex fs
               _ -> mzero

renderDirIndex :: [FilePath] -> XHtml.Html
renderDirIndex entries = hackagePage "Directory Listing"
    [ (XHtml.anchor XHtml.! [XHtml.href e] XHtml.<< e)
      XHtml.+++ XHtml.br
    | e <- entries ]

serveTarEntry :: FilePath -> Int -> FilePath -> ETag -> IO Response
serveTarEntry tarfile off fname etag = do
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
                         (setHeader "Content-Type" mimeType) .
                         (setHeader "ETag" (formatETag etag))) $
                         resultBS 200 body
         return response
    _ -> fail "oh noes!!"

-- | Extended mapping from file extension to mime type
mimeTypes' :: Map.Map String String
mimeTypes' = Happstack.mimeTypes `Map.union` Map.fromList
  [("xhtml", "application/xhtml+xml")]

constructTarIndexFromFile :: FilePath -> IO TarIndex
constructTarIndexFromFile file = do
  tar <- BS.readFile file
  case constructTarIndex tar of
    Left err       -> fail err
    Right tarIndex -> return tarIndex

-- | Forcing the Either will force the tar index
constructTarIndex :: BS.ByteString -> Either String TarIndex
constructTarIndex tar =
  case extractInfo (Tar.read tar) of
    Just info -> let tarIndex = TarIndex.construct info
                 in tarIndex `seq` Right tarIndex
    Nothing   -> Left "bad tar file"

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

