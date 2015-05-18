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
    , loadTarEntry
    , constructTarIndexFromFile
    , constructTarIndex
    ) where

import Happstack.Server.Types
import Happstack.Server.Monads
import Happstack.Server.Routing (method)
import Happstack.Server.Response
import Distribution.Server.Framework.HappstackUtils (mime, remainingPath)
import Distribution.Server.Framework.CacheControl
import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Framework.ResponseContentTypes as Resource

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex)

import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Strict ((<<), (!))
import qualified Data.ByteString.Lazy as BS
import System.FilePath
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (msum, mzero, MonadPlus(..))
import System.IO

-- | Serve the contents of a tar file
-- file. TODO: This is not a sustainable implementation,
-- but it gives us something to test with.
serveTarball :: (MonadIO m, MonadPlus m)
             => String     -- description for directory listings
             -> [FilePath] -- dir index file names (e.g. ["index.html"])
             -> FilePath   -- root dir in tar to serve
             -> FilePath   -- the tarball
             -> TarIndex   -- index for tarball
             -> [CacheControl]
             -> ETag       -- the etag
             -> ServerPartT m Response
serveTarball descr indices tarRoot tarball tarIndex cacheCtls etag = do
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
                 cacheControl cacheCtls etag
                 tfe <- liftIO $ serveTarEntry tarball off path
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
                 -> do
                      cacheControl cacheCtls etag
                      ok $ toResponse $ Resource.XHtml $
                        renderDirIndex descr path fs
               _ -> mzero

renderDirIndex :: String -> FilePath -> [(FilePath, TarIndex.TarIndexEntry)] -> XHtml.Html
renderDirIndex descr topdir topentries =
    hackagePage title
      [ XHtml.h2 << title
      , XHtml.h3 << addTrailingPathSeparator topdir
      , renderForest "" topentries]
  where
    title = "Directory listing for " ++ descr
    renderForest _   [] = XHtml.noHtml
    renderForest dir ts = XHtml.ulist ! [ XHtml.theclass "directory-list" ]
                           << map (uncurry (renderTree dir)) ts

    renderTree dir entryname (TarIndex.TarFileEntry _) =
      XHtml.li << XHtml.anchor ! [XHtml.href (dir </> entryname)]
                              << entryname
    renderTree dir entryname (TarIndex.TarDir entries) =
      XHtml.li << [ XHtml.anchor ! [XHtml.href (dir </> entryname)]
                              << addTrailingPathSeparator entryname
                  , renderForest (dir </> entryname) entries ]


loadTarEntry :: FilePath -> TarIndex.TarEntryOffset -> IO (Either String (Tar.FileSize, BS.ByteString))
loadTarEntry tarfile off = do
  htar <- openFile tarfile ReadMode
  hSeek htar AbsoluteSeek (fromIntegral $ off * 512)
  header <- BS.hGet htar 512
  case Tar.read header of
    (Tar.Next Tar.Entry{Tar.entryContent = Tar.NormalFile _ size} _) -> do
         body <- BS.hGet htar (fromIntegral size)
         return $ Right (size, body)
    _ -> fail "failed to read entry from tar file"

serveTarEntry :: FilePath -> TarIndex.TarEntryOffset -> FilePath -> IO Response
serveTarEntry tarfile off fname = do
    Right (size, body) <- loadTarEntry tarfile off
    return . ((setHeader "Content-Length" (show size)) .
              (setHeader "Content-Type" mimeType)) $
              resultBS 200 body
  where mimeType = mime fname

constructTarIndexFromFile :: FilePath -> IO TarIndex
constructTarIndexFromFile file = do
  tar <- BS.readFile file
  case constructTarIndex tar of
    Left err       -> fail err
    Right tarIndex -> return tarIndex

-- | Forcing the Either will force the tar index
constructTarIndex :: BS.ByteString -> Either String TarIndex
constructTarIndex = either (\e -> Left ("bad tar file: " ++ show e)) Right
                  . TarIndex.construct . Tar.read

