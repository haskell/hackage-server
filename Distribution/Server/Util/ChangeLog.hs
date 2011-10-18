{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Util.ChangeLog
-- Copyright   :  (c) Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils for handling change logs
-----------------------------------------------------------------------------
module Distribution.Server.Util.ChangeLog (
    lookupTarball,
    lookupChangeLog
  ) where

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Util.ServeTarball (readTarIndex)
import qualified Data.TarIndex as TarIndex

import Distribution.Text (disp)

import Control.Monad (msum)
import Text.PrettyPrint.HughesPJ (render)
import System.FilePath ((</>))

lookupTarball :: BlobStorage -> PkgInfo -> Maybe (IO (FilePath, TarIndex.TarIndex))
lookupTarball store pkgInfo = 
    case pkgTarball pkgInfo of
        [] -> Nothing
        ((tb, _):_) -> Just $
            do let blobId = pkgTarballNoGz tb
                   fp = BlobStorage.filepath store blobId
               index <- readTarIndex fp
               return (fp, index)

lookupChangeLog :: BlobStorage -> PkgInfo -> IO (Either String (FilePath, TarIndex.TarEntryOffset, String))
lookupChangeLog store pkgInfo = case lookupTarball store pkgInfo of
        Nothing -> return $ Left "Could not extract changelog: no tarball exists."
        Just io ->
            do (fp, index) <- io
               case msum $ map (lookupFile index) candidates of
                 Just (name, offset) -> return $ Right (fp, offset, name)
                 Nothing ->
                     do let msg = "No changelog found, files considered: " ++ show candidates
                        return $ Left msg
    where
      lookupFile index fname =
          do entry <- TarIndex.lookup index fname
             case entry of
               TarIndex.TarFileEntry offset -> return (fname, offset)
               _ -> fail "is a directory"
      candidates =
          let l = ["ChangeLog", "CHANGELOG", "CHANGE_LOG", "Changelog", "changelog"]
              pkgId = render $ disp (pkgInfoId pkgInfo)
          in map (pkgId </>) $ map (++ ".html") l ++ l
