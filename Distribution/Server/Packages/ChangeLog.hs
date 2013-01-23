{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Packages.ChangeLog (
    findChangeLog
  ) where

import Control.Monad (msum)
import System.FilePath ((</>))
import qualified Text.PrettyPrint.HughesPJ as Pretty (render)

import Data.TarIndex (TarIndex, TarEntryOffset)
import qualified Data.TarIndex as TarIndex
import Distribution.Text (disp)
import Distribution.Server.Packages.Types (PkgInfo(..))

findChangeLog :: PkgInfo -> TarIndex -> Either String (TarEntryOffset, String)
findChangeLog PkgInfo{pkgInfoId} index =
  case msum $ map lookupFile candidates of
    Just (offset, name) ->
      Right (offset, name)
    Nothing -> do
      Left $ "No changelog found, considered: " ++ show candidates
  where
    lookupFile fname = do
      entry <- TarIndex.lookup index fname
      case entry of
        TarIndex.TarFileEntry offset -> return (offset, fname)
        _ -> fail "is a directory"

    candidates = let pkgId = Pretty.render $ disp pkgInfoId
                 in map (pkgId </>) $ filenames ++ map (++ ".html") filenames

    filenames = [ "ChangeLog"
                , "CHANGELOG"
                , "CHANGE_LOG"
                , "Changelog"
                , "changelog"
                ]
