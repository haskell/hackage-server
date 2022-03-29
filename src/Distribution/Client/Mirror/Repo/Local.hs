module Distribution.Client.Mirror.Repo.Local (
    withTargetRepo
  , downloadIndex
  , uploadPackage
  , packageExists
  ) where

-- stdlib
import Control.Monad
import Network.URI (URI, uriPath)
import System.Directory
import System.FilePath

-- Cabal
import Distribution.Text

-- hackage
import Distribution.Client hiding (downloadIndex)
import Distribution.Client.Mirror.Session
import Distribution.Client.Mirror.Repo.Util
import Distribution.Client.Mirror.Repo.Types

withTargetRepo :: URI -> FilePath -> (TargetRepo -> IO a) -> IO a
withTargetRepo uri cacheDir callback = callback TargetLocal {
      targetRepoPath      = uriPath uri
    , targetRepoCachePath = cacheDir
    }

downloadIndex :: FilePath -> FilePath -> MirrorSession [PkgIndexInfo]
downloadIndex root _cacheDir = do
    -- We don't know which version of the index this repo has. If there is a
    -- 00-index available then reading this is faster (fewer entries); but if
    -- there isn't, we check if the 01-index is available.
    firstToSucceed $ map tryDownload [index00, index01]
  where
    tryDownload :: FilePath -> MirrorSession (Maybe [PkgIndexInfo])
    tryDownload fp = do indexExists <- liftIO $ doesFileExist fp
                        if indexExists
                          then Just `liftM` readIndex root fp
                          else return Nothing

    -- If we can't find any index, return the empty list (no files on target)
    firstToSucceed :: [MirrorSession (Maybe [a])] -> MirrorSession [a]
    firstToSucceed []     = return []
    firstToSucceed (a:as) = do mIdx <- a
                               case mIdx of
                                 Just idx -> return idx
                                 Nothing  -> firstToSucceed as

    index00, index01 :: FilePath
    index00 = root </> "00-index.tar.gz"
    index01 = root </> "01-index.tar.gz"

-- | Upload a package
uploadPackage :: FilePath
              -> PkgIndexInfo
              -> FilePath
              -> MirrorSession ()
uploadPackage targetRepoPath' pkginfo locTgz = liftIO $ do
    createDirectoryIfMissing True pkgDir
    copyFile locTgz pkgFile
  where
    pkgDir  = targetRepoPath' </> "package"
    pkgFile = pkgDir   </> display pkgid <.> "tar.gz"

    PkgIndexInfo pkgid _ _ _ = pkginfo

-- | Check if a package already exists
packageExists :: FilePath
              -> PkgIndexInfo
              -> MirrorSession Bool
packageExists targetRepoPath' pkginfo = liftIO $
    doesFileExist pkgFile
  where
    pkgDir  = targetRepoPath' </> "package"
    pkgFile = pkgDir   </> display pkgid <.> "tar.gz"

    PkgIndexInfo pkgid _ _ _ = pkginfo

