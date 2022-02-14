module Distribution.Client.Mirror.Repo.Hackage2 (
    withSourceRepo
  , withTargetRepo
  , downloadIndex
  , downloadPackage
  , authenticate
  , uploadPackage
  , finalizeLocalMirror
  , cacheTargetIndex
  ) where

-- stdlib
import Data.Time
import Network.Browser
import Network.URI
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BS

-- Cabal
import Distribution.Package
import Distribution.Text

-- hackage
import Distribution.Client (PkgIndexInfo(..), (<//>))
import Distribution.Client.Mirror.Session
import Distribution.Client.Mirror.Repo.Util
import Distribution.Client.Mirror.Repo.Types
import Distribution.Server.Util.Parse (packUTF8)

withSourceRepo :: URI -> FilePath -> (SourceRepo -> IO a) -> IO a
withSourceRepo uri cacheDir callback = callback SourceHackage2 {
      sourceRepoURI       = uri
    , sourceRepoCachePath = cacheDir
    }

withTargetRepo :: URI -> FilePath -> (TargetRepo -> IO a) -> IO a
withTargetRepo uri cacheDir callback = callback TargetHackage2 {
      targetRepoURI       = uri
    , targetRepoCachePath = cacheDir
    }

downloadIndex :: URI -> FilePath -> MirrorSession [PkgIndexInfo]
downloadIndex uri cacheDir = do
    rsp <- browserAction $ downloadFile indexURI indexFile
    case rsp of
      Just theError -> mirrorError (GetEntityError EntityIndex theError)
      Nothing -> notifyResponse GetIndexOk >> readIndex (show uri) indexFile
  where
    indexURI  = remoteIndexPath uri
    indexFile = sourceCachedIndexPath cacheDir

downloadPackage :: URI
                -> PackageId
                -> FilePath
                -> FilePath
                -> MirrorSession (Maybe GetError)
downloadPackage srcURI pkgid locCab locTgz = browserActions [
       downloadFile' srcCab locCab
     , downloadFile' srcTgz locTgz
     ]
  where
    srcPackage :: FilePath
    srcPackage = "package" </> display pkgid

    srcBase, srcTgz, srcCab :: URI
    srcBase = srcURI  <//> srcPackage
    srcTgz  = srcBase <//> display pkgid <.> "tar.gz"
    srcCab  = srcBase <//> display (packageName pkgid) <.> "cabal"

authenticate :: URI -> MirrorSession ()
authenticate = browserAction . setAuthorityGen . provideAuthInfo

-- | Upload a package
uploadPackage :: URI
              -> Bool
              -> PkgIndexInfo
              -> FilePath
              -> FilePath
              -> MirrorSession ()
uploadPackage targetRepoURI' doMirrorUploaders pkginfo locCab locTgz = do
    cab <- liftIO $ BS.readFile locCab
    tgz <- liftIO $ BS.readFile locTgz

    rsp <- browserActions $ [
        requestPUT cabURI "text/plain" cab
      , requestPUT tgzURI "application/x-gzip" tgz
      , maybe (return Nothing) putPackageUploadTime mtime
      ] ++
      (if doMirrorUploaders
         then [maybe (return Nothing) putPackageUploader muname]
         else []
      )

    case rsp of
      Just theError ->
        notifyResponse (PutPackageFailed theError pkgid)
      Nothing -> do
        notifyResponse PutPackageOk
        liftIO $ removeFile locCab
        liftIO $ removeFile locTgz

      -- TODO: think about in what situations we delete the file
      -- and if we should actually cache it if we don't successfully upload.

      -- TODO: perhaps we shouldn't report failure for the whole package if
      -- we fail to set the upload time/uploader
  where
    PkgIndexInfo pkgid mtime muname _muid = pkginfo
    baseURI = targetRepoURI' <//> "package" </> display pkgid
    cabURI  = baseURI <//> display (packageName pkgid) <.> "cabal"
    tgzURI  = baseURI <//> display pkgid               <.> "tar.gz"

    putPackageUploadTime time = do
      let timeStr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
      requestPUT (baseURI <//> "upload-time") "text/plain" (packUTF8 timeStr)

    putPackageUploader uname = do
      let nameStr = display uname
      requestPUT (baseURI <//> "uploader") "text/plain" (packUTF8 nameStr)

finalizeLocalMirror :: FilePath -> FilePath -> MirrorSession ()
finalizeLocalMirror sourceCache targetRepoPath' = liftIO $ do
    copyFile (sourceCachedIndexPath sourceCache)
             (targetIndexPath targetRepoPath')

cacheTargetIndex :: FilePath -> FilePath -> MirrorSession ()
cacheTargetIndex sourceCache targetCache = liftIO $
    copyFile (sourceCachedIndexPath sourceCache)
             (targetCachedIndexPath targetCache)

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

remoteIndexPath :: URI -> URI
remoteIndexPath uri = uri <//> "packages/index.tar.gz"

targetIndexPath :: FilePath -> FilePath
targetIndexPath targetRepoPath' = targetRepoPath' </> "00-index.tar.gz"

sourceCachedIndexPath :: FilePath -> FilePath
sourceCachedIndexPath cacheDir = cacheDir </> "00-index.tar.gz"
