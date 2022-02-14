-- | Functionality common to all repo kinds
module Distribution.Client.Mirror.Repo.Util (
    readIndex
  , provideAuthInfo
  ) where

-- stdlib
import Control.Exception
import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX
import Network.URI hiding (authority)
import System.FilePath
import System.IO
import qualified Data.ByteString.Lazy    as BS
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

-- Cabal
import Distribution.Package

-- hackage
import Distribution.Client hiding (provideAuthInfo)
import Distribution.Client.Index as PackageIndex (read)
import Distribution.Client.Mirror.Session
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import qualified Distribution.Server.Util.GZip as GZip

readIndex :: String   -- ^ Description of the repository (for error messages)
          -> FilePath -- ^ Location of the downloaded index
          -> MirrorSession [PkgIndexInfo]
readIndex repDescr indexFile = do
    res <- liftIO $ withFile indexFile ReadMode $ \hnd ->
      evaluate . PackageIndex.read selectDetails isCabalFile
               . decompressIfNecessary
             =<< BS.hGetContents hnd
    case res of
      Left  err  -> mirrorError (ParseEntityError EntityIndex repDescr err)
      Right pkgs -> return pkgs
  where
    selectDetails :: PackageId -> Tar.Entry -> PkgIndexInfo
    selectDetails pkgid entry =
        PkgIndexInfo
          pkgid
          (Just time)
          (if null username then Nothing else Just (UserName username))
          (if userid == 0   then Nothing else Just (UserId userid))
      where
        time     = epochTimeToUTC (Tar.entryTime entry)
        username = Tar.ownerName (Tar.entryOwnership entry)
        userid   = Tar.ownerId   (Tar.entryOwnership entry)

    decompressIfNecessary :: BS.ByteString -> BS.ByteString
    decompressIfNecessary =
      if takeExtension indexFile == ".gz"
        then GZip.decompressNamed indexFile
        else id

    epochTimeToUTC :: Tar.EpochTime -> UTCTime
    epochTimeToUTC = posixSecondsToUTCTime . realToFrac

    isCabalFile :: FilePath -> Bool
    isCabalFile fp = takeExtension fp == ".cabal"

provideAuthInfo :: URI -> URI -> String -> IO (Maybe (String, String))
provideAuthInfo repoURI uri _realm = return go
  where
    go :: Maybe (String, String)
    go = do
      guard $ hostName uri == hostName repoURI
      authority <- uriAuthority repoURI
      let (username, ':':passwd0) = break (==':') (uriUserInfo authority)
          passwd = takeWhile (/='@') passwd0
      guard $ not (null username)
      guard $ not (null passwd)
      return (username, passwd)

    hostName :: URI -> Maybe String
    hostName = fmap uriRegName . uriAuthority
