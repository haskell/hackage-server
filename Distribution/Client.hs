{-# LANGUAGE PatternGuards #-}
module Distribution.Client where

import Network.HTTP
import Network.Browser
import Network.URI (URI(..), URIAuth(..), parseURI)

import Distribution.Server.LegacyImport.UploadLog as UploadLog (read, Entry(..))
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import Distribution.Server.Util.Index as PackageIndex (read)
import Distribution.Server.Util.Merge
import Distribution.Package
import Distribution.Verbosity
import Distribution.Simple.Utils

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Codec.Compression.GZip  as GZip
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import qualified System.FilePath.Posix as Posix

-------------------------
-- Command line handling
-------------------------

validateHackageURI :: String -> Either String URI
validateHackageURI str = case parseURI str of
  Nothing                          -> Left ("invalid URL " ++ str)
  Just uri
    | uriScheme uri /= "http:"     -> Left ("only http URLs are supported " ++ str)
    | isNothing (uriAuthority uri) -> Left ("server name required in URL " ++ str)
    | otherwise                    -> Right uri

----------------------------------------------------
-- Fetching info from source and destination servers
----------------------------------------------------

data PkgIndexInfo = PkgIndexInfo
                       PackageId
                       (Maybe UTCTime)
                       (Maybe UserName)
                       (Maybe UserId)
  deriving Show

downloadIndex :: URI -> FilePath -> HttpSession [PkgIndexInfo]
downloadIndex uri | isOldHackageURI uri = downloadOldIndex uri
                  | otherwise           = downloadNewIndex uri
  where

isOldHackageURI :: URI -> Bool
isOldHackageURI uri
  | Just auth <- uriAuthority uri = uriRegName auth == "hackage.haskell.org"
  | otherwise                     = False


downloadOldIndex :: URI -> FilePath -> HttpSession [PkgIndexInfo]
downloadOldIndex uri cacheFile = do

    downloadFile indexURI indexFile
    downloadFile logURI logFile

    ioAction $ do

      pkgids <- withFile indexFile ReadMode $ \hnd -> do
        content <- BS.hGetContents hnd
        case PackageIndex.read (\pkgid _ -> pkgid) (GZip.decompress content) of
          Left  err  -> die $ "Error parsing index at " ++ show uri ++ ": " ++ err
          Right pkgs -> return pkgs

      log <- withFile logFile ReadMode $ \hnd -> do
        content <- hGetContents hnd
        case UploadLog.read content of
          Right log -> return log
          Left  err -> die $ "Error parsing log at " ++ show uri ++ ": " ++ err

      return (mergeLogInfo pkgids log)

  where
    indexURI  = uri <//> "packages" </> "archive" </> "00-index.tar.gz"
    indexFile = cacheFile <.> "tar.gz"

    logURI    = uri <//> "packages" </> "archive" </> "log"
    logFile   = cacheFile <.> "log"

    mergeLogInfo pkgids log =
        catMaybes
      . map selectDetails
      $ mergeBy (\pkgid entry -> compare pkgid (entryPkgId entry))
                (sort pkgids)
                ( map (maximumBy (comparing entryTime))
                . groupBy (equating  entryPkgId)
                . sortBy  (comparing entryPkgId)
                $ log )

    selectDetails (OnlyInRight _)     = Nothing
    selectDetails (OnlyInLeft  pkgid) =
      Just $ PkgIndexInfo pkgid Nothing Nothing Nothing
    selectDetails (InBoth pkgid (UploadLog.Entry time uname _)) =
      Just $ PkgIndexInfo pkgid (Just time) (Just uname) Nothing

    entryPkgId (Entry _ _ pkgid) = pkgid
    entryTime  (Entry time _ _)  = time


downloadNewIndex :: URI -> FilePath -> HttpSession [PkgIndexInfo]
downloadNewIndex uri cacheFile = do
    downloadFile indexURI indexFile
    ioAction $ withFile indexFile ReadMode $ \hnd -> do
      content <- BS.hGetContents hnd
      case PackageIndex.read selectDetails (GZip.decompress content) of
        Left err   -> error $ "Error parsing index at " ++ show uri ++ ": " ++ err
        Right pkgs -> return pkgs

  where
    indexURI  = uri <//> "packages/00-index.tar.gz"
    indexFile = cacheFile <.> "tar.gz"

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

        epochTimeToUTC :: Tar.EpochTime -> UTCTime
        epochTimeToUTC = posixSecondsToUTCTime . realToFrac


-------------------------
-- HTTP utilities
-------------------------

infixr 5 <//>

(<//>) :: URI -> FilePath -> URI
uri <//> path = uri { uriPath = Posix.addTrailingPathSeparator (uriPath uri)
                                Posix.</> path }


type HttpSession a = BrowserAction (HandleStream ByteString) a

httpSession :: Verbosity -> HttpSession a -> IO a
httpSession verbosity action =
    browse $ do
      setUserAgent "hackage-mirror"
      setErrHandler die
      setOutHandler (debug verbosity)
      setAllowBasicAuth True
      setCheckForProxy True
      action

downloadFile :: URI -> FilePath -> HttpSession ()
downloadFile uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  let etagFile = file <.> "etag"
  metag <- ioAction $ catchJustDoesNotExistError
                        (Just <$> readFile etagFile)
                        (\_ -> return Nothing)
  case metag of
    Just etag -> do
      let headers = [mkHeader HdrIfNoneMatch (quote etag)]
      (_, rsp) <- request (Request uri GET headers BS.empty)
      case rspCode rsp of
        (3,0,4) -> out $ file ++ " unchanged with ETag " ++ etag
        (2,0,0) -> ioAction $ writeDowloadedFileAndEtag rsp
        _       -> err (showFailure uri rsp)

    Nothing -> do
      (_, rsp) <- request (Request uri GET [] BS.empty)
      case rspCode rsp of
        (2,0,0) -> ioAction $ writeDowloadedFileAndEtag rsp
        _       -> err (showFailure uri rsp)

  where
    writeDowloadedFileAndEtag rsp = do
      BS.writeFile file (rspBody rsp)
      setETag file (unquote <$> findHeader HdrETag rsp)

getETag :: FilePath -> IO (Maybe String)
getETag file =
    catchJustDoesNotExistError
      (Just <$> readFile (file </> ".etag"))
      (\_ -> return Nothing)

setETag :: FilePath -> Maybe String -> IO ()
setETag file Nothing     = catchJustDoesNotExistError
                             (removeFile (file <.> "etag"))
                             (\_ -> return ())
setETag file (Just etag) = writeFile  (file <.> "etag") etag

catchJustDoesNotExistError =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)

quote   s = '"' : s ++ ['"']

unquote ('"':s) = go s
  where
    go []       = []
    go ('"':[]) = []
    go (c:cs)   = c : go cs
unquote     s   = s

-- AAARG! total lack of exception handling in HTTP monad!
downloadFile' :: URI -> FilePath -> HttpSession Bool
downloadFile' uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  mcontent <- requestGET' uri
  case mcontent of
    Nothing      -> do out $ "404 " ++ show uri
                       return False

    Just content -> do ioAction $ BS.writeFile file content
                       return True

requestGET :: URI -> HttpSession ByteString
requestGET uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    checkStatus uri rsp
    return (rspBody rsp)
  where
    headers = []

-- Really annoying!
requestGET' :: URI -> HttpSession (Maybe ByteString)
requestGET' uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    case rspCode rsp of
      (4,0,4) -> return Nothing
      _       -> do checkStatus uri rsp
                    return (Just (rspBody rsp))
  where
    headers = []


requestPUT :: URI -> String -> ByteString -> HttpSession ()
requestPUT uri mimetype body = do
    (_, rsp) <- request (Request uri PUT headers body)
    checkStatus uri rsp
  where
    headers = [ Header HdrContentLength (show (BS.length body))
              , Header HdrContentType mimetype ]


checkStatus :: URI -> Response ByteString -> HttpSession ()
checkStatus uri rsp = case rspCode rsp of
  (2,0,0) -> return ()
  (4,0,0) -> ioAction (warn normal (showFailure uri rsp)) >> return ()
  code    -> err (showFailure uri rsp)

showFailure uri rsp =
    show (rspCode rsp) ++ " " ++ rspReason rsp ++ show uri
 ++ case lookupHeader HdrContentType (rspHeaders rsp) of
      Just mimetype | "text/plain" `isPrefixOf` mimetype
                   -> '\n' : BS.unpack (rspBody rsp)
      _            -> ""