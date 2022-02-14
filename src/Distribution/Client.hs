{-# LANGUAGE PatternGuards #-}
module Distribution.Client
  ( -- * Command line handling
    validateHackageURI
  , validateHackageURI'
  , validatePackageIds
    -- * Fetching info from source and destination servers
  , PkgIndexInfo(..)
  , downloadIndex
  , readNewIndex
    -- * HTTP utilities
  , HttpSession
  , uriHostName
  , httpSession
  , requestGET'
  , requestPUT
  , (<//>)
  , provideAuthInfo
    -- * TODO: Exported although they appear unusued
  , extractURICredentials
  , removeURICredentials
  , getETag
  , downloadFile'
  , requestGET
  , requestPUTFile
  , requestPOST
  , checkStatus
  ) where

import Network.HTTP
import Network.Browser
import Network.URI (URI(..), URIAuth(..), parseURI)

import Distribution.Server.Prelude
import Distribution.Client.UploadLog as UploadLog (read, Entry(..))
import Distribution.Client.Index as PackageIndex (read)
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))
import Distribution.Server.Util.Merge
import Distribution.Server.Util.Parse (unpackUTF8)
import Distribution.Package
import Distribution.Verbosity
import Distribution.Simple.Utils
import Distribution.Text

import Data.Version
import Data.List
import Control.Exception
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy          as BS
import qualified Distribution.Server.Util.GZip as GZip
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar

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
    Nothing  -> Left ("invalid URL " ++ str)
    Just uri -> validateHackageURI' uri

validateHackageURI' :: URI -> Either String URI
validateHackageURI' uri
    | uriScheme uri /= "http:"     = Left $ "only http URLs are supported " ++ show uri
    | isNothing (uriAuthority uri) = Left $ "server name required in URL " ++ show uri
    | otherwise                    = Right uri

validatePackageIds :: [String] -> Either String [PackageId]
validatePackageIds pkgstrs =
    case theErrors of
      theError : _ ->
          Left $ "'" ++ theError ++ "' is not a valid package name or id"
      _ -> Right pkgs
  where
    pkgstrs'  = [ (pkgstr, simpleParse pkgstr) | pkgstr <- pkgstrs ]
    pkgs      = [ pkgid  | (_, Just pkgid)   <- pkgstrs' ]
    theErrors = [ pkgstr | (pkgstr, Nothing) <- pkgstrs' ]

----------------------------------------------------
-- Fetching info from source and destination servers
----------------------------------------------------

data PkgIndexInfo = PkgIndexInfo
                       PackageId
                       (Maybe UTCTime)  -- Upload time
                       (Maybe UserName) -- Name of uploader
                       (Maybe UserId)   -- Id of uploader
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
downloadOldIndex uri cacheDir = do

    downloadFile indexURI indexFile
    downloadFile logURI logFile

    liftIO $ do

      pkgids <- withFile indexFile ReadMode $ \hnd -> do
        content <- BS.hGetContents hnd
        case PackageIndex.read (\pkgid _ -> pkgid) (const True) (GZip.decompressNamed indexFile content) of
          Right pkgs     -> return pkgs
          Left  theError ->
              dieNoVerbosity $ "Error parsing index at " ++ show uri ++ ": " ++ theError

      theLog <- withFile logFile ReadMode $ \hnd -> do
        content <- hGetContents hnd
        case UploadLog.read content of
          Right theLog   -> return theLog
          Left  theError ->
              dieNoVerbosity $ "Error parsing log at " ++ show uri ++ ": " ++ theError

      return (mergeLogInfo pkgids theLog)

  where
    indexURI  = uri <//> "packages" </> "archive" </> "00-index.tar.gz"
    indexFile = cacheDir </> "00-index.tar.gz"

    logURI    = uri <//> "packages" </> "archive" </> "log"
    logFile   = cacheDir </> "log"

    mergeLogInfo pkgids theLog =
        catMaybes
      . map selectDetails
      $ mergeBy (\pkgid entry -> compare pkgid (entryPkgId entry))
                (sort pkgids)
                ( map (maximumBy (comparing entryTime))
                . groupBy (equating  entryPkgId)
                . sortBy  (comparing entryPkgId)
                $ theLog )

    selectDetails (OnlyInRight _)     = Nothing
    selectDetails (OnlyInLeft  pkgid) =
      Just $ PkgIndexInfo pkgid Nothing Nothing Nothing
    selectDetails (InBoth pkgid (UploadLog.Entry time uname _)) =
      Just $ PkgIndexInfo pkgid (Just time) (Just uname) Nothing

    entryPkgId (Entry _ _ pkgid) = pkgid
    entryTime  (Entry time _ _)  = time


downloadNewIndex :: URI -> FilePath -> HttpSession [PkgIndexInfo]
downloadNewIndex uri cacheDir = do
    downloadFile indexURI indexFile
    readNewIndex cacheDir

  where
    indexURI  = uri <//> "packages/00-index.tar.gz"
    indexFile = cacheDir </> "00-index.tar.gz"

readNewIndex :: FilePath -> HttpSession [PkgIndexInfo]
readNewIndex cacheDir = do
    liftIO $ withFile indexFile ReadMode $ \hnd -> do
      content <- BS.hGetContents hnd
      case PackageIndex.read selectDetails (const True) (GZip.decompressNamed indexFile content) of
        Left theError ->
            error ("Error parsing index at " ++ show indexFile ++ ": "
                ++ theError)
        Right pkgs -> return pkgs

  where
    indexFile = cacheDir </> "00-index.tar.gz"

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


extractURICredentials :: URI -> Maybe (String, String)
extractURICredentials uri
  | Just authority <- uriAuthority uri
  , (username, ':':passwd0) <- break (==':') (uriUserInfo authority)
  , let passwd = takeWhile (/='@') passwd0
  , not (null username)
  , not (null passwd)
  = Just (username, passwd)
extractURICredentials _ = Nothing

removeURICredentials :: URI -> URI
removeURICredentials uri = uri { uriAuthority = fmap (\auth -> auth { uriUserInfo = "" }) (uriAuthority uri) }

provideAuthInfo :: URI -> Maybe (String, String) -> URI -> String -> IO (Maybe (String, String))
provideAuthInfo for_uri credentials = \uri _realm -> do
    if uriHostName uri == uriHostName for_uri then return credentials
                                              else return Nothing

uriHostName :: URI -> Maybe String
uriHostName = fmap uriRegName . uriAuthority


type HttpSession a = BrowserAction (HandleStream ByteString) a

httpSession :: Verbosity -> String -> Version -> HttpSession a -> IO a
httpSession verbosity agent version action =
    browse $ do
      setUserAgent (agent ++ "/" ++ showVersion version)
      setErrHandler dieNoVerbosity
      setOutHandler (debug verbosity)
      setAllowBasicAuth True
      setCheckForProxy True
      action

downloadFile :: URI -> FilePath -> HttpSession ()
downloadFile uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  let etagFile = file <.> "etag"
  metag <- liftIO $ catchJustDoesNotExistError
                        (Just <$> readFile etagFile)
                        (\_ -> return Nothing)
  case metag of
    Just etag -> do
      let headers = [mkHeader HdrIfNoneMatch (quote etag)]
      (_, rsp) <- request (Request uri GET headers BS.empty)
      case rspCode rsp of
        (3,0,4) -> out $ file ++ " unchanged with ETag " ++ etag
        (2,0,0) -> liftIO $ writeDowloadedFileAndEtag rsp
        _       -> err (showFailure uri rsp)

    Nothing -> do
      (_, rsp) <- request (Request uri GET [] BS.empty)
      case rspCode rsp of
        (2,0,0) -> liftIO $ writeDowloadedFileAndEtag rsp
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

catchJustDoesNotExistError :: IO a -> (IOError -> IO a) -> IO a
catchJustDoesNotExistError =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)

quote :: String -> String
quote   s = '"' : s ++ ['"']

unquote :: String -> String
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

    Just content -> do liftIO $ BS.writeFile file content
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


requestPUTFile :: URI -> String -> Maybe String -> FilePath -> HttpSession ()
requestPUTFile uri mime_type mEncoding file = do
    content <- liftIO $ BS.readFile file
    requestPUT uri mime_type mEncoding content

requestPOST, requestPUT :: URI -> String -> Maybe String -> ByteString -> HttpSession ()
requestPOST = requestPOSTPUT POST
requestPUT = requestPOSTPUT PUT

requestPOSTPUT :: RequestMethod -> URI -> String -> Maybe String -> ByteString -> HttpSession ()
requestPOSTPUT meth uri mimetype mEncoding body = do
    (_, rsp) <- request (Request uri meth headers body)
    checkStatus uri rsp
  where
    headers = [ Header HdrContentLength (show (BS.length body))
              , Header HdrContentType mimetype ]
              ++ case mEncoding of
                Nothing       -> []
                Just encoding -> [ Header HdrContentEncoding encoding ]


checkStatus :: URI -> Response ByteString -> HttpSession ()
checkStatus uri rsp = case rspCode rsp of
  -- 200 OK
  (2,0,0) -> return ()
  -- 201 Created
  (2,0,1) -> return ()
  -- 201 Created
  (2,0,2) -> return ()
  -- 204 No Content
  (2,0,4) -> return ()
  -- 400 Bad Request
  (4,0,0) -> liftIO (warn normal (showFailure uri rsp)) >> return ()
  -- Other
  _code   -> err (showFailure uri rsp)

showFailure :: URI -> Response ByteString -> String
showFailure uri rsp =
    show (rspCode rsp) ++ " " ++ rspReason rsp ++ show uri
 ++ case lookupHeader HdrContentType (rspHeaders rsp) of
      Just mimetype | "text/plain" `isPrefixOf` mimetype
                   -> '\n' : (unpackUTF8 . rspBody $ rsp)
      _            -> ""
