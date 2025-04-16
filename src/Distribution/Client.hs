{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , Request
  , mkRequest
  , mkUploadRequest
  , noRedirects
  , applyBasicAuth
  , runRequest
  , Response(..)
  , responseReadBSL
  , requestGET'
  , requestPUT
  , (<//>)
  , getETag
  , checkStatus
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
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

import Control.Exception
import Control.Monad.Trans.Reader
import Data.Version
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy          as BS
import qualified Data.ByteString.Char8         as BSS
import qualified Distribution.Server.Util.GZip as GZip
import qualified Codec.Archive.Tar             as Tar
import qualified Codec.Archive.Tar.Entry       as Tar

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import qualified System.FilePath.Posix as Posix
import Network.HTTP ()


-------------------------
-- Command line handling
-------------------------

validateHackageURI :: String -> Either String URI
validateHackageURI str = case parseURI str of
    Nothing  -> Left ("invalid URL " ++ str)
    Just uri -> validateHackageURI' uri

validateHackageURI' :: URI -> Either String URI
validateHackageURI' uri
    | not $ okayScheme (uriScheme uri) =
                                     Left $ "only http URLs are supported " ++ show uri
    | isNothing (uriAuthority uri) = Left $ "server name required in URL " ++ show uri
    | otherwise                    = Right uri
  where
    okayScheme "http:"  = True
    okayScheme "https:" = True
    okayScheme _        = False

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
        mapMaybe selectDetails
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

uriHostName :: URI -> Maybe String
uriHostName = fmap uriRegName . uriAuthority


newtype HttpSession a = HttpSession (ReaderT HttpEnv IO a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO)

data HttpEnv = HttpEnv { httpManager :: Manager
                       , initialHeaders :: RequestHeaders
                       }

mkRequest
    :: Method
    -> RequestHeaders
    -> URI
    -> HttpSession Request
mkRequest meth headers uri = do
    req0 <- liftIO $ requestFromURI uri
    return $ req0 { method = meth, requestHeaders = headers }

mkUploadRequest
    :: Method
    -> URI
    -> String        -- ^ MIME type
    -> Maybe String  -- ^ encoding
    -> RequestHeaders
    -> ByteString    -- ^ body
    -> HttpSession Request
mkUploadRequest meth uri mimetype mEncoding headers body = do
    req <- mkRequest meth (headers ++ headers') uri
    return $ req { requestBody = RequestBodyLBS body }
  where
    headers' = [ (hContentLength, BSS.pack $ show (BS.length body))
              , (hContentType, BSS.pack mimetype) ]
              ++ case mEncoding of
                Nothing       -> []
                Just encoding -> [ (hContentEncoding, BSS.pack encoding) ]

-- | Prohibit following of redirects.
noRedirects :: Request -> Request
noRedirects req = req { redirectCount = 0 }

runRequest :: Request
            -> (Response BodyReader -> IO a)
            -> HttpSession a
runRequest req0 k = HttpSession $ do
    env <- ask
    let req = req0 { requestHeaders = initialHeaders env ++ requestHeaders req0 }
    liftIO $ withResponse req (httpManager env) k

responseReadBSL :: Response BodyReader -> IO (Response BS.ByteString)
responseReadBSL rsp =
    traverse (fmap BS.fromChunks . brConsume) rsp

httpSession :: Verbosity -> String -> Version -> HttpSession a -> IO a
httpSession verbosity agent version (HttpSession action) = do
    manager <- newTlsManager
    let env = HttpEnv { httpManager = manager
                      , initialHeaders = [ (hUserAgent, BSS.pack $ agent ++ "/" ++ showVersion version) ]
                      }
    runReaderT action env

downloadFile :: URI -> FilePath -> HttpSession ()
downloadFile uri file = do
  liftIO $ putStrLn $ "downloading " ++ show uri ++ " to " ++ file
  let etagFile = file <.> "etag"
  metag <- liftIO $ catchJustDoesNotExistError
                        (Just <$> readFile etagFile)
                        (\_ -> return Nothing)
  case metag of
    Just etag -> do
      let headers = [(hIfNoneMatch, BSS.pack (quote etag))]
      req <- mkRequest "GET" headers uri
      runRequest req $ \rsp -> do
        case statusCode $ responseStatus rsp of
          304 -> putStrLn $ file ++ " unchanged with ETag " ++ etag
          200 -> writeDowloadedFileAndEtag rsp
          _   -> do rsp' <- responseReadBSL rsp
                    hPutStrLn stderr (showFailure uri rsp')

    Nothing -> do
        req <- mkRequest "GET" [] uri
        runRequest req $ \rsp ->
          case statusCode $ responseStatus rsp of
            200 -> writeDowloadedFileAndEtag rsp
            _   -> do rsp' <- responseReadBSL rsp
                      hPutStrLn stderr (showFailure uri rsp')

  where
    writeDowloadedFileAndEtag rsp = do
      bss <- brConsume (responseBody rsp)
      BS.writeFile file (BS.fromChunks bss)
      setETag file (unquote . BSS.unpack <$> lookup hETag (responseHeaders rsp))

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

-- | Like 'requestGET' but return @Nothing@ on 404 status.
requestGET' :: URI -> HttpSession (Maybe ByteString)
requestGET' uri = do
    req <- mkRequest "GET" headers uri
    runRequest req $ \rsp -> do
        case statusCode $ responseStatus rsp of
          404 -> return Nothing
          _   -> do rsp' <- responseReadBSL rsp
                    checkStatus uri rsp'
                    return $ Just (responseBody rsp')
  where
    headers = []

requestPUT :: URI -> String -> Maybe String -> ByteString -> HttpSession ()
requestPUT uri mimetype mEncoding body = do
    req <- mkUploadRequest "PUT" uri mimetype mEncoding [] body
    runRequest req $ \rsp -> do
        rsp' <- responseReadBSL rsp
        checkStatus uri rsp'

checkStatus :: URI -> Response ByteString -> IO ()
checkStatus uri rsp = case statusCode $ responseStatus rsp of
  -- 200 OK
  200 -> return ()
  -- 201 Created
  201 -> return ()
  -- 201 Created
  202 -> return ()
  -- 204 No Content
  204 -> return ()
  -- 400 Bad Request
  400 -> liftIO (warn normal (showFailure uri rsp)) >> return ()
  -- Other
  _code -> fail (showFailure uri rsp)

showFailure :: URI -> Response ByteString -> String
showFailure uri rsp = unlines
    [ "error: failed HTTP request"
    , "  status: " ++ show (responseStatus rsp)
    , "  url: " ++ show uri
    , "  response: " ++
        case lookup hContentType (responseHeaders rsp) of
          Just mimetype | "text/plain" `BSS.isPrefixOf` mimetype
                       -> '\n' : (unpackUTF8 . responseBody $ rsp)
          _            -> ""
    ]
