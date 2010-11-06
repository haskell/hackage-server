module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import System.Environment
import System.Locale (defaultTimeLocale)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.FilePath.Posix

import Happstack.Server.HTTP.FileServe (mimeTypes, guessContentType)
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as HTTP
import qualified Network.Browser as Browser
import Network.URI (URI(..), URIAuth(..), parseURI)
import Network.HTTP.Headers (HeaderName(..), replaceHeader)

import Distribution.Server.Backup.UploadLog as UploadLog (read, Entry(..))
import Distribution.Server.Users.Types (UserName)
import Distribution.Server.Util.Index as PackageIndex (read)
import Distribution.Server.Auth.Basic as Auth
import Distribution.Package
import Distribution.Text
import Data.Version

main = withArgs $ \source dest -> do
    hSetBuffering stdout NoBuffering
    userStr <- prompt "User name"
    passStr <- prompt "Password"
    let conf = MirrorConfig userStr passStr source dest

    entries <- downloadLogOld conf

    -- The destination index may include cabal entries for packages which
    -- don't have tarballs. More inspection would be needed to mirror all
    -- of the tarballs.
    index <- downloadIndexNew conf

    let diffed = diffAllEntries (makeEntryMap entries) (makeIndexMap index)
        diffNum = Map.size diffed
    putStrLn $ concat ["... Packages to mirror (", show diffNum, "): "]
    putStrLn $ "> " ++ intercalate ", " (map display $ Map.keys diffed)
    forFold (Map.toList diffed) 1 $ \index (pkgid, entry) -> do
        putStr $ show index ++ ". Downloading " ++ display pkgid ++ "... "
        res <- downloadPackageOld conf pkgid
        case res of
            Left err -> do
                putError $ "could not find package with upload entry " ++ show (display entry) ++ " on source server."
            Right bstr -> do
                putStr $ "mirroring... "
                res' <- putPackage conf entry bstr
                case res' of
                    Nothing  -> putStrLn "success"
                    Just err -> putError $ "unsuccessful putting " ++ display pkgid ++ ": " ++ show err
        sleepSec
        return (index+1)
    putStrLn "All done"
  where prompt str = do
            putStr $ str ++ ": "
            getLine
        putError str = do
            putStrLn ""
            putStrLn $ "*** Error: " ++ str
        forFold list init func = foldM func init list
        sleepSec = threadDelay (1*1000000)
        withArgs func = getArgs >>= \args -> case args of
            [a, b] -> func a b
            _ -> mapM_ putStrLn
              [ "Usage: hackage-mirror [source host] [destination host]"
              , "- hackage-mirror copies from hackage-script to a hackage-server instance."
              , "- You will be prompted for your name and password at the destination server."
              , "- The current behavior of this client is to copy all tarballs for which"
              , "  no cabal file exists in the destination (it will not overwrite cabal"
              , "  files without tarballs)." ]
        

data MirrorConfig = MirrorConfig {
    mirrorName :: String,
    mirrorPass :: String,
    hackageSrc :: String,
    hackageDst :: String
    -- can add additional config options, including behavior
}

srcUri, dstUri :: MirrorConfig -> String
srcUri = hackageSrc
dstUri = hackageDst

--------------------------------------------------------------------------------
-- Upload a package to the new server
putPackage :: MirrorConfig -> UploadLog.Entry -> ByteString -> IO (Maybe String)
putPackage config (UploadLog.Entry time uname pkgid) pkgData = do
    let timeStr = formatTime defaultTimeLocale "%c" time
        nameStr = display uname
        userData = (mirrorName config, mirrorPass config)
        pkgUri  = dstUri config </> "package" </> display pkgid
                                </> display pkgid <.> "tar.gz"
        pkgUri' = fromJust $ parseURI pkgUri
    (ctype, reqStr) <- makeMultipart
        [("date", timeStr), ("user", nameStr)]
        [("package", display pkgid ++ ".tar.gz", pkgData)]
    (_, rsp) <- Browser.browse $ do
        Browser.setOutHandler $ \_ -> return ()
        Browser.setAllowRedirects True -- handle HTTP redirects
        Browser.setAuthorityGen $ \uri realm -> return $ if isHackage uri pkgUri' && realm==authorizationRealm then Just userData else Nothing
        let req = HTTP.mkRequest HTTP.PUT pkgUri' :: HTTP.Request String
        Browser.request
            $ replaceHeader HdrContentLength (show $ BS.length reqStr)
            $ replaceHeader HdrContentType ctype
            $ req { HTTP.rqBody = BS.unpack reqStr }
    return $ case HTTP.rspCode rsp of
        (2, _, _) -> Nothing
        _ -> Just (HTTP.rspReason rsp)
  where isHackage test real = fmap uriRegName (uriAuthority test) == fmap uriRegName (uriAuthority real)

makeMultipart :: [(String, String)] -> [(String, String, ByteString)]
              -> IO (String, ByteString)
makeMultipart fields files = do
    -- or rather randomly generate a boundary
    let boundary = "4369867349876958439243289738"
        boundaryStr = "--" ++ boundary
        fieldString name value =
            unlines [boundaryStr, "Content-Disposition: form-data; name=\"" ++ name ++ "\"", "", value]
        fileString name filename = 
            let mime = fromMaybe "application/octet-stream" (guessContentType mimeTypes filename)
            in unlines [boundaryStr, "Content-Disposition: form-data; name=\"" ++ name ++ "\"; filename=\"" ++ filename ++ "\"",
                        "Content-Type: " ++ mime, ""]
    return $ (,) ("multipart/form-data; boundary=" ++ boundary) $ BS.concat
      [ BS.pack $ concatMap (uncurry fieldString) fields
      , BS.concat $ map (\(name, filename, value) -> BS.concat [BS.pack $ fileString name filename, value, BS.pack "\n"]) files
      , BS.pack $ boundaryStr ++ "--\n\n"
      ]

--------------------------------------------------------------------------------
-- Download individual packages. If one fails, the mirror client keeps on going.
downloadPackageOld :: MirrorConfig -> PackageId -> IO (Either String ByteString)
downloadPackageOld config pkgid =
    downloadPackage $
        srcUri config
    </> "packages/archive"
    </> (display $ packageName pkgid)
    </> (display $ packageVersion pkgid)
    </> display pkgid <.> "tar.gz"
-- /packages/archive/<package>/<version>/<package>-<version>.tar.gz

downloadPackageNew :: MirrorConfig -> PackageId -> IO (Either String ByteString)
downloadPackageNew config pkgid =
    downloadPackage $
        srcUri config
    </> "package"
    </> display pkgid
    </> display pkgid <.> ".tar.gz"
-- /package/<package>-<version>/<package>-<version>.tar.gz

downloadPackage :: String -> IO (Either String ByteString)
downloadPackage uri = do
    res <- HTTP.simpleHTTP (HTTP.getRequest uri)
    return $ case res of
        Left err                                        -> Left  (show err)
        Right r@HTTP.Response{ HTTP.rspCode = (2,0,0) } -> Right (BS.pack $ HTTP.rspBody r)
        Right r                                         -> Left  (HTTP.rspReason r)

--------------------------------------------------------------------------------
-- Downloading logs: get package list and upload information from source Hackage
downloadLogOld :: MirrorConfig -> IO [UploadLog.Entry]
downloadLogOld config = do
    putStrLn $ "Getting source log " ++ logURI
    entries <- downloadLog logURI
    putStrLn $ show (length entries) ++ " entries"
    return entries
  where
    logURI = srcUri config </> "packages/archive/log"

-- presently, there is no downloadLogNew
-- it could use the fact that users/times are stored in the metadata
-- of the index tarball, assuming it's not been tampered with

downloadLog :: String -> IO [UploadLog.Entry]
downloadLog uri = do
    rstr <- HTTP.getResponseBody =<< HTTP.simpleHTTP (HTTP.getRequest uri)
    case UploadLog.read rstr of
        Right elist -> return elist
        Left  err -> error $ "Error parsing log at " ++ uri ++ ": " ++ err

--------------------------------------------------------------------------------
-- Downloading indices: get package list from destination Hackage, to diff with source
downloadIndexOld :: MirrorConfig -> IO [PackageId]
downloadIndexOld config =
    downloadIndex $ srcUri config </> "/packages/archive/00-index.tar.gz"

downloadIndexNew :: MirrorConfig -> IO [PackageId]
downloadIndexNew config = do
    putStrLn $ "Getting destination index " ++ indexURI
    index <- downloadIndex indexURI
    putStrLn $ show (length index) ++ " entries"
    return index
  where
    indexURI = dstUri config </> "packages/index.tar.gz"

downloadIndex :: String -> IO [PackageId]
downloadIndex uri = do
    rstr <- HTTP.getResponseBody =<< HTTP.simpleHTTP (HTTP.getRequest uri)
    case PackageIndex.read const (GZip.decompress $ BS.pack rstr) of
        Left err   -> error $ "Error parsing index at " ++ uri ++ ": " ++ err
        Right pkgs -> return pkgs

--------------------------------------------------------------------------------
-- Diffing between entries and indices.
makeEntryMap :: [UploadLog.Entry] -> Map PackageId (UTCTime, UserName)
makeEntryMap entries =
  Map.fromList
    [ (pkg, (time, uname))
        | UploadLog.Entry time uname pkg <- entries
        , null . versionTags $ packageVersion pkg ]

makeIndexMap :: [PackageId] -> Map PackageId ()
makeIndexMap = Map.fromList . map (flip (,) ())

-- This is one of many possible diff functions. Another approach would be
-- to order entries by upload time and mirror the most recent until an
-- already-seen one is found.
diffAllEntries :: Map PackageId (UTCTime, UserName) -> Map PackageId () -> Map PackageId UploadLog.Entry
diffAllEntries entryMap indexMap =
      Map.mapWithKey remakeEntry
    $ Map.difference entryMap indexMap
  where remakeEntry pkg (time, uname) = UploadLog.Entry time uname pkg

