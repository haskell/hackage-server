{-# LANGUAGE RankNTypes, MultiParamTypeClasses, RecordWildCards  #-}

module Distribution.Server.Import
    ( importTar
    ) where

import Happstack.State (update)

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (decompress)
import Control.Monad
import Control.Monad.State.Class
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.Locale

import Distribution.Simple.Utils (fromUTF8)
import qualified Data.ByteString.Lazy.Char8 as BS8

import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import System.FilePath (splitDirectories, splitExtension, takeExtension)
import Data.List (isPrefixOf, isSuffixOf)
import Text.CSV hiding (csv)

import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.PackageDescription.Parse
    ( parsePackageDescription
    , ParseResult(..)
    )

import Distribution.Server.Export.Utils
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage, BlobId)

import qualified Distribution.Server.Distributions.Distributions as Distros
import Distribution.Server.Distributions.Distributions
    ( Distributions
    , DistroName
    , DistroVersions
    , DistroPackageInfo(..)
    )
import Distribution.Server.Distributions.State (ReplaceDistributions(..))

import Distribution.Server.Packages.State
    ( Documentation(..)
    , PackagesState(..)
    , ReplacePackagesState(..)
    , ReplaceDocumentation(..)
    )

import Distribution.Server.BuildReport.BuildReport (BuildReport)
import qualified Distribution.Server.BuildReport.BuildReport as Reports

import Distribution.Server.BuildReport.BuildReports (BuildReports, BuildReportId, BuildLog)
import qualified Distribution.Server.BuildReport.BuildReports as Reports

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Permissions as Permissions
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Permissions (Permissions,GroupName)
import Distribution.Server.Users.State (ReplacePermissions(..))

import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.PackageIndex (PackageIndex)
import Distribution.Package

import Distribution.Text hiding (parse)

-- | Takes apart the import tarball. If there are any errors,
-- the first error encounter is returned.
-- Otherwise the result of the import is injected into the running
-- happs state.
-- The format of the input is assumed to be tar.gz
importTar :: BlobStorage -> ByteString -> IO (Maybe String)
importTar storage tar
    = do
  res <- runImport storage .
                fromEntries .
                Tar.read .
                decompress $ tar
  case res of
    Left err -> return . Just $ err
    Right IS{..}
        -> do
      update $ ReplaceDocumentation isDocs
      update $ ReplacePackagesState $ PackagesState
                  isPackages
                  isBuildReps
                  isUsers
      update $ ReplacePermissions isPerms
      update $ ReplaceDistributions isDistributions isDistVersions
      return Nothing

fromEntries :: Tar.Entries -> Import ()
fromEntries Tar.Done = return ()
fromEntries (Tar.Fail err) = fail err
fromEntries (Tar.Next x xs) = fromEntry x >> fromEntries xs

fromEntry :: Tar.Entry -> Import ()
fromEntry entry
    = case Tar.entryContent entry of
        Tar.NormalFile bytes _ ->
            fromFile (Tar.entryPath entry) bytes

        Tar.Directory{} -> return () -- ignore directory entries

        _ -> fail $ "Unexpected entry: " ++ Tar.entryPath entry

fromFile :: FilePath -> ByteString -> Import ()
fromFile path contents
    = let paths = splitDirectories path
      in case paths of
           baseDir:rest | "export-" `isPrefixOf` baseDir
                                   -> go rest
           _ -> return () -- ignore unknown files

 where
   go ["users","auth.csv"]
       = importAuth contents

   go ["users","permissions.csv"]
       = importPermissions contents

   go ["build-reports", repIdString, "report.txt"]
       = importReport repIdString contents

   go ["build-reports", repIdString, "log.txt"]
       = importLog repIdString contents

   go ("package" : rest) = package rest contents

   go (["distros", filename])
       | takeExtension filename == ".csv"
           = importDistro filename contents

   go _ = return () -- ignore unknown files

package :: [String] -> ByteString -> Import ()
package [_, pkgIdStr, "documentation.tar.gz"] contents
    = do
  pkgId <- parse "package id" pkgIdStr
  blobId <- addFile contents
  addDocumentation pkgId blobId

package [pkgName, pkgTarName] contents
    | pkgName `isPrefixOf` pkgTarName
      && ".tar" `isSuffixOf` pkgTarName
    = let (pkgIdStr,_) = splitExtension pkgTarName
      in do

        pkgId <- parse "package id" pkgIdStr
        parts <- packageParts . Tar.read $ contents
        validateParts pkgIdStr parts
        let (source, Just cabalContents, Just uploadsContents) = parts
        uploads <- parseUploads uploadsContents
        pkgDesc <- parsePackageDesc cabalContents

        case uploads of
          (upTime,upUser):rest
              -> addPackage $ PkgInfo
                 { pkgInfoId = pkgId
                 , pkgDesc = pkgDesc
                 , pkgData = cabalContents
                 , pkgTarball = source
                 , pkgUploadTime = upTime
                 , pkgUploadUser = upUser
                 , pkgUploadOld = rest
                 }
          _ -> fail $ "Package " ++ show (display pkgId)
                ++ "requires at least one uploading user"


 where validateParts from (_,Nothing,_)
           = validateError from
       validateParts from (_,_,Nothing)
           = validateError from
       validateParts _ _
           = return ()

       validateError from
           = fail $ "Bad tar for " ++ from

       parseUploads :: ByteString -> Import [(UTCTime, UserId)]
       parseUploads file
           = case customParseCSV "uploads.csv" (bytesToString file) of
               Left err -> fail . show $ err
               Right records -> mapM fromRecord (drop 2 records) 

       fromRecord [userStr, timeStr]
           = do
         user <- parse "user id" userStr
         utcTime <- parseTime timeStr
         return (utcTime, user)
       fromRecord x = fail $ "Error handling upload record: " ++ show x

       parseTime str
           = case Time.parseTime defaultTimeLocale timeFormatSpec str of
               Nothing -> fail $ "Unable to parse time: " ++ str
               Just x  -> return x

       parsePackageDesc :: ByteString -> Import GenericPackageDescription
       parsePackageDesc file
           = case parsePackageDescription (bytesToString file) of
               ParseFailed err     -> fail . show $ err
               ParseOk _warnings a -> return a
package _ _ = return () -- ignore unknown files

-- The pieces needed for a package entry are stuffed together
-- in their own tarball, so we can gaurantee they arrive at the
-- same place in the tar entries stream for the import
packageParts :: Tar.Entries
             -> Import (Maybe BlobId     -- source tarball
                       ,Maybe ByteString -- .cabal file
                       ,Maybe ByteString -- uploads csv
                       )
packageParts entries
    = go entries (Nothing, Nothing, Nothing)
 where go Tar.Done x       = return x
       go (Tar.Fail err) _ = fail err
       go (Tar.Next one rest) x
           = processEntry one x >>= go rest

       processEntry entry x@(src, cbl, ups)
           = case Tar.entryContent entry of
               Tar.NormalFile contents _
                   -> case splitDirectories (Tar.entryPath entry) of
                        ["uploads.csv"] -> return (src, cbl, Just contents)
                        [file]
                            | ".cabal" `isSuffixOf` file
                                -> return (src, Just contents, ups)
                            | ".tar.gz" `isSuffixOf` file
                                -> do
                                blobId <- addFile contents
                                return (Just blobId, cbl, ups)
                        _ -> return x
               _ -> return x

importReport :: String -> ByteString -> Import ()
importReport repIdStr contents
    = do
  repId <- parse "report id" repIdStr
  case Reports.parse (bytesToString contents) of
    Left err -> fail err
    Right report -> insertBuildReport repId report

importLog :: String -> ByteString -> Import ()
importLog repIdStr contents
    = do
  repId <- parse "report id" repIdStr
  blobId <- addFile contents
  insertBuildLog repId (Reports.BuildLog blobId)

importAuth :: ByteString -> Import ()
importAuth contents
    = case customParseCSV "auth.csv" (bytesToString contents) of
        Left e -> fail e
        Right csv -> mapM_ fromRecord (drop 2 csv)

 where fromRecord
        [nameStr, idStr, "deleted", "none", ""]
            = do
          name <- parse "user name" nameStr
          user <- parse "user id" idStr
          insertUser user $ UserInfo name Deleted

       fromRecord
        [nameStr, idStr, authType, "basic", auth]
            = do
          name <- parse "user name" nameStr
          user <- parse "user id" idStr
          authInfo <- parseAuth authType
          insertUser user $ UserInfo name $ authInfo auth

       fromRecord x = fail $ "Error processing auth record: " ++ show x

       -- parseAuth :: String -> Import (String -> UserAuth)
       parseAuth "enabled"  = return $ Enabled  . PasswdHash
       parseAuth "disabled" = return $ Disabled . PasswdHash
       parseAuth sts = fail $ "unable to parse auth status: " ++ sts

importPermissions :: ByteString -> Import ()
importPermissions contents
    = case customParseCSV "permissions.csv" (bytesToString contents) of
        Left e -> fail e
        Right csv -> mapM_ fromRecord (drop 1 csv)

 where fromRecord
        (groupStr:users)
            = do
          groupName <- parse "group name" groupStr
          forM_ users $ \userStr ->
              parse "user id" userStr >>= addPermission groupName
       fromRecord x = fail $ "Error handling permissions record: " ++ show x

importDistro :: String -> ByteString -> Import ()
importDistro filename contents
    = case customParseCSV filename (bytesToString contents) of
        Left e -> fail e
        Right csv -> do
          let [[distroStr]] = take 1 $ drop 1 csv
          distro <- parse "distribution name" distroStr
          addDistribution distro
          mapM_ (fromRecord distro) (drop 3 csv)

 where fromRecord distro
        [ packageStr
        , versionStr
        , uri
        ] = do
         packageName <- parse "package name" packageStr
         version <- parse "version" versionStr
         addDistroPackage distro packageName $ Distros.DistroPackageInfo version uri
       fromRecord _ x
           = fail $
             "Invalid distribution record in " ++ filename ++ " : " ++ show x

-- Parse a string, throw an error if it's bad
parse :: Text a => String -> String -> Import a
parse label text
    = case simpleParse text of
        Nothing
            -> fail $
               "Unable to parse " ++ label ++ " : " ++ show text
        Just a -> return a
                            
bytesToString :: ByteString -> String
bytesToString = fromUTF8 . BS8.unpack

-- Import is a state monad over the IS data type

{-

The import tar-ball is read in and we update the import-state.
That way, if we mess up half-way through, we aren't left with
an inconsistent server-state.

NOTE the blob-storage is updated, but since it's a quasi-functional
strucuture, I'm okay with this.

-}

data IS
    = IS
      { isUsers :: !Users
      , isPerms :: !Permissions
      , isPackages :: !(PackageIndex PkgInfo)
      , isDocs :: !Documentation
      , isBuildReps :: !BuildReports
      , isDistributions :: !Distributions
      , isDistVersions :: !DistroVersions
      , isStorage :: BlobStorage
      }

addFile :: ByteString -> Import BlobId
addFile file = do
  store <- gets isStorage
  io2imp $ BlobStorage.add store file

insertUser :: UserId -> UserInfo -> Import ()
insertUser user info
    = do
  s <- get
  case Users.insert user info (isUsers s) of
    Nothing -> fail $ "Duplicate user id for user: " ++ display user
    Just users' -> do
      put $ s {isUsers = users'}

addPermission :: GroupName -> UserId -> Import ()
addPermission group user
    = modify $ \is ->
      is {isPerms = Permissions.addToGroup group user (isPerms is)}

addPackage :: PkgInfo -> Import ()
addPackage pkg
    = modify $ \is ->
      is {isPackages = PackageIndex.insert pkg (isPackages is)}

addDocumentation :: PackageIdentifier -> BlobId -> Import ()
addDocumentation pkgId blob
    = modify $ \is ->
      is { isDocs = Documentation (Map.insert pkgId blob (documentation (isDocs is)))}

insertBuildReport :: BuildReportId -> BuildReport -> Import ()
insertBuildReport reportId report
    = do
  s <- get
  case Reports.insertReport (isBuildReps s) reportId report of
    Nothing -> fail $ "Duplicate report id for report: " ++ display reportId
    Just buildReps' -> do
      put $ s { isBuildReps = buildReps' }

insertBuildLog :: BuildReportId -> BuildLog -> Import ()
insertBuildLog reportId buildLog
    = do
  s <- get
  case Reports.addBuildLog (isBuildReps s) reportId buildLog of
    Nothing -> fail $ "Duplicate report id for log: " ++ display reportId
    Just buildReps' -> do
      put $ s { isBuildReps = buildReps' }

addDistribution :: DistroName -> Import ()
addDistribution distro
    = do
  is <- get
  case Distros.addDistro distro (isDistributions is) of
    Nothing -> fail $ "Could not add distro: " ++ display distro
    Just dists -> put is{isDistributions = dists}

addDistroPackage :: DistroName -> PackageName -> DistroPackageInfo -> Import ()
addDistroPackage distro package info
    = do
  is <- get
  put is{isDistVersions = Distros.addPackage distro package info (isDistVersions is)}


-- implementation of the Import data type

newtype Import a
    = Imp 
      {unImp :: forall r .
          (String -> IO r)   -- error
       -> (a -> IS -> IO r)  -- success
       -> IS                 -- state
       -> IO r
      }

runImport :: BlobStorage -> Import () -> IO (Either String IS)
runImport storage imp = unImp imp err k initState
 where k _ s = return $ Right s

       err = return . Left

       initState
           = IS
              Users.empty
              Permissions.empty
              (PackageIndex.fromList [])
              (Documentation Map.empty)
              Reports.empty
              Distros.emptyDistributions
              Distros.emptyDistroVersions
              storage


instance Functor Import where
    f `fmap` g
        = Imp $ \err k ->
          unImp g err (k . f)

instance Monad Import where
    return a = Imp $ \_ k -> k a

    m >>= f
      = Imp $ \err k ->
        unImp m err $ \a ->
        unImp (f a) err k

    m >> n
      = Imp $ \err k ->
        unImp m err $ \_ ->
        unImp n err k

    fail str
        = Imp $ \err _k _s ->
          err str

instance MonadState IS Import where
    get
        = Imp $ \_ k s ->
          k s s
    put s
        = Imp $ \_ k _ ->
          {- s `seq` -} k () s

-- not sure if this is as good as it could be,
-- but it seems that k must be fully applied if I want
-- to unwrap x
--
-- todo: catch IO errors and re-throw as Import errors?
io2imp :: IO a -> Import a
io2imp x
    = Imp $ \_ k s ->
      x >>= \v -> k v s

-- |Chops off the last entry if it's null.
-- I'm not sure why parseCSV does this, but it's
-- irritating.
customParseCSV :: String -> String -> Either String CSV
customParseCSV filename inp
    = case parseCSV filename inp of
        Left err -> Left . show $ err
        Right csv -> Right . chopLastRecord $ csv

 where
   chopLastRecord [] = []
   chopLastRecord ([""]:[]) = []
   chopLastRecord (x:xs) = x : chopLastRecord xs
