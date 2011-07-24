module Distribution.Server.Features.LegacyRedirects (
    legacyRedirectsFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Upload

import Distribution.Server.Util.Happstack (remainingPath)

import Distribution.Package
         ( PackageIdentifier(..), packageName, PackageId )
import Distribution.Text
         ( display, simpleParse )

import Data.Version ( Version (..) )

import qualified System.FilePath.Posix as Posix (joinPath, splitExtension)

import Control.Applicative ( (<$>) )
import Control.Monad (msum, mzero)


-- | A feature to provide redirection for URLs that existed in the first
-- incarnation of the hackage server.
--
legacyRedirectsFeature :: UploadFeature -> HackageModule
legacyRedirectsFeature upload = HackageModule {
    featureName = "legacy",
    -- get rid of trailing resource and manually create a mapping?
    resources   = [(resourceAt "/..") { resourceGet = [("", \_ -> serveLegacyGets)], resourcePost = [("", \_ -> serveLegacyPosts upload)] }],
    dumpBackup    = Nothing,
    restoreBackup = Nothing
}

-- | Support for the old URL scheme from the first version of hackage.
--

-- | POST for package upload, particularly for cabal-install compatibility.
--
-- "check" no longer exists; it's now "candidates", and probably
-- provides too different functionality to redirect
serveLegacyPosts :: UploadFeature -> ServerPart Response
serveLegacyPosts upload = msum
  [ dir "packages" $ msum
      [ dir "upload" $ movedUpload
    --, postedMove "check"  "/check"
      ]
  , dir "cgi-bin" $ dir "hackage-scripts" $ msum
      [ dir "protected" $ dir "upload" $ movedUpload
    --, postedMove "check"  "/check"
      ]
  , dir "upload" movedUpload
  ]
  where

    -- We assume we don't need to serve a fancy HTML response
    movedUpload :: ServerPart Response
    movedUpload = nullDir >> do
      upResult <- runServerPartE (uploadPackage upload)
      ok $ toResponse $ unlines $ uploadWarnings upResult



-- | GETs, both for cabal-install to use, and for links scattered throughout the web.
--
-- method is already guarded against, but methodSP is a nicer combinator than nullDir.
serveLegacyGets :: ServerPart Response
serveLegacyGets = msum
  [ dir "packages" $ msum
      [ dir "archive" $ serveArchiveTree
      , simpleMove "hackage.html"    "/"
      , simpleMove "00-index.tar.gz" "/packages/index.tar.gz"
        --also search.html, advancedsearch.html, accounts.html, and admin.html
      ]
  , dir "cgi-bin" $ dir "hackage-scripts" $ msum
      [ dir "package" $ path $ \packageId -> methodSP GET $
          movedPermanently ("/package/" ++ display (packageId :: PackageId)) $
          toResponse ""
      ]
  ]
  where
    -- HTTP 301 is suitable for permanently redirecting pages
    simpleMove from to = dir from $ methodSP GET $ movedPermanently to (toResponse "")

-- Some of the old-style paths may contain a version number
-- or the text 'latest'. We represent the path '$pkgName/latest'
-- as a package id of '$pkgName' in the new url schemes.

data VersionOrLatest
    = V Version
    | Latest

instance FromReqURI VersionOrLatest where
    fromReqURI "latest" = Just Latest
    fromReqURI str = V <$> fromReqURI str

volToVersion :: VersionOrLatest -> Version
volToVersion Latest = Version [] []
volToVersion (V v)  = v

serveArchiveTree :: ServerPart Response
serveArchiveTree = msum
  [ dir "pkg-list.html" $ methodSP GET $ movedPermanently "/packages/" (toResponse "")
  , dir "package" $ path $ \fileName -> methodSP GET $
   case Posix.splitExtension fileName of
    (fileName', ".gz") -> case Posix.splitExtension fileName' of
       (packageStr, ".tar") -> case simpleParse packageStr of
          Just pkgid ->
            movedPermanently (packageTarball pkgid) $ toResponse ""
          _ -> mzero
       _ -> mzero
    _ -> mzero
  , dir "00-index.tar.gz" $ methodSP GET $ movedPermanently "/packages/index.tar.gz" (toResponse "")
  , path $ \name -> do
     msum
      [ path $ \version ->
        let pkgid = PackageIdentifier {pkgName = name, pkgVersion = volToVersion version}
        in msum
         [ let dirName = display pkgid ++ ".tar.gz"
           in dir dirName $ methodSP GET $
              movedPermanently (packageTarball pkgid) (toResponse "")

         , let fileName = display name ++ ".cabal"
           in dir fileName $ methodSP GET $
              movedPermanently (cabalPath pkgid) (toResponse "")

         , dir "doc" $ dir "html" $ remainingPath $ \paths ->
             let doc = Posix.joinPath paths
             in methodSP GET $
                movedPermanently (docPath pkgid doc) (toResponse "")
         ]
      ]
  ]
  where
    packageTarball :: PackageId -> String
    packageTarball pkgid = "/package/" ++ display pkgid ++ "/" ++ display pkgid ++ ".tar.gz"

    docPath pkgid file = "/package/" ++ display pkgid ++ "/" ++ "doc/" ++ file

    cabalPath pkgid = "/package/" ++ display pkgid ++ "/"
                   ++ display (packageName pkgid) ++ ".cabal"

