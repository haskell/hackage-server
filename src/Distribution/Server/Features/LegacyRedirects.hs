-- TODO: check why this is needed:
{-# OPTIONS_GHC -fsimpl-tick-factor=150 #-}
module Distribution.Server.Features.LegacyRedirects (
    legacyRedirectsFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Features.Upload

import Distribution.Package
         ( PackageIdentifier(..), packageName, PackageId )
import Distribution.Text
         ( display, simpleParse )

import Distribution.Version ( Version, nullVersion )

import qualified System.FilePath.Posix as Posix (joinPath, splitExtension)


-- | A feature to provide redirection for URLs that existed in the first
-- incarnation of the hackage server.
--
legacyRedirectsFeature :: UploadFeature -> HackageFeature
legacyRedirectsFeature upload = (emptyHackageFeature "legacy") {
    -- get rid of trailing resource and manually create a mapping?
    featureResources =
      [ (resourceAt "/..") {
            resourceGet = [("", \_ -> serveLegacyGets)]
          , resourcePost = [("", \_ -> serveLegacyPosts upload)]
          }
      ]
  , featureState = []
  }

-- | Support for the old URL scheme from the first version of hackage.
--

-- | POST for package upload, particularly for cabal-install compatibility.
--
-- "check" no longer exists; it's now "candidates", and probably
-- provides too different functionality to redirect
serveLegacyPosts :: UploadFeature -> ServerPartE Response
serveLegacyPosts upload = msum
  [ dir "packages" $ msum
      [ dir "upload" $ movedUpload
    --, postedMove "check"  "/check"
      ]
  , dir "cgi-bin" $ dir "hackage-scripts" $ msum
      [ dir "protected" $ dir "upload-pkg" $ movedUpload
    --, postedMove "check"  "/check"
      ]
  , dir "upload" movedUpload
  ]
  where

    -- We assume we don't need to serve a fancy HTML response
    movedUpload :: ServerPartE Response
    movedUpload = nullDir >> do
      upResult <- uploadPackage upload
      ok $ toResponse $ unlines $ uploadWarnings upResult



-- | GETs, both for cabal-install to use, and for links scattered throughout the web.
serveLegacyGets :: ServerPartE Response
serveLegacyGets = msum
  [ simpleMove "00-index.tar.gz" "/packages/index.tar.gz"
  , simpleMove "00-index.tar" "/packages/index.tar"
  , dir "packages" $ msum
      [ dir "archive" $ serveArchiveTree
      , simpleMove "hackage.html"    "/"
      , simpleMove "00-index.tar.gz" "/packages/index.tar.gz"
        --also search.html, advancedsearch.html, accounts.html, and admin.html
      ]
  , dir "cgi-bin" $ dir "hackage-scripts" $ msum
      [ dir "package" $ path $ \packageId -> method GET >> nullDir >>
          (movedPermanently ("/package/" ++ display (packageId :: PackageId)) $
           toResponse "")
      ]
  , dir "package" $ path $ \fileName -> method GET >> nullDir >>
      case Posix.splitExtension fileName of
        (fileName', ".gz") -> case Posix.splitExtension fileName' of
          (packageStr, ".tar") -> case simpleParse packageStr of
            Just pkgid ->
              movedPermanently (packageTarball pkgid) $ toResponse ""
            _ -> mzero
          _ -> mzero
        _ -> mzero
  , simpleMove "recent"      "/packages/recent"
  , simpleMove "recent.html" "/packages/recent.html"
  , simpleMove "recent.rss"  "/packages/recent.rss"
  ]

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
volToVersion Latest = nullVersion
volToVersion (V v)  = v

serveArchiveTree :: ServerPartE Response
serveArchiveTree = msum
  [ simpleMove "pkg-list.html" "/packages/"
  , dir "package" $ path $ \fileName -> method GET >> nullDir >>
   case Posix.splitExtension fileName of
    (fileName', ".gz") -> case Posix.splitExtension fileName' of
       (packageStr, ".tar") -> case simpleParse packageStr of
          Just pkgid ->
            movedPermanently (packageTarball pkgid) $ toResponse ""
          _ -> mzero
       _ -> mzero
    _ -> mzero
  , simpleMove "00-index.tar.gz" "/packages/index.tar.gz"
  , simpleMove "recent.html" "/packages/recent"
  , simpleMove "recent.rss"  "/packages/recent.rss"
  , simpleMove "00-hoogle.tar.gz" "/packages/hoogle.tar.gz"
  , path $ \name -> do
     msum
      [ path $ \version ->
        let pkgid = PackageIdentifier {pkgName = name, pkgVersion = volToVersion version}
        in msum
         [ simpleMove (display pkgid ++ ".tar.gz") (packageTarball pkgid)
         , simpleMove (display name ++ ".cabal")   (cabalPath pkgid)

         , dir "doc" $ dir "html" $ remainingPath $ \paths ->
             let doc = Posix.joinPath paths
             in simpleMoveTo (docPath pkgid doc)
         ]
      ]
  ]
  where
    docPath pkgid file = "/package/" ++ display pkgid ++ "/" ++ "docs/" ++ file

    cabalPath pkgid = "/package/" ++ display pkgid ++ "/"
                   ++ display (packageName pkgid) ++ ".cabal"

packageTarball :: PackageId -> String
packageTarball pkgid = "/package/" ++ display pkgid
                    ++ "/" ++ display pkgid ++ ".tar.gz"

-- HTTP 301 is suitable for permanently redirecting pages
simpleMove :: String -> String -> ServerPartE Response
simpleMove from to = dir from $ simpleMoveTo to

simpleMoveTo :: String -> ServerPartE Response
simpleMoveTo to = method GET >> nullDir >> movedPermanently to (toResponse "")
