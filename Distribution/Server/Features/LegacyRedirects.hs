module Distribution.Server.Features.LegacyRedirects (
    legacyRedirectsFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Instances ()

import Happstack.Server as Happs
import Distribution.Server.Util.Happstack (remainingPath)

import Distribution.Package
         ( PackageIdentifier(..), packageName, PackageId )
import Distribution.Text
         ( display, simpleParse )

import qualified System.FilePath.Posix as Posix (joinPath, splitExtension)
import Control.Monad (msum, mzero)


-- | A feature to provide redirection for URLs that existed in the first
-- incarnation of the hackage server.
--
legacyRedirectsFeature :: HackageModule
legacyRedirectsFeature = HackageModule {
    featureName = "legacy",
    -- get rid of trailing resource and manually create a mapping?
    resources   = [(resourceAt "/...") { resourceGet = [("", \_ _ -> serveLegacyRedirects)] }],
    dumpBackup    = Nothing,
    restoreBackup = Nothing
}

-- | Support for the old URL scheme from the first version of hackage.
--
serveLegacyRedirects :: ServerPart Response
serveLegacyRedirects = msum
  [ dir "packages" $ msum
      [ dir "archive" $ serveArchiveTree
      , simpleMove "hackage.html"    "/"
      , postedMove "upload"          "/upload"
      , postedMove "check"           "/check"
      , simpleMove "00-index.tar.gz" "/packages/index.tar.gz"
        --also search.html, advancedsearch.html, accounts.html, and admin.html
      ]
  , dir "cgi-bin" $ dir "hackage-scripts" $ msum
      [ dir "package" $ path $ \packageId -> methodSP GET $
          movedPermanently ("/package/" ++ display (packageId :: PackageId)) $
          toResponse ""
      , dir "protected" $ postedMove "upload"  "/upload"
      , postedMove "check"  "/check"
      ]
  ]
  where
    -- HTTP 301 is suitable for permanently redirecting pages
    simpleMove from to = dir from $ methodSP GET $ movedPermanently to (toResponse "")
    -- HTTP 307 makes the client resubmit the post request to a different URL,
    -- something particular to this status code
    postedMove from to = dir from $ methodSP POST $ tempRedirect to (toResponse "")

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
  , path $ \nameStr -> do
     let Just name = simpleParse nameStr
     msum
      [ path $ \version ->
        let pkgid = PackageIdentifier {pkgName = name, pkgVersion = version}
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

