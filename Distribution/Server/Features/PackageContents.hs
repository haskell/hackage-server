{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageContents (
    PackageContentsFeature(..),
    PackageContentsResource(..),
    initPackageContentsFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache

import Distribution.Server.Packages.ChangeLog
import Distribution.Server.Packages.Readme
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render
import Distribution.Server.Features.Users
import Distribution.Server.Util.ServeTarball
import Distribution.Server.Util.Markdown (renderMarkdown, supposedToBeMarkdown)
import Distribution.Server.Pages.Template (hackagePage)

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as BS (ByteString, toStrict)
import qualified Text.XHtml.Strict as XHtml
import qualified Distribution.Utils.ShortText as ST
import           Text.XHtml.Strict ((<<), (!))


data PackageContentsFeature = PackageContentsFeature {
    packageFeatureInterface :: HackageFeature,
    packageContentsResource :: PackageContentsResource,

    -- necessary information for the representation of a package resource
    -- This needs to be here in order to extract from the tar file
    packageRender :: PkgInfo -> IO PackageRender
}

instance IsHackageFeature PackageContentsFeature where
    getFeatureInterface = packageFeatureInterface

data PackageContentsResource = PackageContentsResource {
    packageContents             :: Resource,
    packageContentsChangeLog    :: Resource,
    packageContentsReadme       :: Resource,
    packageContentsChangeLogUri :: PackageId -> String
}

initPackageContentsFeature :: ServerEnv
                           -> IO (CoreFeature
                               -> TarIndexCacheFeature
                               -> UserFeature
                               -> IO PackageContentsFeature)
initPackageContentsFeature _ = do
    return $ \core tarIndexCache user -> do
      let feature = packageContentsFeature core tarIndexCache user

      return feature

packageContentsFeature :: CoreFeature
                       -> TarIndexCacheFeature
                       -> UserFeature
                       -> PackageContentsFeature

packageContentsFeature CoreFeature{ coreResource = CoreResource{
                                      packageInPath
                                    , lookupPackageId
                                    }
                                  }
                       TarIndexCacheFeature{packageTarball, findToplevelFile}
                       UserFeature{queryGetUserDb}
  = PackageContentsFeature{..}
  where
    packageFeatureInterface = (emptyHackageFeature "package-contents") {
        featureResources =
          map ($ packageContentsResource) [
              packageContents
            , packageContentsChangeLog
            , packageContentsReadme
            ]
      , featureState = []
      , featureDesc = "The PackageContents feature shows the contents of packages and caches their TarIndexes"
      }

    packageContentsResource = PackageContentsResource {
          packageContents = (resourceAt "/package/:package/src/..") {
              resourceGet = [("", serveContents)]
            }
        , packageContentsChangeLog = (resourceAt "/package/:package/changelog.:format") {
              resourceGet = [("txt",  serveChangeLogText)
                            ,("html", serveChangeLogHtml)]
            }
        , packageContentsReadme = (resourceAt "/package/:package/readme.:format") {
              resourceGet = [("txt",  serveReadmeText)
                            ,("html", serveReadmeHtml)]
            }
        , packageContentsChangeLogUri = \pkgid ->
            renderResource (packageContentsChangeLog packageContentsResource) [display pkgid, display (packageName pkgid)]
        }

    packageRender :: PkgInfo -> IO PackageRender
    packageRender pkg = do
        users <- queryGetUserDb
        changeLog <- findToplevelFile pkg isChangeLogFile
                 >>= either (\_ -> return Nothing) (return . Just)
        readme    <- findToplevelFile pkg isReadmeFile
                 >>= either (\_ -> return Nothing) (return . Just)
        let render = doPackageRender users pkg
        return $ render { rendChangeLog = changeLog, rendReadme = readme }

{-------------------------------------------------------------------------------
  TODO: everything below is duplicated in PackageCandidates.
-------------------------------------------------------------------------------}

    -- result: changelog or not-found error
    serveChangeLogText :: DynamicPath -> ServerPartE Response
    serveChangeLogText dpath = do
      pkg        <- packageInPath dpath >>= lookupPackageId
      mChangeLog <- liftIO $ findToplevelFile pkg isChangeLogFile
      case mChangeLog of
        Left err ->
          errNotFound "Changelog not found" [MText err]
        Right (tarfile, etag, offset, filename) -> do
          cacheControl [Public, maxAgeDays 30] etag
          liftIO $ serveTarEntry tarfile offset filename

    serveChangeLogHtml :: DynamicPath -> ServerPartE Response
    serveChangeLogHtml dpath = do
      pkg        <- packageInPath dpath >>= lookupPackageId
      mChangeLog <- liftIO $ findToplevelFile pkg isChangeLogFile
      let pkgId   = packageId pkg
      let url     = packageURL pkgId
      let pkgName = display pkgId
      case mChangeLog of
        Left _ -> do
          let message = [MText "Package ", MLink pkgName url, MText " has no changelog file in source distribution. "]
          let home = homepage $ packageDescription $ pkgDesc pkg
          if ST.null home then
            errNotFound "Changelog not found" message
          else
            let homeUrl = ST.fromShortText home in
            errNotFound "Changelog not found" (message ++ [MText "You may find one at the package home page: ", MLink homeUrl homeUrl])
        Right (tarfile, etag, offset, filename) -> do
          contents <- either (\err -> errInternalError [MText err])
                             (return . snd)
                  =<< liftIO (loadTarEntry tarfile offset)
          cacheControl [Public, maxAgeDays 30] etag
          return $ toResponse $ Resource.XHtml $
            let title  = "Changelog for " ++ pkgName
                title2 = "Changelog for " XHtml.+++ (XHtml.anchor ! [XHtml.href url] << pkgName)
            in hackagePage title
                 [ XHtml.h2 << title2
                 , XHtml.thediv ! [XHtml.theclass "embedded-author-content"]
                               << if supposedToBeMarkdown filename
                                    then renderMarkdown filename contents
                                    else XHtml.thediv ! [XHtml.theclass "preformatted"]
                                                     << unpackUtf8 contents
                 ]

    serveReadmeText :: DynamicPath -> ServerPartE Response
    serveReadmeText dpath = do
      pkg     <- packageInPath dpath >>= lookupPackageId
      mReadme <- liftIO $ findToplevelFile pkg isReadmeFile
      case mReadme of
        Left err ->
          errNotFound "Readme not found" [MText err]
        Right (tarfile, etag, offset, filename) -> do
          cacheControl [Public, maxAgeDays 30] etag
          liftIO $ serveTarEntry tarfile offset filename

    serveReadmeHtml :: DynamicPath -> ServerPartE Response
    serveReadmeHtml dpath = do
      pkg     <- packageInPath dpath >>= lookupPackageId
      mReadme <- liftIO $ findToplevelFile pkg isReadmeFile
      case mReadme of
        Left err ->
          errNotFound "Readme not found" [MText err]
        Right (tarfile, etag, offset, filename) -> do
          contents <- either (\err -> errInternalError [MText err])
                             (return . snd)
                  =<< liftIO (loadTarEntry tarfile offset)
          cacheControl [Public, maxAgeDays 30] etag
          return $ toResponse $ Resource.XHtml $
            let title = "Readme for " ++ display (packageId pkg) in
            hackagePage title
              [ XHtml.h2 << title
              , XHtml.thediv ! [XHtml.theclass "embedded-author-content"]
                            << if supposedToBeMarkdown filename
                                 then renderMarkdown filename contents
                                 else XHtml.thediv ! [XHtml.theclass "preformatted"]
                                                  << unpackUtf8 contents
              ]

    -- return: not-found error or tarball
    serveContents :: DynamicPath -> ServerPartE Response
    serveContents dpath = do
      pkg      <- packageInPath dpath >>= lookupPackageId
      mTarball <- liftIO $ packageTarball pkg
      case mTarball of
        Left err ->
          errNotFound "Could not serve package contents" [MText err]
        Right (fp, etag, index) ->
          serveTarball (display (packageId pkg) ++ " source tarball")
                       [] (display (packageId pkg)) fp index
                       [Public, maxAgeDays 30] etag

unpackUtf8 :: BS.ByteString -> String
unpackUtf8 = T.unpack
           . T.decodeUtf8With T.lenientDecode
           . BS.toStrict

-- TODO: this helper is defined in at least two other places; consolidate
-- | URL describing a package.
packageURL :: PackageIdentifier -> XHtml.URL
packageURL pkgId = "/package" </> display pkgId
