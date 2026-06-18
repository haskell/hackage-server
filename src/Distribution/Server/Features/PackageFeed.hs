{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Distribution.Server.Features.PackageFeed where

import Distribution.Server.Features.Core
import Distribution.Server.Features.TarIndexCache
import Distribution.Server.Features.Users
import Distribution.Server.Framework
import Distribution.Server.Packages.ChangeLog
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Utils
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Util.Parse (unpackUTF8)
import Distribution.Server.Util.ServeTarball (loadTarEntry)
import Distribution.Server.Util.Markdown (renderMarkdown, supposedToBeMarkdown)
import Distribution.Server.Pages.Package () -- for ShortText html instance, for now.

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Utils.ShortText (fromShortText)

import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format
import Network.URI( URI(..), uriToString )
import qualified Text.RSS as RSS
import Text.RSS ( RSS(RSS) )
import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Strict ((<<), (+++), (!))

newtype PackageFeedFeature = PackageFeedFeature {
  packageFeedFeatureInterface :: HackageFeature
}

instance IsHackageFeature PackageFeedFeature where
  getFeatureInterface = packageFeedFeatureInterface

initPackageFeedFeature :: ServerEnv
                       -> IO ( CoreFeature
                          -> UserFeature
                          -> TarIndexCacheFeature
                          -> IO PackageFeedFeature)
initPackageFeedFeature env =
  return $ \core users tars ->
    return $ packageFeedFeature env core users tars

packageFeedFeature :: ServerEnv
                   -> CoreFeature
                   -> UserFeature
                   -> TarIndexCacheFeature
                   -> PackageFeedFeature
packageFeedFeature ServerEnv{..}
                   CoreFeature{..}
                   UserFeature{..}
                   TarIndexCacheFeature{..}
  = PackageFeedFeature{..}
  where

    CoreResource{..} = coreResource

    packageFeedFeatureInterface = (emptyHackageFeature "package feed") {
      featureResources = [ packageFeedResource ]
      , featureState = []
      , featureDesc = "Provides RSS feed for individual packages"
      , featureCaches = []
      , featurePostInit = pure ()
    }

    packageFeedResource :: Resource
    packageFeedResource = (resourceAt "/package/:package.rss") {
      resourceDesc = [(GET, "Package feed")]
    , resourceGet = [("rss", packageFeed)]
    }

    packageFeed :: DynamicPath -> ServerPartE Response
    packageFeed dpath = do
      users <- queryGetUserDb
      now <- liftIO getCurrentTime
      pkgname <- packageInPath dpath
      pkgs <- sortOn (Down . pkgOriginalUploadTime) <$> lookupPackageName pkgname
      pkgs' <- liftIO $ forM pkgs changelog
      return $ toResponse $ renderPackageFeed users serverBaseURI now pkgname pkgs'

    changelog :: PkgInfo -> IO (PkgInfo, XHtml.Html)
    changelog pkg = findToplevelFile pkg isChangeLogFile >>= \case
        Left _ -> return (pkg, XHtml.primHtml "(No changelog found.)")
        Right (tarfile, _, offset, filename) ->
          loadTarEntry tarfile offset >>= \case
            Left _ -> return (pkg, XHtml.primHtml "(No changelog found.)")
            Right (_, content) ->
              if supposedToBeMarkdown filename
                then return (pkg, renderMarkdown filename content)
                else return (pkg, XHtml.pre << unpackUTF8 content)

renderPackageFeed :: Users -> URI -> UTCTime -> PackageName -> [(PkgInfo, XHtml.Html)] -> RSS
renderPackageFeed users hostURI now name pkgs = RSS title uri desc (channel updated) items
  where title = unPackageName name ++ " – new releases on Hackage"
        desc = "New releases of package '" ++ unPackageName name ++ "' on Hackage."
        items = feedItems users uri <$> pkgs
        uri = hostURI { uriPath = "/package/" ++ display name }
        updated = maybe now (pkgOriginalUploadTime . fst) (listToMaybe pkgs)

channel :: UTCTime -> [RSS.ChannelElem]
channel updated =
  [ RSS.Language "en"
  , RSS.ManagingEditor "admin@hackage.haskell.org"
  , RSS.WebMaster "admin@hackage.haskell.org"
  , RSS.ChannelPubDate updated
  , RSS.LastBuildDate updated
  , RSS.Generator "rss-feed"
  ]

feedItems :: Users -> URI -> (PkgInfo, XHtml.Html) -> [RSS.ItemElem]
feedItems users hostURI (pkgInfo, chlog) =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id uri "")
  , RSS.PubDate time
  , RSS.Description (XHtml.showHtmlFragment desc)
  , RSS.Author uploader
  ]
  where title = pkgName ++ " (" ++ fromShortText (synopsis pd) ++ ")"
        uri = hostURI { uriPath = "/package/" ++ pkgName }
        desc = XHtml.dlist << XHtml.concatHtml
          [ d "Homepage"   $ XHtml.anchor ! [XHtml.href (fromShortText $ homepage pd)] << homepage pd
          , d "Author"     $ author pd
          , d "Uploaded"   $ "by " ++ uploader ++ " at " ++ timestr
          , d "Maintainer" $ maintainer pd
          ] +++ XHtml.hr +++ chlog
        pkgName = display (pkgInfoId pkgInfo)
        UploadInfo time uploaderId = pkgOriginalUploadInfo pkgInfo
        timestr = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
        uploader = display $ Users.userIdToName users uploaderId
        pd = packageDescription $ pkgDesc $ pkgLatestRevision pkgInfo
        d dt dd = XHtml.dterm (XHtml.toHtml dt) +++ XHtml.ddef (XHtml.toHtml dd)
