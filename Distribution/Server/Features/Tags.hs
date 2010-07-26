module Distribution.Server.Features.Tags (
    TagsFeature(..),
    initTagsFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Error
import Distribution.Server.Packages.Tag

import Distribution.Text
import Distribution.Package

import Data.Function (fix)
import Data.List (intercalate)
import Control.Monad (mzero)

import Happstack.State
import Happstack.Server

data TagsFeature = TagsFeature {
    tagsResource :: TagsResource
}

-- TODO: registry for calculated tags
data TagsResource = TagsResource {
    tagsListing :: Resource,
    tagListing :: Resource,
    packageTagsListing :: Resource,

    tagUri :: String -> Tag -> String,
    tagsUri :: String -> String,
    packageTagsUri :: String -> PackageName -> String
    -- /packages/tags/.:format
    -- /packages/tag/:tag.:format
    -- /package/:package/tags.:format
    -- /package/:package/tags/edit (HTML)
}

instance HackageFeature TagsFeature where
    getFeature tags = HackageModule
      { featureName = "tags"
      , resources   = map ($tagsResource tags) [tagsListing, tagListing, packageTagsListing]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initTagsFeature :: Config -> CoreFeature -> IO TagsFeature
initTagsFeature _ _ = do
    return TagsFeature
      { tagsResource = fix $ \r -> TagsResource
          { tagsListing = (resourceAt "/packages/tags/.:format") { resourceGet = [("txt", \_ -> textAllTags)] }
          , tagListing = (resourceAt "/packages/tag/:tag.:format") { resourceGet = [("txt", textATag)] }
          , packageTagsListing = (resourceAt "/package/:package/tags.:format") { resourceGet = [("txt", textPackageTags)], resourcePut = [("txt", textPutTags)] }

          , tagUri = \format tag -> renderResource (tagListing r) [display tag, format]
          , tagsUri = \format -> renderResource (tagsListing r) [format]
          , packageTagsUri = \format pkgname -> renderResource (packageTagsListing r) [display pkgname, format]
            -- for more fine-tuned tag manipulation, could also define:
            -- * DELETE /package/:package/tag/:tag (remove single tag)
            -- * POST /package/:package\/tags (add single tag)
          }
      }
  where
    textPutTags dpath = textResponse $ withPackageName dpath $ \pkgname ->
                        responseWith (putTags pkgname) $ \_ ->
        returnOk . toResponse $ "Set the tags for " ++ display pkgname
    textAllTags = do
        tags <- query GetTagList  
        return . toResponse $ intercalate ", " $ map (display . fst) tags
    textATag dpath = withTagPath dpath $ \_ pkgnames -> do
        return . toResponse $ intercalate ", " $ map display pkgnames
    textPackageTags dpath = textResponse $ withPackageAllPath dpath $ \pkgname _ -> do
        tags <- query $ TagsForPackage pkgname
        returnOk . toResponse $ display (TagList tags)

withTagPath :: DynamicPath -> (Tag -> [PackageName] -> ServerPart a) -> ServerPart a
withTagPath dpath func = case simpleParse =<< lookup "tag" dpath of
    Nothing -> mzero
    Just tag -> do
        pkgs <- query $ PackagesForTag tag
        func tag pkgs

putTags :: PackageName -> MServerPart ()
putTags pkgname = withPackageAll pkgname $ \_ -> do
    -- let anyone edit tags for the moment. otherwise, we can do:
    -- users <- query GetUserDb; withHackageAuth users Nothing Nothing $ \_ _ -> do
    mtags <- getDataFn $ look "tags"
    case simpleParse =<< mtags of
        Just (TagList tags) -> do
            update $ SetPackageTags pkgname tags
            returnOk ()
        Nothing -> returnError 400 "Tags not recognized" [MText "Couldn't parse your tag list. It should be comma separated with any number of alphanumerical tags. Tags can also also have -+#*."]

