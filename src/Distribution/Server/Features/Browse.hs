{-# LANGUAGE BlockArguments, NamedFieldPuns #-}
module Distribution.Server.Features.Browse (initBrowseFeature, PaginationConfig(..), StartIndex(..), NumElems(..), paginate) where

import Control.Monad.Except (ExceptT, liftIO, throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as S
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.FilePath ((</>))

import Data.Aeson (Value(Array), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V
import Text.XHtml.Strict (renderHtmlFragment)

import Distribution.Server.Features.Browse.ApplyFilter (applyFilter)
import Distribution.Server.Features.Browse.Options (BrowseOptions(..), IsSearch(..), Column(DefaultColumn), Direction(Ascending), columnToTemplateName, directionToTemplateName, parseSearchQuery)
import Distribution.Server.Features.Core (CoreFeature(CoreFeature), CoreResource, coreResource, corePackageNameUri)
import Distribution.Server.Features.Distro (DistroFeature)
import Distribution.Server.Features.Html.HtmlUtilities (HtmlUtilities(HtmlUtilities), htmlUtilities, makeRow)
import Distribution.Server.Features.PackageCandidates (PackageCandidatesFeature)
import Distribution.Server.Features.PackageList (ListFeature(ListFeature), PackageItem(..), getAllLists, makeItemList)
import Distribution.Server.Features.Search (SearchFeature(SearchFeature), searchPackages)
import Distribution.Server.Features.Tags (Tag(..), TagsFeature(TagsFeature), TagsResource, tagUri, tagsResource)
import Distribution.Server.Features.Users (UserFeature(UserFeature), UserResource, userResource, userPageUri)
import Distribution.Server.Framework.Error (ErrorResponse(ErrorResponse))
import Distribution.Server.Framework.Feature (HackageFeature(..), emptyHackageFeature)
import Distribution.Server.Framework.RequestContentTypes (expectAesonContent)
import Distribution.Server.Framework.Resource (Resource(..), resourceAt)
import Distribution.Server.Framework.ServerEnv (ServerEnv(..))
import Distribution.Server.Framework.Templating
import Distribution.Server.Users.Types (UserName)
import Distribution.Simple.Utils (toUTF8LBS)
import Distribution.Text (display)

import Happstack.Server.Monads (ServerPartT)
import Happstack.Server.Response (ToMessage(toResponse))
import Happstack.Server.Types (Method(GET, POST), Response)

type BrowseFeature =
    CoreFeature
    -> UserFeature
    -> TagsFeature
    -> ListFeature
    -> SearchFeature
    -> DistroFeature
    -> PackageCandidatesFeature
    -> IO HackageFeature

data ResponseFormatWithMethod
  = JSON_POST
  | HTML_POST
  | HTML_GET

initBrowseFeature :: ServerEnv -> IO BrowseFeature
initBrowseFeature ServerEnv{serverTemplatesDir, serverTemplatesMode} = do
  templates <-
    loadTemplates serverTemplatesMode
      [ serverTemplatesDir, serverTemplatesDir </> "Html" ]
      [ "table-interface.html"
      , "noscript-search-form.html"
      , "noscript-next-page-form.html"
      ]
  pure \coreFeature userFeature tagsFeature listFeature searchFeature distroFeature packageCandidatesFeature -> do
    let
      html = htmlUtilities coreFeature packageCandidatesFeature tagsFeature userFeature
      renderer :: ResponseFormatWithMethod -> ServerPartT (ExceptT ErrorResponse IO) Response
      renderer = renderResultPage coreFeature userFeature tagsFeature listFeature searchFeature distroFeature html templates
    pure $
      (emptyHackageFeature "search/browse backend")
        { featureResources =
          [ (resourceAt "/packages/search")
            { resourceDesc =
              [ (POST, "Browse and search for users with JavaScript enabled")
              ]
            , resourcePost =
              [ ("json"
                , \_ -> renderer JSON_POST
                )
              ]
            }
          , (resourceAt "/packages/noscript-search")
            { resourceDesc =
              [ (POST, "Browse and search for users without JavaScript enabled")
              , (GET,  "Browse and search for users without JavaScript enabled")
              ]
            , resourcePost =
              [ ("html"
                , \_ -> renderer HTML_POST
                )
              ]
            , resourceGet =
              [ ("html"
                , \_ -> renderer HTML_GET
                )
              ]
            }
          ]
        , featureState = []
        }

data PaginationConfig = PaginationConfig
  { totalNumberOfElements :: Int
  , pageNumber :: Int
  }
  deriving Show

newtype NumElems = NumElems Int
  deriving (Eq, Show)
newtype StartIndex = StartIndex Int
  deriving (Eq, Show)

pageSize :: Int
pageSize = 50 -- make sure it is kept in sync with frontend

paginate :: PaginationConfig -> Maybe (StartIndex, NumElems)
paginate PaginationConfig{totalNumberOfElements, pageNumber} = do
 startIndex <-
   if totalNumberOfElements <= pageNumber * pageSize
      then
        -- We don't want to claim that the page 0 is ever out of bounds,
        -- since it is normal to request page 0 of a listing with 0 results.
        if pageNumber == 0
           then Just 0
           else Nothing
      else Just $ pageNumber * pageSize
 Just
   ( StartIndex startIndex
   , NumElems $
       if totalNumberOfElements < startIndex + pageSize
         then totalNumberOfElements - startIndex
         else pageSize
   )

packageIndexInfoToValue :: CoreResource -> TagsResource -> UserResource -> PackageItem -> Value
packageIndexInfoToValue
  coreResource tagsResource userResource
  PackageItem{itemName, itemDownloads, itemVotes,
    itemDesc, itemTags, itemLastUpload, itemMaintainer} =
  object
    [ Key.fromString "name" .= renderPackage itemName
    , Key.fromString "downloads" .= itemDownloads
    , Key.fromString "votes" .= itemVotes
    , Key.fromString "description" .= itemDesc
    , Key.fromString "tags" .= map renderTag (S.toAscList itemTags)
    , Key.fromString "lastUpload" .= iso8601Show itemLastUpload
    , Key.fromString "maintainers" .= map renderUser itemMaintainer
    ]
  where
  renderTag :: Tag -> Value
  renderTag tag =
    object
      [ Key.fromString "uri" .= tagUri tagsResource "" tag
      , Key.fromString "display" .= display tag
      ]
  renderUser :: UserName -> Value
  renderUser user =
    object
      [ Key.fromString "uri" .= userPageUri userResource "" user
      , Key.fromString "display" .= display user
      ]
  renderPackage pkg =
    object
      [ Key.fromString "uri" .= corePackageNameUri coreResource "" pkg
      , Key.fromString "display" .= display pkg
      ]

renderResultPage :: CoreFeature -> UserFeature -> TagsFeature -> ListFeature -> SearchFeature -> DistroFeature -> HtmlUtilities -> Templates -> ResponseFormatWithMethod -> ServerPartT (ExceptT ErrorResponse IO) Response
renderResultPage CoreFeature{coreResource} UserFeature{userResource} TagsFeature{tagsResource} ListFeature{getAllLists, makeItemList} SearchFeature{searchPackages} distroFeature HtmlUtilities{makeRow} templates format = do
  browseOptions <-
    case format of
      HTML_GET -> pure (BrowseOptions 0 DefaultColumn Ascending mempty)
      _        -> expectAesonContent
  (filters, terms) <- parseSearchQuery (boSearchQuery browseOptions)
  (isSearch, pkgDetails) <-
    liftIO $ case terms of
      [] -> do
        allItemsMap <- getAllLists
        pure (IsNotSearch, Map.elems allItemsMap)
      nonEmptyTerms -> do
        packageNames <- searchPackages nonEmptyTerms
        items <- makeItemList packageNames
        pure (IsSearch, items)
  now <- liftIO getCurrentTime
  listOfPkgs <- liftIO $ applyFilter now isSearch distroFeature (boSortColumn browseOptions) (boSortDirection browseOptions) filters pkgDetails
  let page = fromIntegral (boPage browseOptions)
      config =
        PaginationConfig
          { totalNumberOfElements = length listOfPkgs
          , pageNumber = page
          }
  (StartIndex startIndex, NumElems numElems) <-
    lift $ maybe
      (throwError . badRespFromString $ "Invalid page number: " ++ show config)
      pure
      (paginate config)
  let pageContents = V.slice startIndex numElems (V.fromList listOfPkgs)
  case format of
    JSON_POST ->
      pure . toResponse $
        object
          [ Key.fromString "pageContents" .= Array (fmap (packageIndexInfoToValue coreResource tagsResource userResource) pageContents)
          , Key.fromString "numberOfResults" .= toJSON (length listOfPkgs)
          ]
    _ -> do
      template <- getTemplate templates "table-interface.html"
      noscriptForm <- getTemplate templates "noscript-search-form.html"
      nextPageForm <- getTemplate templates "noscript-next-page-form.html"
      let
        rowList = fmap makeRow pageContents
        tabledata = toUTF8LBS . renderHtmlFragment $ V.toList rowList
        noscriptFormRendered =
          renderTemplate $ noscriptForm
            [ directionToTemplateName (boSortDirection browseOptions) $= True
            , columnToTemplateName (boSortColumn browseOptions) $= True
            , "pageNumber" $= show page
            , "searchQuery" $= boSearchQuery browseOptions
            ]
        minIdx = page * pageSize + 1;
        maxIdx' = (page + 1) * pageSize
        total = totalNumberOfElements config
        hasNext = isJust (paginate config { pageNumber = page + 1})
        maxIdx = min maxIdx' total
        footer =
          renderTemplate $
            nextPageForm
              [ "page" $= page + 1
              , "searchQuery" $= boSearchQuery browseOptions
              , "sortDirection" $= directionToTemplateName (boSortDirection browseOptions)
              , "sortColumn" $= columnToTemplateName (boSortColumn browseOptions)
              , "maxIdx" $= maxIdx
              , "minIdx" $= minIdx
              , "total" $= total
              , "hasNext" $= hasNext
              ]
      pure . toResponse $ template
        [ templateUnescaped "tabledata" tabledata
        , templateUnescaped "content" noscriptFormRendered
        , if total == 0
             then "footer" $= "No results found."
             else templateUnescaped "footer" footer
        , "heading" $= "Search without JavaScript"
        , "noDatatable" $= True
        ]

badRespFromString :: String -> ErrorResponse
badRespFromString err = ErrorResponse 400 [] err []
