{-# LANGUAGE BlockArguments, NamedFieldPuns, TupleSections #-}
module Distribution.Server.Features.Browse (initBrowseFeature, PaginationConfig(..), StartIndex(..), NumElems(..), paginate) where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT, liftIO, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except)
import Data.ByteString.Lazy (ByteString)
import Data.Time (getCurrentTime)

import Data.Aeson (Value(Array), eitherDecode, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V

import Distribution.Server.Features.Browse.ApplyFilter (applyFilter)
import Distribution.Server.Features.Browse.Options (BrowseOptions(..), IsSearch(..))
import Distribution.Server.Features.Core (CoreFeature(CoreFeature), queryGetPackageIndex, coreResource)
import Distribution.Server.Features.Distro (DistroFeature)
import Distribution.Server.Features.PackageList (ListFeature(ListFeature), makeItemList)
import Distribution.Server.Features.Search (SearchFeature(SearchFeature), searchPackages)
import Distribution.Server.Features.Tags (TagsFeature(TagsFeature), tagsResource)
import Distribution.Server.Features.Users (UserFeature(UserFeature), userResource)
import Distribution.Server.Framework.Error (ErrorResponse(ErrorResponse))
import Distribution.Server.Framework.Feature (HackageFeature(..), emptyHackageFeature)
import Distribution.Server.Framework.Resource (Resource(..), resourceAt)
import Distribution.Server.Framework.ServerEnv (ServerEnv(..))
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Happstack.Server.Monads (ServerPartT)
import Happstack.Server.Response (ToMessage(toResponse))
import Happstack.Server.RqData (lookBS)
import Happstack.Server.Types (Method(POST), Response)

type BrowseFeature =
    CoreFeature
    -> UserFeature
    -> TagsFeature
    -> ListFeature
    -> SearchFeature
    -> DistroFeature
    -> IO HackageFeature

initBrowseFeature :: ServerEnv -> IO BrowseFeature
initBrowseFeature _env =
  pure \coreFeature userFeature tagsFeature listFeature searchFeature distroFeature ->
    pure $
      (emptyHackageFeature "json")
        { featureResources =
          [ (resourceAt "/packages/search")
            { resourceDesc =
              [ (POST, "Browse and search using a BrowseOptions structure in multipart/form-data encoding")
              ]
            , resourcePost =
              [ ("json"
                , \_ -> getNewPkgList coreFeature userFeature tagsFeature listFeature searchFeature distroFeature
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

paginate :: PaginationConfig -> Maybe (StartIndex, NumElems)
paginate PaginationConfig{totalNumberOfElements, pageNumber} = do
 let pageSize = 50 -- make sure it is kept in sync with frontend
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

getNewPkgList :: CoreFeature -> UserFeature -> TagsFeature -> ListFeature -> SearchFeature -> DistroFeature -> ServerPartT (ExceptT ErrorResponse IO) Response
getNewPkgList CoreFeature{queryGetPackageIndex, coreResource} UserFeature{userResource} TagsFeature{tagsResource} ListFeature{makeItemList} SearchFeature{searchPackages} distroFeature = do
  browseOptionsBS <- lookBS "browseOptions"
  browseOptions <- lift (parseBrowseOptions browseOptionsBS)
  (isSearch, packageNames) <-
    case boSearchTerms browseOptions of
      [] ->    (IsNotSearch,) <$> PackageIndex.allPackageNames <$> queryGetPackageIndex
      terms -> (IsSearch,)    <$> liftIO (searchPackages terms)
  pkgDetails <- liftIO (makeItemList packageNames)
  now <- liftIO getCurrentTime
  listOfPkgs <- liftIO $ applyFilter now isSearch coreResource userResource tagsResource distroFeature browseOptions pkgDetails
  let config =
        PaginationConfig
          { totalNumberOfElements = length listOfPkgs
          , pageNumber = fromIntegral $ boPage browseOptions
          }
  (StartIndex startIndex, NumElems numElems) <-
    lift $ maybe
      (throwError . badRespFromString $ "Invalid page number: " ++ show config)
      pure
      (paginate config)
  let pageContents = V.slice startIndex numElems (V.fromList listOfPkgs)
  pure . toResponse $
    object
      [ Key.fromString "pageContents" .= Array pageContents
      , Key.fromString "numberOfResults" .= toJSON (length listOfPkgs)
      ]

parseBrowseOptions :: ByteString -> ExceptT ErrorResponse IO BrowseOptions
parseBrowseOptions browseOptionsBS = except eiDecoded
  where
  eiDecoded :: Either ErrorResponse BrowseOptions
  eiDecoded = left badRespFromString (eitherDecode browseOptionsBS)

badRespFromString :: String -> ErrorResponse
badRespFromString err = ErrorResponse 400 [] err []
