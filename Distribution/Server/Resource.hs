{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}

module Distribution.Server.Resource where

import Happstack.Server
import qualified Data.ByteString.Char8 as BS
import qualified Network.URI as URI
import Data.Time.Clock (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (mplus, msum, when)
import Data.Maybe (maybeToList)
import Data.List (intercalate)
import Data.Monoid (mappend)

import Happstack.State (QueryEvent, UpdateEvent, query, update)


data Resource = Resource {
    resourceLocation :: BranchPath,
    resourceMethods :: Map Method (DynamicPath -> MediaType -> ServerPart (Response, Bool)),
    resourceInvalidates :: [BranchPath]
}

type DynamicPath = Map String String
type MediaType = Maybe BS.ByteString
-- until Happstack 0.6.*
deriving instance Ord Method
data BranchComponent = StaticBranch String | DynamicBranch String
type BranchPath = [BranchComponent]
type ClientCache = Map URI.URI UTCTime

data ServerTree = ServerTree {
    nodeResource :: Maybe (Resource),
    nodeResponse :: Maybe (ServerPart Response),
    dirForest :: DirForest,
    resourceForest :: ResourceForest
}


-- Shared user group abstraction should probably go elsewhere, but the
-- group-resource-factory is in this module for now
type UserList = Set Int
class (QueryEvent a UserList, UpdateEvent a ()) => UserGroup a where
    -- call: query queryUserList
    queryUserList :: a
    -- call: update (\group -> ...)
    updateUserList :: (UserList -> UserList) -> a

{-
-- Helper functions
removeUser :: UserGroup a => UserId -> a
removeUser user = updateUserList (Set.delete user)

addUser :: UserGroup a => UserId -> a
addUser user = addUser (Set.insert user)
-}

-- TODO: implement
makeGroupResources :: UserGroup a => a -> BranchPath -> [Resource]
makeGroupResources group prefix = []

type DirForest = Map String ServerTree
type ResourceForest = Map String ServerTree

serverTreeEmpty :: ServerTree
serverTreeEmpty = ServerTree Nothing Nothing Map.empty Map.empty

spiffyResources :: ServerTree -> ServerTree
spiffyResources (ServerTree mresource response sdirs resources) = ServerTree (fmap spiffyResource mresource) response
                                                                (Map.map spiffyResources sdirs) (Map.map spiffyResources resources)
    where spiffyResource :: Resource -> Resource
          spiffyResource resource = resource { resourceMethods =
                           addMissingWithMap (Just . makeOptions . Map.keys) OPTIONS
                         . addMissingWithMap (fmap makeHead . Map.lookup GET) HEAD
                         $ resourceMethods resource }
          addMissingWithMap :: Ord k => (Map k a -> Maybe a) -> k -> Map k a -> Map k a
          addMissingWithMap f key mmap = Map.alter (`mplus` f mmap) key mmap
          makeHead responseForGET = \dpath mediaType -> do
              (_, _) <- responseForGET dpath mediaType
              noBody
          -- one downside of the DynamicBranch String approach (as opposed to a more typeful
          -- generic system) is that, out of multiple resources served from the same
          -- ServerTree node, only the first one's options will be answered
          makeOptions methods = \_ _ -> do
              setHeaderM "Allow" (intercalate ", " . map show $ methods)
              noBody
          noBody = return $ (toResponse "", False)

renderServerTree :: ServerTree -> DynamicPath -> ServerPart Response
renderServerTree (ServerTree resource response sdirs resources) dpath = msum $ maybeToList response ++ maybeToList (fmap renderResource resource) ++ renderDirs ++ [renderResources]
  where
    renderDirs = map (\(name, tree) -> dir name $ renderServerTree tree dpath) (Map.toList sdirs)
    renderResources = path $ \pname -> msum $ map (\(name, tree) -> renderServerTree tree (Map.insert name pname dpath)) $ Map.toList resources
    renderResource (Resource _ resourceMap resourceDepends) = do
        -- use map instead of msum, slightly more efficient (?)
        met <- fmap rqMethod askRq
        require (return $ Map.lookup met resourceMap) $ \task -> methodSP met $ do
            mediaType <- getHeaderM "accept"
            (resResponse, isModified) <- task dpath mediaType
            when isModified $ invalidate resourceDepends
            return resResponse
    invalidate _ = return ()

--withPackage :: DynamicPath -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response

reinsert :: String -> ServerTree -> Map String ServerTree -> Map String ServerTree
reinsert key newTree pairMap = Map.insertWith combine key newTree pairMap

-- combine new old
combine :: ServerTree -> ServerTree -> ServerTree
combine (ServerTree resource response sdirs resources) (ServerTree resource' response' sdirs' resources') =
    -- replace old resource with new resource, combine old and new responses
    ServerTree (mplus resource resource') (mappend response response')
               (Map.foldWithKey reinsert sdirs' sdirs) -- this combines them
               (Map.foldWithKey reinsert resources resources')

addResource :: Resource -> ServerTree -> ServerTree
addResource resource tree = snd $ treeFold (resourceLocation resource) (ServerTree (Just resource) Nothing Map.empty Map.empty) tree

addResponse :: BranchPath -> ServerPart Response -> ServerTree -> ServerTree
addResponse trunk response tree = snd $ treeFold trunk (ServerTree Nothing (Just response) Map.empty Map.empty) tree

--this function takes a list whose head is the resource and traverses leftwards in the URI
--this is due to the original design of specifying URI branches: if the resources are
--themselves encoded in the branch, then subresources should share the same parent
--resource, sharing list tails. I may switch the convention sometime, since this one
--is nearly too 'clever' for me to even debug, though it works.
treeFold :: BranchPath -> ServerTree -> ServerTree -> (ServerTree, ServerTree)
treeFold [] newChild topLevel = (topLevel, combine newChild topLevel)
treeFold (StaticBranch sdir:otherTree) newChild topLevel = (Map.findWithDefault serverTreeEmpty sdir (dirForest tree), newTree)
    where (tree, newTree) = treeFold otherTree (tree { dirForest = reinsert sdir newChild (dirForest tree) }) topLevel
treeFold (DynamicBranch sdir:otherTree) newChild topLevel = (Map.findWithDefault serverTreeEmpty sdir (resourceForest tree), newTree)
    where (tree, newTree) = treeFold otherTree (tree { resourceForest = reinsert sdir newChild (resourceForest tree) }) topLevel


