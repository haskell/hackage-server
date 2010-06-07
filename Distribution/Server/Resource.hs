{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts #-}

module Distribution.Server.Resource (
    Config(..),
    Resource (..),
    DynamicPath,
    BranchComponent(..),
    BranchPath,
    ServerResponse,
    ServerTree(..),
    makeGroupResources,
    serverTreeEmpty,
    trunkAt,
    resourceAt,
    defaultResource,
    serveResource,
    renderServerTree,
    addResponse
  ) where

import Happstack.Server
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Users.Group (UserGroup(..), UserList(..))

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (msum)
import Data.Maybe (maybeToList)
import Data.List (intercalate, find)
import Data.Monoid (mappend)
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Network.URI as URI
import qualified Distribution.Server.Cache as Cache

import Happstack.State (QueryEvent, UpdateEvent, query, update)


data Config = Config {
  serverStore      :: BlobStorage,
  serverStaticDir  :: FilePath,
  serverURI        :: URI.URIAuth,
  serverCache      :: Cache.Cache
}

data Resource = Resource {
    resourceLocation :: BranchPath,
    resourceGet    :: Maybe ServerResponse,
    resourcePut    :: Maybe ServerResponse,
    resourcePost   :: Maybe ServerResponse,
    resourceDelete :: Maybe ServerResponse
}

resourceAt :: String -> Resource
resourceAt = defaultResource . trunkAt

defaultResource :: BranchPath -> Resource
defaultResource bpath = Resource bpath Nothing Nothing Nothing Nothing

-- to be used with functions like withPackage :: DynamicPath -> (PackagesState -> PkgInfo -> [PkgInfo] -> ServerPart Response) -> ServerPart Response

type DynamicPath = [(String, String)]
-- until Happstack 0.6.*
deriving instance Ord Method
data BranchComponent = StaticBranch String | DynamicBranch String deriving (Show, Eq, Ord)
type BranchPath = [BranchComponent]

type ServerResponse = Config -> DynamicPath -> ServerPart Response

data ServerTree = ServerTree {
    nodeResponse :: Maybe ServerResponse,
    nodeForest :: Map BranchComponent ServerTree
}

-- Shared user group abstraction should probably go elsewhere, but the
-- group-resource-factory is in this module for now

--data UserGroup a = UserGroup {
--    groupName :: String,
--    queryUserList :: a,
--    updateUserList :: (UserList -> UserList) -> a
--}
-- TODO: implement
makeGroupResources :: (QueryEvent a (Maybe UserList), UpdateEvent b (), UpdateEvent c ()) => BranchPath -> (DynamicPath -> Maybe (UserGroup a b c)) -> [Resource]
makeGroupResources branch group = [] {-[viewList, modifyList]
  where
    viewList = defaultResource branch { resourceGet = getList, resourcePost = postUser }
    getList dpath = do
        userList <- liftIO (query $ queryList dpath)
        return $ toResponse ()
    modifyList = defaultResource (DynamicBranch "user":branch) { resourceGet = getUser, resourceDelete = deleteUser }
    putUser dpath = withUser dpath $ \userId ->
        liftIO (update $ updateList dpath (Map.insert userId))
    deleteUser dpath = withUser dpath $ \userId ->
        liftIO (update $ updateList dpath (Map.delete userId))
    -- require (return - group dpath)
--(UserGroup name queryList updateList)-}

serverTreeEmpty :: ServerTree
serverTreeEmpty = ServerTree Nothing Map.empty

-- "/package/:package/doc/:doctree/"
trunkAt :: String -> BranchPath
trunkAt arg = fromTrunkJust . find (\(_, str) -> null str || str == "/") . Parse.readP_to_S parser $ arg
            -- Parser gives ReadS BranchPath = [BranchPath, String)]
  where
    fromTrunkJust (Just a) = reverse (fst a)
    -- bottom, not ideal, but this is one of the risks of using declarative parsers
    -- alternatively return a dummy path, like [StaticBranch "error"]
    fromTrunkJust Nothing  = error $ "Distribution.Server.Resource.trunkLiteral: Could not parse trunk literal " ++ show arg
    parser :: Parse.ReadP BranchPath -- = [BranchComponent]
    parser = Parse.many $ do
        Parse.char '/'
        fmap DynamicBranch (Parse.char ':' >> Parse.munch1 (/='/')) Parse.<++ (fmap StaticBranch (Parse.munch1 (/='/')))

serveResource :: Resource -> (BranchPath, ServerResponse)
serveResource (Resource trunk rget rput rpost rdelete) = (,) trunk $ \config dpath -> msum $
    map (\func -> func config dpath) ([ (methodSP met .) . res | (Just res, met) <- zip methods methodsList]
                   ++ return (makeOptions $ concat [ met | (Just _, met) <- zip methods methodsList]))
  where
    methods = [rget, rput, rpost, rdelete]
    methodsList = [[GET, HEAD], [PUT], [POST], [DELETE]]
    -- apparently Happstack can do HEAD on its own! plus we need the Content-Length
    makeHead :: ServerResponse -> ServerResponse
    makeHead responseGET = \config dpath -> do
      _ <- responseGET config dpath
      noBody
    -- one downside of the DynamicBranch String approach (as opposed to a more typeful
    -- generic system) is that, out of multiple resources served from the same
    -- ServerTree node, only the first one's options will be answered
    makeOptions :: [Method] -> ServerResponse
    makeOptions methodList = \_ _ -> do
      setHeaderM "Allow" (intercalate ", " . map show $ methodList)
      noBody
    noBody = return $ toResponse ()

renderServerTree :: Config -> DynamicPath -> ServerTree -> ServerPart Response
renderServerTree config dpath (ServerTree func forest) = msum $ maybeToList (fmap (\fun -> fun config dpath) func) ++ map (uncurry renderBranch) (Map.toList forest)
  where
    renderBranch :: BranchComponent -> ServerTree -> ServerPart Response
    renderBranch (StaticBranch  sdir) tree = dir sdir $ renderServerTree config dpath tree
    renderBranch (DynamicBranch sdir) tree = path $ \pname -> renderServerTree config ((sdir, pname):dpath) tree

reinsert :: BranchComponent -> ServerTree -> Map BranchComponent ServerTree -> Map BranchComponent ServerTree
-- combine will only be called if branchMap already contains the key
reinsert key newTree branchMap = Map.insertWith combine key newTree branchMap


-- combine new old
combine :: ServerTree -> ServerTree -> ServerTree
combine (ServerTree response forest) (ServerTree response' forest') =
    -- replace old resource with new resource, combine old and new responses
    -- reinsert will only be called if forest' is non-empty
    ServerTree (mappend response response') (Map.foldWithKey reinsert forest forest')

addResponse :: BranchPath -> ServerResponse -> ServerTree -> ServerTree
addResponse trunk response tree = snd $ treeFold trunk (ServerTree (Just response) Map.empty) tree

--this function takes a list whose head is the resource and traverses leftwards in the URI
--this is due to the original design of specifying URI branches: if the resources are
--themselves encoded in the branch, then subresources should share the same parent
--resource, sharing list tails. I may switch the convention sometime, since this one
--is nearly too 'clever' for me to even debug, though it works.
treeFold :: BranchPath -> ServerTree -> ServerTree -> (ServerTree, ServerTree)
treeFold [] newChild topLevel = (topLevel, combine newChild topLevel)
treeFold (sdir:otherTree) newChild topLevel = (Map.findWithDefault serverTreeEmpty sdir (nodeForest tree), newTree)
    where (tree, newTree) = treeFold otherTree (tree { nodeForest = reinsert sdir newChild (nodeForest tree) }) topLevel
--treeFold (DynamicBranch sdir:otherTree) newChild topLevel = (Map.findWithDefault serverTreeEmpty sdir (resourceForest tree), newTree)
--    where (tree, newTree) = treeFold otherTree (tree { resourceForest = reinsert sdir newChild (resourceForest tree) }) topLevel


