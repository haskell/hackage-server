{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, FlexibleContexts, FlexibleInstances #-}

module Distribution.Server.Resource (
    Resource (..),
    BranchComponent(..),
    BranchPath,
    Content,
    ServerTree(..),
    URIGen,
    serverTreeEmpty,
    trunkAt,
    renderURI,
    renderResource,
    renderLink,
    resourceAt,
    defaultResource,
    serveResource,
    renderServerTree,
    addServerNode
  ) where

import Happstack.Server
import Distribution.Server.Types

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<*>), (<$>))
import Control.Monad (msum)
import Data.Maybe (maybeToList)
import Data.List (intercalate, find)
import qualified Text.ParserCombinators.ReadP as Parse

import qualified Happstack.Server.SURI as SURI
import System.FilePath.Posix ((</>))
--for basic link creating
import Text.XHtml.Strict (anchor, href, (!), (<<), toHtml, Html)

data Resource = Resource {
    resourceLocation :: BranchPath,
    resourceGet    :: [(Content, ServerResponse)],
    resourcePut    :: [(Content, ServerResponse)],
    resourcePost   :: [(Content, ServerResponse)],
    resourceDelete :: [(Content, ServerResponse)],
    -- When we can't possibly know in advance what sort of content-type there is - e.g. serving something from a tarball using a TarIndex.
    -- 
    -- This field likewise is not guarded on empty path lengths. To require typing in all cases, something like TrailingBranch could be
    -- added to BranchComponent
    resourceUntyped :: Maybe (ServerResponse)
}
-- favors first
instance Monoid Resource where
    mempty = Resource [] [] [] [] [] Nothing
    mappend (Resource bpath rget rput rpost rdelete run) (Resource bpath' rget' rput' rpost' rdelete' run') =
        Resource (simpleCombine bpath bpath') (ccombine rget rget') (ccombine rput rput')
                   (ccombine rpost rpost') (ccombine rdelete rdelete') (mappend run run')
      where ccombine list list' = foldr insertType list' list
            insertType conres list = case break ((fst conres==) . fst) list of
                (absent, []) -> conres:absent
                (prefix, (_:suffix)) -> prefix ++ (conres:suffix)
            -- two resources that are being combined will generally have the same path
            -- nonetheless this is a monoid-friendly attempt to combine two separate ones
            -- should BranchPath be part of resource in the first place?
            zipCombine (x:xs) (y:ys) | x == y    = x:zipCombine xs ys
                                     | otherwise = x:xs
            zipCombine []     (y:ys) = y:ys
            zipCombine xs     []     = xs
            -- easier, if not hackier, way to do it - like Monoid instance of Last
            simpleCombine xs ys = if null ys then xs else ys

type Content = String

resourceAt :: String -> Resource
resourceAt = defaultResource . trunkAt

-- Perhaps provide HTTP 405 (Method not allowed) defaults
defaultResource :: BranchPath -> Resource
defaultResource bpath = Resource bpath [] [] [] [] Nothing

extendResource :: Resource -> Resource
extendResource resource = defaultResource (resourceLocation resource)

data BranchComponent = StaticBranch String | DynamicBranch String  | DynamicFormat String | StaticFormat String deriving (Show, Eq, Ord)
type BranchPath = [BranchComponent]

type URIGen = DynamicPath -> Maybe String

-- Allows the formation of a URI from a URI specification (BranchPath).
-- Unfortunately, URIs may need to be obey additional constraints not checked here.
-- Each feature should probably provide its own typed generating functions.
-- This method might support Maybe Content too, if .format extensions are put in place.
renderURI :: BranchPath -> URIGen
renderURI path dpath = ("/" </>) <$> go (reverse path)
    where go (StaticBranch  sdir:rest) = (SURI.escape sdir </>) <$> go rest
          go (DynamicBranch sdir:rest) = ((</>) . SURI.escape) <$> lookup sdir dpath <*> go rest
          go [] = Just ""

renderResource :: Resource -> URIGen
renderResource = renderURI . resourceLocation

renderLink :: URIGen -> DynamicPath -> String -> Html
renderLink gen dpath text = case gen dpath of
    Nothing  -> toHtml text
    Just uri -> anchor ! [href uri] << text


data ServerTree a = ServerTree {
    nodeResponse :: Maybe a,
    nodeForest :: Map BranchComponent (ServerTree a)
} deriving (Show)

instance Functor ServerTree where
    fmap func (ServerTree value forest) = ServerTree (fmap func value) (Map.map (fmap func) forest)

serverTreeEmpty :: ServerTree a
serverTreeEmpty = ServerTree Nothing Map.empty

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
serveResource (Resource trunk rget rput rpost rdelete run) = (,) trunk $ \config dpath -> msum $
    map (\func -> func config dpath) $ untypePart ++ methodPart ++ [optionPart]
  where
    optionPart = makeOptions $ concat [ met | ((_:_), met) <- zip methods methodsList]
    methodPart = [ (methodSP met .) . negotiate defaultRes res | ((defaultRes:res), met) <- zip methods methodsList]
    untypePart = maybeToList run
    -- might we want to do methodOnly in some places (instead of methodSP)?
    -- degenerate negotiation - most of the work should happen in ContentType.hs
    negotiate :: (Content, ServerResponse) -> [(Content, ServerResponse)] -> ServerResponse
    negotiate defaultRes _ = snd defaultRes
    methods = [rget, rput, rpost, rdelete]
    methodsList = [[GET, HEAD], [PUT], [POST], [DELETE]]
    makeOptions :: [Method] -> ServerResponse
    makeOptions methodList = \_ _ -> methodSP OPTIONS $ do
        setHeaderM "Allow" (intercalate ", " . map show $ methodList)
        noBody
    noBody = return $ toResponse ()

-- essentially a ReaderT (Config, DynamicPath) ServerPart Response
renderServerTree :: Config -> DynamicPath -> ServerTree ServerResponse -> ServerPart Response
renderServerTree config dpath (ServerTree func forest) = msum $ maybeToList (fmap (\fun -> fun config dpath) func) ++ map (uncurry renderBranch) (Map.toList forest)
  where
    renderBranch :: BranchComponent -> ServerTree ServerResponse -> ServerPart Response
    renderBranch (StaticBranch  sdir) tree = dir sdir $ renderServerTree config dpath tree
    renderBranch (DynamicBranch sdir) tree = path $ \pname -> renderServerTree config ((sdir, pname):dpath) tree
    renderBranch _ _ = return . toResponse $ "Uncharted territory" -- parse formats and munge accept headers accordingly

reinsert :: Monoid a => BranchComponent -> ServerTree a -> Map BranchComponent (ServerTree a) -> Map BranchComponent (ServerTree a)
-- combine will only be called if branchMap already contains the key
reinsert key newTree branchMap = Map.insertWith combine key newTree branchMap

combine :: Monoid a => ServerTree a -> ServerTree a -> ServerTree a
combine (ServerTree newResponse newForest) (ServerTree oldResponse oldForest) =
    -- replace old resource with new resource, combine old and new responses
    ServerTree (mappend newResponse oldResponse) (Map.foldWithKey reinsert oldForest newForest)

addServerNode :: Monoid a => BranchPath -> a -> ServerTree a -> ServerTree a
addServerNode trunk response tree = treeFold trunk (ServerTree (Just response) Map.empty) tree

--this function takes a list whose head is the resource and traverses leftwards in the URI
--this is due to the original design of specifying URI branches: if the resources are
--themselves encoded in the branch, then subresources should share the same parent
--resource, sharing list tails.
--
--This version is greatly simplified compared to what was previously here.
treeFold :: Monoid a => BranchPath -> ServerTree a -> ServerTree a -> ServerTree a
treeFold [] newChild topLevel = combine newChild topLevel
treeFold (sdir:otherTree) newChild topLevel = treeFold otherTree (ServerTree Nothing $ Map.singleton sdir newChild) topLevel 

