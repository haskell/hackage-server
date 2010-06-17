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
    extendResource,
    serveResource,
    renderServerTree,
    addServerNode,
    drawServerTree
  ) where

import Happstack.Server
import Distribution.Server.Types

import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Data.Maybe (maybeToList)
import Data.Function (on)
import Data.List (intercalate, unionBy, findIndices)
import qualified Text.ParserCombinators.Parsec as Parse

import qualified Happstack.Server.SURI as SURI
import System.FilePath.Posix ((</>))
--for basic link creating
import Text.XHtml.Strict (anchor, href, (!), (<<), toHtml, Html)
import qualified Data.Tree as Tree (Tree(..), drawTree)

type Content = String

data Resource = Resource {
    resourceLocation :: BranchPath,
    resourceGet    :: [(Content, ServerResponse)],
    resourcePut    :: [(Content, ServerResponse)],
    resourcePost   :: [(Content, ServerResponse)],
    resourceDelete :: [(Content, ServerResponse)],
    resourceFormat  :: ResourceFormat,
    resourcePathEnd :: BranchEnd
}
-- favors first
instance Monoid Resource where
    mempty = Resource [] [] [] [] [] noFormat NoSlash
    mappend (Resource bpath rget rput rpost rdelete rformat rend)
            (Resource bpath' rget' rput' rpost' rdelete' rformat' rend') =
        Resource (simpleCombine bpath bpath') (ccombine rget rget') (ccombine rput rput')
                   (ccombine rpost rpost') (ccombine rdelete rdelete')
                   (simpleCombine rformat rformat') (simpleCombine rend rend')
      where ccombine = unionBy ((==) `on` fst)
            simpleCombine xs ys = if null bpath then ys else xs

-- | A path element of a URI. Unstable data structure. Possible to-be-removed current and to-be-added future components include:
--
-- * StaticBranch dirName - \/dirName
-- * DynamicBranch dynamicName - \/anyName (mapping created dynamicName -> anyName)
-- * TrailingBranch - doesn't consume any path; it's here to prevent e.g. conflict between \/.:format and \/...
--
-- trunkAt yields a simple list of BranchComponents
-- resourceAt yields the same, and some complex metadata for processing formats and the like
data BranchComponent = StaticBranch String | DynamicBranch String | TrailingBranch deriving (Show, Eq, Ord)
type BranchPath = [BranchComponent]

-- this type dictates the preprocessing we must do on extensions (foo.json) when serving Resources
-- For BranchFormat, we need to do:
-- 1. for NoFormat - don't do any preprocessing. The second field is ignored here.
-- 1. for StaticFormat  - look for a specific format, and guard against that
-- 2. for DynamicFormat - strip off any extension (starting from the right end, so no periods allowed)
-- Under either of the above cases, the component might need to be preprocessed as well.
-- 1. for Nothing - this means a standalone format, like /.json (as in /distro/arch/.json)
--                  either accept /distro/arch/ or read /distro/arch/.format and pass it along
-- 2. for Just (StaticBranch sdir) - strip off the pre-format part and make sure it equals sdir
-- 3. for Just (DynamicBranch sdir) - strip off the pre-format part and pass it with the DynamicPath as sdir
-- DynamicFormat also has the property that it is optional, and defaulting is allowed.
data ResourceFormat = ResourceFormat BranchFormat (Maybe BranchComponent) deriving (Show, Eq, Ord)

noFormat :: ResourceFormat
noFormat = ResourceFormat NoFormat Nothing

data BranchFormat = NoFormat | DynamicFormat | StaticFormat String deriving (Show, Eq, Ord)
data BranchEnd  = Slash | NoSlash | Trailing deriving (Show, Eq, Ord)

resourceAt :: String -> Resource
resourceAt arg = mempty
  { resourceLocation = reverse loc
  , resourceFormat  = format
  , resourcePathEnd = slash
  }
  where
    branch = either trunkError id $ Parse.parse parseFormatTrunkAt "Distribution.Server.Resource.trunkAt" arg
    trunkError pe = error $ "Distribution.Server.Resource.resourceAt: Could not parse trunk literal " ++ show arg ++ ". Parsec error: " ++ show pe
    (loc, slash, format) = trunkToResource branch

extendResource :: Resource -> Resource
extendResource resource = resource { resourceGet = [], resourcePut = [], resourcePost = [], resourceDelete = [] }

-- other combinatoresque methods here - e.g. addGet :: Content -> ServerResponse -> Resource -> Resource

type URIGen = DynamicPath -> Maybe String

-- Allows the formation of a URI from a URI specification (BranchPath).
-- Unfortunately, URIs may need to be obey additional constraints not checked here.
-- Each feature should probably provide its own typed generating functions.
-- This method might support Maybe Content too, if .format extensions are put in place.
renderURI :: BranchPath -> URIGen
renderURI bpath dpath = ("/" </>) <$> go (reverse bpath)
    where go (StaticBranch  sdir:rest) = (SURI.escape sdir </>) <$> go rest
          go (DynamicBranch sdir:rest) = ((</>) . SURI.escape) <$> lookup sdir dpath <*> go rest
          go (TrailingBranch:_) = Just ""
          go [] = Just ""

-- TODO: take advantage of the metadata, including adding formats
-- The formats are particularly needed if redirecting from a POST request to a different URL
-- e.g. POST to /packages/.json with foo-0.1.tar.gz, be redirected to /package/foo-0.1.json
renderResource :: Resource -> URIGen
renderResource = renderURI . resourceLocation

renderLink :: URIGen -> DynamicPath -> String -> Html
renderLink gen dpath text = case gen dpath of
    Nothing  -> toHtml text
    Just uri -> anchor ! [href uri] << text

-- top-level call to weed out cases that don't make sense recursively
trunkToResource  :: [(BranchComponent, BranchFormat)] -> ([BranchComponent], BranchEnd, ResourceFormat)
trunkToResource [] = ([], Slash, noFormat) -- ""
trunkToResource [(StaticBranch "", format)] = ([], Slash, ResourceFormat format Nothing) -- "/" or "/.format"
trunkToResource anythingElse = trunkToResource' anythingElse

trunkToResource' :: [(BranchComponent, BranchFormat)] -> ([BranchComponent], BranchEnd, ResourceFormat)
trunkToResource' [] = ([], NoSlash, noFormat)
-- /...
trunkToResource' ((TrailingBranch, _):xs) | null xs = ([TrailingBranch], Trailing, noFormat)
                                          | otherwise = error "Trailing path only allowed at very end"
-- /foo/, /foo/.format, or /foo/.:format
trunkToResource' [(branch, NoFormat), (StaticBranch "", format)] = pathFormatSep format
  where pathFormatSep (StaticFormat form) = ([branch, StaticBranch ("." ++ form)], NoSlash, noFormat) -- /foo/.json, format is not optional here!
        pathFormatSep DynamicFormat = ([branch], Slash, ResourceFormat DynamicFormat Nothing) -- /foo/.format
        pathFormatSep NoFormat = ([branch], Slash, noFormat) -- /foo/
-- /foo.format/[...] (rewrite into next case)
trunkToResource' ((StaticBranch sdir, StaticFormat format):xs) = trunkToResource' ((StaticBranch (sdir ++ "." ++ format), NoFormat):xs)
-- /foo/[...]
trunkToResource' ((branch, NoFormat):xs) = case trunkToResource' xs of (xs', slash, res) -> (branch:xs', slash, res)
-- /foo.format
trunkToResource' [(branch, format)] = pathFormat branch format
  where pathFormat (StaticBranch sdir) (StaticFormat form) = ([StaticBranch (sdir ++ "." ++ form)], NoSlash, noFormat) -- foo.json
        pathFormat (StaticBranch sdir) DynamicFormat = ([DynamicBranch sdir], NoSlash, ResourceFormat DynamicFormat (Just branch)) -- foo.:json
        pathFormat (DynamicBranch {}) (StaticFormat {}) = ([branch], NoSlash, ResourceFormat format (Just branch)) -- :foo.json
        pathFormat (DynamicBranch {}) DynamicFormat = ([branch], NoSlash, ResourceFormat DynamicFormat (Just branch)) -- :foo.:json
        pathFormat _ NoFormat = ([branch], NoSlash, noFormat) -- foo or :foo
        pathFormat _ _ = error "Trailing path can't have a format"
-- /foo.format/[...]
trunkToResource' _ = error "Format only allowed at end of path"

trunkAt :: String -> BranchPath
trunkAt arg = either trunkError reverse $ Parse.parse parseTrunkAt "Distribution.Server.Resource.trunkAt" arg
    where trunkError pe = error $ "Distribution.Server.Resource.trunkAt: Could not parse trunk literal " ++ show arg ++ ". Parsec error: " ++ show pe

parseTrunkAt :: Parse.Parser [BranchComponent]
parseTrunkAt = do
    components <- Parse.many (Parse.try parseComponent)
    Parse.optional (Parse.char '/')
    Parse.eof
    return components
  where
    parseComponent = do
        Parse.char '/'
        fmap DynamicBranch (Parse.char ':' >> Parse.many1 (Parse.noneOf "/"))
          Parse.<|> fmap StaticBranch (Parse.many1 (Parse.noneOf "/"))

parseFormatTrunkAt :: Parse.Parser [(BranchComponent, BranchFormat)]
parseFormatTrunkAt = do
    components <- Parse.many (Parse.try parseComponent)
    rest <- Parse.option [] (Parse.char '/' >> return [(StaticBranch "", NoFormat)])
    Parse.eof
    return (components ++ rest)
  where
    parseComponent :: Parse.Parser (BranchComponent, BranchFormat)
    parseComponent = do
        Parse.char '/'
        Parse.choice $ map Parse.try
          [ Parse.char '.' >> Parse.many1 (Parse.char '.') >>
            ((Parse.lookAhead (Parse.char '/') >> return ()) Parse.<|> Parse.eof) >> return (TrailingBranch, NoFormat)
          , Parse.char ':' >> parseMaybeFormat DynamicBranch
          , do Parse.lookAhead (Parse.satisfy (/=':'))
               Parse.choice $ map Parse.try
                 [ parseMaybeFormat StaticBranch
                 , fmap ((,) (StaticBranch "")) parseFormat
                 , fmap (flip (,) NoFormat . StaticBranch) untilNext
                 ]
          ]
    parseMaybeFormat :: (String -> BranchComponent) -> Parse.Parser (BranchComponent, BranchFormat)
    parseMaybeFormat control = do
        sdir <- Parse.many1 (Parse.noneOf "/.")
        format <- Parse.option NoFormat parseFormat
        return (control sdir, format)
    parseFormat :: Parse.Parser BranchFormat
    parseFormat = Parse.char '.' >> Parse.choice
        [ Parse.char ':' >> untilNext >> return DynamicFormat
        , fmap StaticFormat untilNext
        ]
    untilNext :: Parse.Parser String
    untilNext = Parse.many1 (Parse.noneOf "/")

-- serveResource does all the path format and HTTP method preprocessing for a Resource
--
-- For a small curl-based test suite of [Resource]:
-- [res "/foo" ["json"], res "/foo/:bar.:format" ["html", "json"], res "/baz/test/.:format" ["html", "text", "json"], res "/package/:package/:tarball.tar.gz" ["tarball"], res "/a/:a/:b/" ["html", "json"], res "/mon/..." [""], res "/wiki/path.:format" [], res "/hi.:format" ["yaml", "mofo"]]
--     where res field formats = (resourceAt field) { resourceGet = map (\format -> (format, \_ -> return . toResponse . (++"\n") . ((show format++" - ")++) . show)) formats }
serveResource :: Resource -> (BranchPath, ServerResponse)
serveResource (Resource trunk rget rput rpost rdelete rformat rend) = (,) trunk $ \config dpath -> msum $
    map (\func -> func config dpath) $ methodPart ++ [optionPart]
  where
    optionPart = makeOptions $ concat [ met | ((_:_), met) <- zip methods methodsList]
    methodPart = [ serveResources met res | (res@(_:_), met) <- zip methods methodsList]
    -- some of the dpath lookup calls can be replaced by pattern matching the head/replacing
    -- at the moment, duplicate entries tend to be inserted in dpath, because old ones are not replaced
    -- Procedure:
    -- > Guard against method
    -- > Extract format/whatnot
    -- > Potentially redirect to canonical slash form
    -- > Go from format/content-type to ServerResponse to serve-}
    serveResources :: [Method] -> [(Content, ServerResponse)] -> ServerResponse
    serveResources met res = case rend of
        Trailing -> \config dpath -> methodOnly met >> serveContent res config dpath
        _ -> \config dpath -> serveFormat res met config dpath
    serveFormat :: [(Content, ServerResponse)] -> [Method] -> ServerResponse
    serveFormat res met = case rformat of
        ResourceFormat (StaticFormat format) Nothing -> \config dpath -> methodSP met $ do
            -- this branch shouldn't happen - /foo/.json would instead be stored as two static dirs
            guard (lookup "format" dpath == Just ('.':format))
            serveContent res config dpath
        ResourceFormat (StaticFormat format) (Just (StaticBranch sdir)) -> \config dpath -> methodSP met $
            if lookup sdir dpath == Just (sdir ++ "." ++ format) then mzero
                                                                 else serveContent res config dpath
        ResourceFormat (StaticFormat format) (Just (DynamicBranch sdir)) -> \config dpath -> methodSP met $
            case matchExt format =<< lookup sdir dpath of
                Just pname -> serveContent res config ((sdir, pname):dpath)
                Nothing -> mzero
        ResourceFormat DynamicFormat Nothing -> \config dpath ->
            msum [ methodSP met $ serveContent res config dpath
                 , path $ \pname -> case pname of
                       ('.':format) -> methodSP met $ serveContent res config (("format", format):dpath)
                       _ -> mzero
                 ]
        ResourceFormat DynamicFormat (Just (StaticBranch sdir)) -> \config dpath -> methodSP met $
            case fmap extractExt (lookup sdir dpath) of
                Just (pname, format) | pname == sdir -> serveContent res config (("format", format):dpath)
                _ -> mzero
        ResourceFormat DynamicFormat (Just (DynamicBranch sdir)) -> \config dpath -> methodSP met $
            -- this is somewhat complicated. consider /pkg-0.1 and /pkg-0.1.html. If the format is optional, where to split?
            -- the solution is to manually check the available formats to see if something matches
            -- if this situation comes up in practice, try to require a trailing slash, e.g. /pkg-0.1/.html
            case fmap (\sd -> (,) sd $ extractExt sd) (lookup sdir dpath) of
                Just (full, (pname, format)) -> do
                    let splitOption = serveContent res config (("format", format):(sdir, pname):dpath)
                        fullOption  = serveContent res config ((sdir, full):dpath)
                    case guard (not $ null format) >> lookup format res of
                        Nothing -> fullOption
                        Just {} -> msum [splitOption, fullOption]
                _ -> mzero
        -- ResourceFormat NoFormat Nothing
        _ -> \config dpath -> methodSP met $ serveContent res config dpath
    serveContent :: [(Content, ServerResponse)] -> ServerResponse
    serveContent res config dpath = do
        -- there should be no remaining path segments at this point, now check page redirection
        met <- fmap rqMethod askRq
        -- we don't check if the page exists before redirecting!
        -- just send them to the canonical place the document may or may not be
        (if met == HEAD || met == GET
            then redirCanonicalSlash dpath
            else id) $ do
        -- "Find " ++ show (lookup "format" dpath) ++ " in " ++ show (map fst res)
        case lookup "format" dpath of
            Just format@(_:_) -> case lookup format res of
                               -- return a specific format if it is found
                Just answer -> answer config dpath
                Nothing -> mzero -- return 404 if the specific format is not found
                  -- return default response when format is empty or non-existent
            _ -> (snd $ head res) config dpath
    redirCanonicalSlash :: DynamicPath -> ServerPart Response -> ServerPart Response
    redirCanonicalSlash dpath trueRes = case rformat of
        ResourceFormat format Nothing | format /= NoFormat -> case lookup "format" dpath of
            Just {} -> requireNoSlash `mplus` trueRes
            Nothing -> requireSlash `mplus` trueRes
        _ -> case rend of
            Slash    -> requireSlash `mplus` trueRes
            NoSlash  -> requireNoSlash `mplus` trueRes
            Trailing -> mplus (nullDir >> requireSlash) trueRes
    requireSlash = do
        theUri <- fmap rqUri askRq
        guard $ last theUri /= '/'
        movedPermanently (theUri ++ "/") (toResponse ())
    requireNoSlash = do
        theUri <- fmap rqUri askRq
        guard $ last theUri == '/'
        movedPermanently (reverse . dropWhile (=='/') . reverse $ theUri) (toResponse ())
    -- matchExt and extractExt could also use manual string-chomping recursion if wanted
    matchExt format pname = let fsize = length format
                                (pname', format') = splitAt (length pname - fsize - 1) pname
                            in if '.':format == format' then Just pname'
                                                        else Nothing
    extractExt pname = case findIndices (=='.') pname of
        [] -> (pname, "")
        xs -> case splitAt (last xs) pname of (pname', _:format) -> (pname', format)
                                              _ -> (pname, "") -- this shouldn't happen
    methods = [rget, rput, rpost, rdelete]
    methodsList = [[GET, HEAD], [PUT], [POST], [DELETE]]
    makeOptions :: [Method] -> ServerResponse
    makeOptions methodList = \_ _ -> methodSP OPTIONS $ do
        setHeaderM "Allow" (intercalate ", " . map show $ methodList)
        return $ toResponse ()

----------------------------------------------------------------------------

data ServerTree a = ServerTree {
    nodeResponse :: Maybe a,
    nodeForest :: Map BranchComponent (ServerTree a)
} deriving (Show)

instance Functor ServerTree where
    fmap func (ServerTree value forest) = ServerTree (fmap func value) (Map.map (fmap func) forest)

drawServerTree :: ServerTree a -> String
drawServerTree tree = Tree.drawTree (transformTree tree Nothing)
  where transformTree (ServerTree res for) mlink = Tree.Node (drawLink mlink res) (map transformForest $ Map.toList for)
        drawLink mlink res = maybe "" ((++": ") . show) mlink ++ maybe "(nothing)" (const "response") res
        transformForest (link, tree') = transformTree tree' (Just link)

serverTreeEmpty :: ServerTree a
serverTreeEmpty = ServerTree Nothing Map.empty

-- essentially a ReaderT (Config, DynamicPath) ServerPart Response
-- this always renders parent URIs, but usually we guard against remaining path segments, so it's fine
renderServerTree :: Config -> DynamicPath -> ServerTree ServerResponse -> ServerPart Response
renderServerTree config dpath (ServerTree func forest) = msum $ maybeToList (fmap (\fun -> fun config dpath) func) ++ map (uncurry renderBranch) (Map.toList forest)
  where
    renderBranch :: BranchComponent -> ServerTree ServerResponse -> ServerPart Response
    renderBranch (StaticBranch  sdir) tree = dir sdir $ renderServerTree config dpath tree
    renderBranch (DynamicBranch sdir) tree = path $ \pname -> renderServerTree config ((sdir, pname):dpath) tree
    renderBranch TrailingBranch tree = renderServerTree config dpath tree

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

