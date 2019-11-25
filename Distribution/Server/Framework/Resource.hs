{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving,
             FlexibleContexts, FlexibleInstances, NamedFieldPuns #-}

module Distribution.Server.Framework.Resource (
    -- | Paths
    DynamicPath,
    BranchComponent(..),
    BranchPath,
    trunkAt,

    -- | Resources
    Resource(..),
    ResourceFormat(..),
    BranchFormat(..),
    BranchEnd(..),
    Content,
    resourceAt,
    extendResource,
    extendResourcePath,
    serveResource,
    resourceMethods,

    -- | URI generation
    renderURI,
    renderResource,
    renderResource',

    -- | ServerTree
    ServerTree(..),
    serverTreeEmpty,
    addServerNode,
    renderServerTree,
    drawServerTree,

    -- | Error page serving
    serveErrorResponse,
    ServerErrorResponse,
  ) where

import Distribution.Server.Prelude

import Happstack.Server
import Distribution.Server.Framework.HappstackUtils (remainingPathString, uriEscape)
import Distribution.Server.Util.ContentType (parseContentAccept)
import Distribution.Server.Framework.Error

import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (on)
import Data.List (intercalate, unionBy, findIndices, find)
import qualified Text.ParserCombinators.Parsec as Parse

import System.FilePath.Posix ((</>), (<.>))
import qualified Data.Tree as Tree (Tree(..), drawTree)
import qualified Data.ByteString.Char8 as BS -- Used for accept header only

type Content = String

type DynamicPath = [(String, String)]

type ServerResponse = DynamicPath -> ServerPartE Response
type ServerErrorResponse = ErrorResponse -> ServerPartE Response

-- | A resource is an object that handles requests at a given URI. Best practice
-- is to construct it by calling resourceAt and then setting the method fields
-- using record update syntax. You can also extend an existing resource with
-- extendResource, which can be mappended to the original to combine their
-- functionality, or with extendResourcePath.
data Resource = Resource {
    -- | The location in a form which can be added to a ServerTree.
    resourceLocation :: BranchPath,
    -- | Handlers for GET requests for different content-types
    resourceGet      :: [(Content, ServerResponse)],
    -- | Handlers for PUT requests
    resourcePut      :: [(Content, ServerResponse)],
    -- | Handlers for POST requests
    resourcePost     :: [(Content, ServerResponse)],
    -- | Handlers for DELETE requests
    resourceDelete   :: [(Content, ServerResponse)],
    -- | The format conventions held by the resource.
    resourceFormat   :: ResourceFormat,
    -- | The trailing slash conventions held by the resource.
    resourcePathEnd  :: BranchEnd,
    -- | Human readable description of the resource
    resourceDesc     :: [(Method, String)]
  }

-- favors first
instance Monoid Resource where
    mempty = Resource [] [] [] [] [] noFormat NoSlash []
    mappend = (<>)

instance Semigroup Resource where
    (Resource bpath rget rput rpost rdelete rformat rend desc) <>
      (Resource bpath' rget' rput' rpost' rdelete' rformat' rend' desc') =
        Resource (simpleCombine bpath bpath') (ccombine rget rget') (ccombine rput rput')
                   (ccombine rpost rpost') (ccombine rdelete rdelete')
                   (simpleCombine rformat rformat') (simpleCombine rend rend') (desc ++ desc')
      where ccombine = unionBy ((==) `on` fst)
            simpleCombine xs ys = if null bpath then ys else xs

-- | A path element of a URI.
--
-- * StaticBranch dirName - \/dirName
-- * DynamicBranch dynamicName - \/anyName (mapping created dynamicName -> anyName)
-- * TrailingBranch - doesn't consume any path; it's here to prevent e.g. conflict between \/.:format and \/...
--
-- trunkAt yields a simple list of BranchComponents.
-- resourceAt yields the same, and some complex metadata for processing formats and the like.
data BranchComponent = StaticBranch String | DynamicBranch String | TrailingBranch deriving (Show, Eq, Ord)
type BranchPath = [BranchComponent]

-- | This type dictates the preprocessing we must do on extensions (foo.json) when serving Resources
-- For BranchFormat, we need to do:
-- 1. for NoFormat - don't do any preprocessing. The second field is ignored here.
-- 1. for StaticFormat  - look for a specific format, and guard against that
-- 2. for DynamicFormat - strip off any extension (starting from the right end, so no periods allowed)
-- Under either of the above cases, the component might need to be preprocessed as well.
-- 1. for Nothing - this means a standalone format, like \/.json (as in \/distro\/arch\/.json)
--                  either accept \/distro\/arch\/ or read \/distro\/arch\/.format and pass it along
-- 2. for Just (StaticBranch sdir) - strip off the pre-format part and make sure it equals sdir
-- 3. for Just (DynamicBranch sdir) - strip off the pre-format part and pass it with the DynamicPath as sdir
-- DynamicFormat also has the property that it is optional, and defaulting is allowed.
data ResourceFormat = ResourceFormat BranchFormat (Maybe BranchComponent) deriving (Show, Eq, Ord)

noFormat :: ResourceFormat
noFormat = ResourceFormat NoFormat Nothing

data BranchFormat = NoFormat | DynamicFormat | StaticFormat String deriving (Show, Eq, Ord)
data BranchEnd  = Slash | NoSlash | Trailing deriving (Show, Eq, Ord)

-- | Creates an empty resource from a string specifying its location and format conventions.
--
-- (Explain path literal syntax.)
resourceAt :: String -> Resource
resourceAt arg = mempty
  { resourceLocation = reverse loc
  , resourceFormat  = format
  , resourcePathEnd = slash
  }
  where
    branch = either trunkError id $ Parse.parse parseFormatTrunkAt "Distribution.Server.Resource.parseFormatTrunkAt" arg
    trunkError pe = error $ "Distribution.Server.Resource.resourceAt: Could not parse trunk literal " ++ show arg ++ ". Parsec error: " ++ show pe
    (loc, slash, format) = trunkToResource branch

-- | Creates a new resource at the same location, but without any of the request
-- handlers of the original. When mappend'd to the original, its methods and content-types
-- will be combined. This can be useful for extending an existing resource with new representations and new
-- functionality.
extendResource :: Resource -> Resource
extendResource resource = resource {
    resourceDesc   = []
  , resourceGet    = []
  , resourcePut    = []
  , resourcePost   = []
  , resourceDelete = []
  }

-- | Creates a new resource that is at a subdirectory of an existing resource. This function takes care of formats
-- as best as it can.
--
-- extendResourcePath "\/bar\/.:format" (resourceAt "\/data\/:foo.:format") == resourceAt "\/data\/:foo\/bar\/:.format"
--
-- Extending static formats with this method is not recommended. (extending "\/:tarball.tar.gz"
-- with "\/data" will give "\/:tarball\/data", with the format stripped, and extending
-- "\/help\/.json" with "\/tree" will give "\/help\/.json\/tree")
extendResourcePath :: String -> Resource -> Resource
extendResourcePath arg resource =
  let endLoc = case resourceFormat resource of
        ResourceFormat (StaticFormat _) Nothing -> case loc of
            (DynamicBranch "format":rest) -> rest
            _ -> funcError "Static ending format must have dynamic 'format' branch"
        ResourceFormat (StaticFormat _) (Just (StaticBranch sdir)) -> case loc of
            (DynamicBranch sdir':rest) | sdir == sdir' -> rest
            _ -> funcError "Static branch and format must match stated location"
        ResourceFormat (StaticFormat _) (Just (DynamicBranch sdir)) -> case loc of
            (DynamicBranch sdir':_) | sdir == sdir' -> loc
            _ -> funcError "Dynamic branch with static format must match stated location"
        ResourceFormat DynamicFormat Nothing -> loc
        ResourceFormat DynamicFormat (Just (StaticBranch sdir)) -> case loc of
            (DynamicBranch sdir':rest) | sdir == sdir' -> StaticBranch sdir:rest
            _ -> funcError "Dynamic format with static branch must match stated location"
        ResourceFormat DynamicFormat (Just (DynamicBranch sdir)) -> case loc of
            (DynamicBranch sdir':_) | sdir == sdir' -> loc
            _ -> funcError "Dynamic branch and format must match stated location"
        -- For a URI like /resource/.format: since it is encoded as NoFormat in trunkToResource,
        -- this branch will incorrectly be taken. this isn't too big a handicap though
        ResourceFormat NoFormat Nothing -> case loc of
            (TrailingBranch:rest) -> rest
            _ -> loc
        _ -> funcError $ "invalid resource format in argument 2"
  in
    extendResource resource { resourceLocation = reverse loc' ++ endLoc, resourceFormat = format', resourcePathEnd = slash' }
  where
    branch = either trunkError id $ Parse.parse parseFormatTrunkAt "Distribution.Server.Resource.parseFormatTrunkAt" arg
    trunkError pe = funcError $ "Could not parse trunk literal " ++ show arg ++ ". Parsec error: " ++ show pe
    funcError reason = error $ "Distribution.Server.Resource.extendResourcePath :" ++ reason
    loc = resourceLocation resource
    (loc', slash', format') = trunkToResource branch

-- Allows the formation of a URI from a URI specification (BranchPath).
-- URIs may obey additional constraints and have special rules (e.g., formats).
-- To accommodate these, insteaduse renderResource to get a URI.
--
-- ".." is a special argument that fills in a TrailingBranch. Make sure it's
-- properly escaped (see Happstack.Server.SURI)
--
-- renderURI (trunkAt "/home/:user/..")
--    [("user", "mgruen"), ("..", "docs/todo.txt")]
--    == "/home/mgruen/docs/todo.txt"
renderURI :: BranchPath -> DynamicPath -> String
renderURI bpath dpath = renderGenURI bpath (flip lookup dpath)

-- Render a URI generally using a function of one's choosing (usually flip lookup dpath)
-- Stops when a requested field is not found, yielding an incomplete URI. I think
-- this is better than having a function that doesn't return *some* result.
renderGenURI :: BranchPath -> (String -> Maybe String) -> String
renderGenURI bpath pathFunc = "/" </> go (reverse bpath)
  where go (StaticBranch  sdir:rest) = uriEscape sdir </> go rest
        go (DynamicBranch sdir:rest)
          | (ddir, sformat@('.':_)) <- break (=='.') sdir
          = case pathFunc ddir of
            Nothing  -> ""
            Just str -> uriEscape str <.> uriEscape sformat </> go rest
        go (DynamicBranch sdir:rest) = case pathFunc sdir of
            Nothing  -> ""
            Just str -> uriEscape str </> go rest
        go (TrailingBranch:_) = fromMaybe "" $ pathFunc ".."
        go [] = ""

-- Doesn't use a DynamicPath - rather, munches path components from a list
-- it stops if there aren't enough, and returns the extras if there's more than enough.
--
-- Trailing branches currently are assumed to be complete escaped URI paths.
renderListURI :: BranchPath -> [String] -> (String, [String])
renderListURI bpath list = let (res, extra) = go (reverse bpath) list in ("/" </> res, extra)
    where go (StaticBranch  sdir:rest) xs  = let (res, extra) = go rest xs in (uriEscape sdir </> res, extra)
          go (DynamicBranch _:rest) (x:xs) = let (res, extra) = go rest xs in (uriEscape x </> res, extra)
          go (TrailingBranch:_) xs = case xs of [] -> ("", []); (x:rest) -> (x, rest)
          go _ rest = ("", rest)

-- Given a Resource, construct a URI generator. If the Resource uses a dynamic format, it can
-- be passed in the DynamicPath as "format". Trailing slash conventions are obeyed.
--
-- See documentation for the Resource to see the conventions in interpreting the DynamicPath.
-- As in renderURI, ".." is interpreted as the trailing branch. It should be the case that
--
-- > fix $ \r -> (resourceAt uri) { resourceGet = [("txt", ok . toResponse . renderResource' r)] }
--
-- will print the URI that was used to request the page.
--
-- renderURI (resourceAt "/home/:user/docs/:doc.:format")
--     [("user", "mgruen"), ("doc", "todo"), ("format", "txt")]
--     == "/home/mgruen/docs/todo.txt"
renderResource' :: Resource -> DynamicPath -> String
renderResource' resource dpath = renderResourceFormat resource (lookup "format" dpath)
               $ renderGenURI (normalizeResourceLocation resource) (flip lookup dpath)

-- A more convenient form of renderResource'. Used when you know the path structure.
-- The first argument unused in the path, if any, will be used as a dynamic format,
-- if any.
--
-- renderResource (resourceAt "/home/:user/docs/:doc.:format")
--     ["mgruen", "todo", "txt"]
--     == "/home/mgruen/docs/todo.txt"
renderResource :: Resource -> [String] -> String
renderResource resource list = case renderListURI (normalizeResourceLocation resource) list of
    (str, format:_) -> renderResourceFormat resource (Just format) str
    (str, []) -> renderResourceFormat resource Nothing str

-- in some cases, DynamicBranches are used to accommodate formats for StaticBranches.
-- this returns them to their pre-format state so renderGenURI can handle them
normalizeResourceLocation :: Resource -> BranchPath
normalizeResourceLocation resource = case (resourceFormat resource, resourceLocation resource) of
    (ResourceFormat _ (Just (StaticBranch sdir)), DynamicBranch sdir':xs) | sdir == sdir' -> StaticBranch sdir:xs
    (_, loc) -> loc

renderResourceFormat :: Resource -> Maybe String -> String -> String
renderResourceFormat resource dformat str = case (resourcePathEnd resource, resourceFormat resource) of
    (NoSlash, ResourceFormat NoFormat _) -> str
    (Slash, ResourceFormat NoFormat _) -> case str of "/" -> "/"; _ -> str ++ "/"
    (NoSlash, ResourceFormat (StaticFormat format) branch) -> case branch of
        Just {} -> str ++ "." ++ format
        Nothing -> str ++ "/." ++ format
    (Slash, ResourceFormat DynamicFormat Nothing) -> case dformat of
        Just format@(_:_) -> str ++ "/." ++ format
        _ -> str ++ "/"
    (NoSlash, ResourceFormat DynamicFormat _) -> case dformat of
        Just format@(_:_) -> str ++ "." ++ format
        _ -> str
    -- This case might be taken by TrailingBranch
    _ -> str

-----------------------
-- Converts the output of parseTrunkFormatAt to something consumable by a Resource
-- It forbids many things that parse correctly, most notably using formats in the middle of a path.
-- It's possible in theory to allow such things, but complicated.
-- Directories are really format-less things.
--
-- trunkToResource is a top-level call to weed out cases that don't make sense recursively in trunkToResource'
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
trunkToResource' ((DynamicBranch ddir, StaticFormat format):xs) = trunkToResource' ((DynamicBranch (ddir ++ "." ++ format), NoFormat):xs)
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
trunkAt arg = either trunkError reverse $ Parse.parse parseTrunkAt "Distribution.Server.Resource.parseTrunkAt" arg
    where trunkError pe = error $ "Distribution.Server.Resource.trunkAt: Could not parse trunk literal " ++ show arg ++ ". Parsec error: " ++ show pe

parseTrunkAt :: Parse.Parser [BranchComponent]
parseTrunkAt = do
    components <- Parse.many (Parse.try parseComponent)
    Parse.optional (Parse.char '/')
    Parse.eof
    return components
  where
    parseComponent = do
        void $ Parse.char '/'
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
        void $ Parse.char '/'
        Parse.choice $ map Parse.try
          [ Parse.char '.' >> Parse.many1 (Parse.char '.') >>
            ((Parse.lookAhead (Parse.char '/') >> return ()) Parse.<|> Parse.eof) >> return (TrailingBranch, NoFormat)
          , Parse.char ':' >> parseMaybeFormat DynamicBranch
          , do Parse.lookAhead (void $ Parse.satisfy (/=':'))
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
-- For a small curl-based testing mini-suite of [Resource]:
-- [res "/foo" ["json"], res "/foo/:bar.:format" ["html", "json"], res "/baz/test/.:format" ["html", "text", "json"], res "/package/:package/:tarball.tar.gz" ["tarball"], res "/a/:a/:b/" ["html", "json"], res "/mon/..." [""], res "/wiki/path.:format" [], res "/hi.:format" ["yaml", "blah"]]
--     where res field formats = (resourceAt field) { resourceGet = map (\format -> (format, \_ -> return . toResponse . (++"\n") . ((show format++" - ")++) . show)) formats }
serveResource :: [(Content, ServerErrorResponse)] -> Resource -> ServerResponse
serveResource errRes (Resource _ rget rput rpost rdelete rformat rend _) = \dpath -> msum $
    map (\func -> func dpath) $ methodPart ++ [optionPart]
  where
    optionPart = makeOptions $ concat [ met | ((_:_), met) <- zip methods methodsList]
    methodPart = [ serveResources met res | (res@(_:_), met) <- zip methods methodsList]
    methods = [rget, rput, rpost, rdelete]
    methodsList = [[GET, HEAD], [PUT], [POST], [DELETE]]
    makeOptions :: [Method] -> ServerResponse
    makeOptions methodList = \_ -> method OPTIONS >> nullDir >> do
        setHeaderM "Allow" (intercalate ", " . map show $ methodList)
        return $ toResponse ()
    -- some of the dpath lookup calls can be replaced by pattern matching the head/replacing
    -- at the moment, duplicate entries tend to be inserted in dpath, because old ones are not replaced
    -- Procedure:
    -- > Guard against method
    -- > Extract format/whatnot
    -- > Potentially redirect to canonical slash form
    -- > Go from format/content-type to ServerResponse to serve-}
    serveResources :: [Method] -> [(Content, ServerResponse)] -> ServerResponse
    serveResources met res dpath = case rend of
        Trailing -> method met >> (remainingPathString >>= \str ->
                                   serveContent res (("..", str):dpath))
        _ -> serveFormat res met dpath
    serveFormat :: [(Content, ServerResponse)] -> [Method] -> ServerResponse
    serveFormat res met = case rformat of
        ResourceFormat NoFormat Nothing -> \dpath -> method met >> nullDir >> serveContent res dpath
        ResourceFormat (StaticFormat format) Nothing -> \dpath -> path $ \format' -> method met >> nullDir >> do
            -- this branch shouldn't happen - /foo/.json would instead be stored as two static dirs
            guard (format' == ('.':format))
            serveContent res dpath
        ResourceFormat (StaticFormat format) (Just (StaticBranch sdir)) -> \dpath -> method met >> nullDir >>
            -- likewise, foo.json should be stored as a single static dir
            if lookup sdir dpath == Just (sdir ++ "." ++ format) then mzero
                                                                 else serveContent res dpath
        ResourceFormat (StaticFormat format) (Just (DynamicBranch sdir)) -> \dpath -> method met >> nullDir >>
            case matchExt format =<< lookup sdir dpath of
                Just pname -> serveContent res ((sdir, pname):dpath)
                Nothing -> mzero
        ResourceFormat DynamicFormat Nothing -> \dpath ->
            msum [ method met >> nullDir >> serveContent res dpath
                 , path $ \pname -> case pname of
                       ('.':format) -> method met >> nullDir >> serveContent res (("format", format):dpath)
                       _ -> mzero
                 ]
        ResourceFormat DynamicFormat (Just (StaticBranch sdir)) -> \dpath -> method met >> nullDir >>
            case fmap extractExt (lookup sdir dpath) of
                Just (pname, format) | pname == sdir -> serveContent res (("format", format):dpath)
                _ -> mzero
        ResourceFormat DynamicFormat (Just (DynamicBranch sdir)) -> \dpath -> method met >> nullDir >>
            -- this is somewhat complicated. consider /pkg-0.1 and /pkg-0.1.html. If the format is optional, where to split?
            -- the solution is to manually check the available formats to see if something matches
            -- if this situation comes up in practice, try to require a trailing slash, e.g. /pkg-0.1/.html
            case fmap (\sd -> (,) sd $ extractExt sd) (lookup sdir dpath) of
                Just (full, (pname, format)) -> do
                    let splitOption = serveContent res (("format", format):(sdir, pname):dpath)
                        fullOption  = serveContent res ((sdir, full):dpath)
                    case guard (not $ null format) >> lookup format res of
                        Nothing -> fullOption
                        Just {} -> splitOption
                _ -> mzero
        -- some invalid combination
        _ -> \dpath -> method met >> nullDir >> serveContent res dpath
    serveContent :: [(Content, ServerResponse)] -> ServerResponse
    serveContent res dpath = do
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
                Just answer -> handleErrors (Just format) $ answer dpath
                Nothing -> mzero -- return 404 if the specific format is not found
                  -- return default response when format is empty or non-existent
            _ -> do (format,answer) <- negotiateContent (head res) res
                    handleErrors (Just format) $ answer dpath

    handleErrors format =
      handleErrorResponse (serveErrorResponse errRes format)

    redirCanonicalSlash :: DynamicPath -> ServerPartE Response -> ServerPartE Response
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

resourceMethods :: Resource -> [Method]
resourceMethods (Resource _ rget rput rpost rdelete _ _ _) = [ met | ((_:_), met) <- zip methods methodsList]
  where methods = [rget, rput, rpost, rdelete]
        methodsList = [GET, PUT, POST, DELETE]

serveErrorResponse :: [(Content, ServerErrorResponse)] -> Maybe Content -> ServerErrorResponse
serveErrorResponse errRes mformat err = do
    -- So our strategy is to give priority to a content type requested by
    -- the client, then secondly any format implied by the requested url
    -- e.g. requesting a .html file, or html being default for the resource
    -- and finally just fall through to using text/plain
    (_, errHandler) <- negotiateContent ("", defHandler) errRes
    errHandler err
  where
    defHandler = fromMaybe throwError $ do
      format <- mformat
      lookup format errRes

negotiateContent :: (FilterMonad Response m, ServerMonad m)
                 => (Content, a) -> [(Content, a)] -> m (Content, a)
negotiateContent def available = do
    when (length available > 1) $
      setHeaderM "Vary" "Accept"
    maccept <- getHeaderM "Accept"
    case maccept of
      Nothing -> return def
      Just accept ->
        return $ fromMaybe def $ listToMaybe $ catMaybes
                   [ simpleContentTypeMapping ct
                       >>= \f -> find (\x -> fst x == f) available
                   | let acceptable = parseContentAccept (BS.unpack accept)
                   , ct <- acceptable ]
  where
    -- This is rather a non-extensible hack
    simpleContentTypeMapping ContentType {ctType, ctSubtype, ctParameters = []} =
      case (ctType, ctSubtype) of
        ("text",        "html")  -> Just "html"
        ("text",        "plain") -> Just "txt"
        ("application", "json")  -> Just "json"
        _                        -> Nothing
    simpleContentTypeMapping _    = Nothing

----------------------------------------------------------------------------

data ServerTree a = ServerTree {
    nodeResponse :: Maybe a,
    nodeForest :: Map BranchComponent (ServerTree a)
} deriving (Show)

instance Functor ServerTree where
    fmap func (ServerTree value forest) = ServerTree (fmap func value) (Map.map (fmap func) forest)

drawServerTree :: ServerTree a -> Maybe (a -> String) -> String
drawServerTree tree func = Tree.drawTree (transformTree tree Nothing)
  where transformTree (ServerTree res for) mlink = Tree.Node (drawLink mlink res) (map transformForest $ Map.toList for)
        drawLink mlink res = maybe "" ((++": ") . show) mlink ++ maybe "(nothing)" (fromMaybe (const "node") func) res
        transformForest (link, tree') = transformTree tree' (Just link)

serverTreeEmpty :: ServerTree a
serverTreeEmpty = ServerTree Nothing Map.empty

-- essentially a ReaderT DynamicPath ServerPart Response
-- this always renders parent URIs, but usually we guard against remaining path segments, so it's fine
renderServerTree :: DynamicPath -> ServerTree ServerResponse -> ServerPartE Response
renderServerTree dpath (ServerTree func forest) =
    msum $ maybeToList (fmap (\fun -> fun dpath) func)
        ++ map (uncurry renderBranch) (Map.toList forest)
  where
    renderBranch :: BranchComponent -> ServerTree ServerResponse -> ServerPartE Response
    renderBranch (StaticBranch  sdir) tree = dir sdir $ renderServerTree dpath tree
    renderBranch (DynamicBranch sdir) tree
      | (ddir, sformat@('.':_)) <- break (=='.') sdir
                   = path $ \pname -> do guard (sformat `isSuffixOf` pname)
                                         let pname' = take (length pname - length sformat) pname
                                         renderServerTree ((ddir, pname'):dpath) tree
      | otherwise  = path $ \pname -> renderServerTree ((sdir, pname):dpath) tree
    renderBranch TrailingBranch tree = renderServerTree dpath tree

reinsert :: Monoid a => BranchComponent -> ServerTree a -> Map BranchComponent (ServerTree a) -> Map BranchComponent (ServerTree a)
-- combine will only be called if branchMap already contains the key
reinsert key newTree branchMap = Map.insertWith combine key newTree branchMap

combine :: Monoid a => ServerTree a -> ServerTree a -> ServerTree a
combine (ServerTree newResponse newForest) (ServerTree oldResponse oldForest) =
    -- replace old resource with new resource, combine old and new responses
    ServerTree (mappend newResponse oldResponse) (Map.foldrWithKey reinsert oldForest newForest)

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

