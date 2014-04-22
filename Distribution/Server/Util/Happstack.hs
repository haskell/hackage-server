
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Util.Happstack (
    htmlFormWrapperHack,
    rqRealMethod,

    remainingPath,
    remainingPathString,
    mime,
    consumeRequestBody,

    uriEscape
  ) where

import Happstack.Server
import Happstack.Server.Internal.Monads
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as BS
import qualified Network.URI as URI
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)


import Debug.Trace

-- | There is a bit of a conflict in the design of a REST api due to the need
-- to support both web browsers and machine clients using machine formats like
-- JSON.
--
-- For machine clients we want a nice set of seaprate resources using JSON and
-- GET\/PUT\/POST\/DELETE.
--
-- For web browsers we want nice pages that aggregate multiple aspects and that
-- use only POST with @multipart\/form-data@. One problem is that these use
-- different resource types: browsers want HTML and form-data, while other
-- clients want JSON or other machine formats. Another problem is that humans
-- want nice pages that aggregate multuple bits into a single resouce, while
-- machine clients want separate single-purpose resources. In addition, the
-- stage changes from resource to resource when doing updates is different.
-- A web browser wants to POST to modify some data and then see either the new
-- data (again an aggregate human-oriented page) or to see an updated view of
-- the same page, while the other clients simply want to know if the update was
-- successful and if it's a new resource then where it is.
--
-- Essentially we have two parts to the system: a web-based database in a nice
-- REST API style, and a separate human oriented website. We want both without
-- having to duplicate all the work. We can get much of the way there for GET
-- because we can simply have certain resources aimed only at humans, or only
-- at machines or in the overlap we can use content negotiation. For updates
-- though the clash is much worse: the classic HTML forms cannot make use of
-- a nice REST API.
--
-- There are a few solutions:
--
-- * make lots of resources accept POST and produce html results. This involves
--   many resources being duplicated in the html feature. This is the way
--   much of the server works now. It's ugly and duplicates things (or we miss
--   the human or machine side).
-- * use Ajax and have the web browser use the REST API directly. This is
--   actually rather attractive. The main REST API can be designed just for
--   machines and the web UI lives just in HTML pages that do all the PUT
--   DELETE etc directly.
-- * wrappers or workarounds for accessing the machine REST API via html forms
--   and POST. This involes the framework translating special form POST
--   requests into internal requests that are handled as if the were the simple
--   direct machine REST API.
--
-- The second option is certainly doable, though there is some prejudice
-- against JS amongst some of our userbase. It also requires our developers
-- to get familiar with JS & JS tech, e.g. jquery. Also, it doesn't have to be
-- all or nothing, can do it incrementally. This is a good longer term option.
--
-- Both options two and three meet the goal of letting most of the server
-- features handle only a simple machine REST API, reducing code duplication
-- and improving coverage of resources in machine formats.
--
-- This code implements the third option. We do a few translation steps, both
-- on the request and on the response. On the request side we deal with the
-- fact that HTML (and even HTML 5) can only perform POST requests, not PUT
-- or DELETE. We also deal with translating HTML @multipart\/form-data@ into
-- simple JSON. On the response side, we deal with the fact that an HTML page
-- that does a request wants to return to another HTML page, perhaps the same
-- one or a different one, while for the simple machine REST API the response
-- if often a simple acknoledgement.
--
-- To indicate that these translation wrappers are required we use a hidden
-- fields in the form. We use a @_method@ field to indicate that the POST
-- request should be translated. For the return location we use a @_return@
-- field.
--
htmlFormWrapperHack :: MonadIO m => ServerPartT m a -> ServerPartT m a
htmlFormWrapperHack rest = do
  withDataFn (look "_method") $ \mthdStr -> do
      case mthdStr of
        -- simple wrappers, just translate the method
        "PUT"    -> setMethod PUT rest
        "DELETE" -> setMethod DELETE rest
        -- new full wrappers
        "wrap-PUT-form"      -> do setRedirect;                 setMethod PUT rest
        "wrap-PUT-form2json" -> do setRedirect; formDataToJSON; setMethod PUT rest
        "wrap-DELETE"        -> do setRedirect;                 setMethod DELETE rest
        _                    -> rest
  where
    setMethod m action = localRq (\req -> req { rqMethod = m }) action
    setRedirect        = withDataFn (look "_return") $ \loc ->
                           composeFilter (redirectReturnLocation loc)
    formDataToJSON = do
      req <- askRq
      jsonval <- requestFormDataAsJSON
      putRequestBody req (Body (JSON.encode jsonval))

    redirectReturnLocation returnLocation rsp@Response { rsCode = code }
      | code == 200 || code == 201 || code == 204
                                 = redirect 303 returnLocation rsp
    redirectReturnLocation _ rsp = rsp

-- | Very simple translation from form-data key value pairs to a single JSON
-- object with equivalent field names and string values.
--    
requestFormDataAsJSON :: MonadIO m => ServerPartT m JSON.Value
requestFormDataAsJSON = do
    (_, mbody, _) <- askRqEnv
    return $ JSON.object
      [ (T.pack k, JSON.toJSON v)
      | (k, Input { inputValue = Right v }) <- fromMaybe [] mbody ]

putRequestBody :: MonadIO m => Request -> RqBody -> m ()
putRequestBody req = liftIO . putMVar (rqBody req)

-- | For use with 'htmlFormWrapperHack': tries to report the original method
-- of a request before the hack was applied.
rqRealMethod :: Request -> Method
-- We want to look in the post data to find out if the method has been
-- changed. But if the method has been changed to something other than
-- POST or PUT, Happstack doesn't return any post data at all. So we
-- set the method to POST temporarily before checking the post
-- parameter.
rqRealMethod rq = fromMaybe (rqMethod rq) $ unsafePerformIO $ runServerPartT_hack rq { rqMethod = POST } $
    withDataFn (liftM (not . null) $ lookInputs "_method") $ \mthd_exists ->
      return $ if mthd_exists then POST else rqMethod rq

runServerPartT_hack :: Monad m => Request -> ServerPartT m a -> m (Maybe a)
runServerPartT_hack rq mx
  = liftM (\res -> case res of
                     Nothing           -> Nothing
                     Just (Left _,  _) -> Nothing
                     Just (Right x, _) -> Just x)
          (ununWebT (runReaderT (unServerPartT mx) rq))


-- |Passes a list of remaining path segments in the URL. Does not
-- include the query string. This call only fails if the passed in
-- handler fails.
remainingPath :: Monad m => ([String] -> ServerPartT m a) -> ServerPartT m a
remainingPath handle = do
    rq <- askRq
    localRq (\newRq -> newRq{rqPaths=[]}) $ handle (rqPaths rq)

-- | Gets the string without altering the request.
remainingPathString :: Monad m => ServerPartT m String
remainingPathString = do
    strs <- liftM rqPaths askRq
    return $ if null strs then "" else foldr1 (</>) . map uriEscape $ strs

-- This disappeared from happstack in 7.1.7
uriEscape :: String -> String
uriEscape = URI.escapeURIString URI.isAllowedInURI

-- |Returns a mime-type string based on the extension of the passed in
-- file.
mime :: FilePath -> String
mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes


-- | Get the raw body of a PUT or POST request.
--
-- Note that for performance reasons, this consumes the data and it cannot be
-- called twice.
--
consumeRequestBody :: Happstack m => m BS.ByteString
consumeRequestBody = do
    mRq <- takeRequestBody =<< askRq
    case mRq of
      Nothing -> escape $ internalServerError $ toResponse
                   "consumeRequestBody cannot be called more than once."
      Just (Body b) -> return b
