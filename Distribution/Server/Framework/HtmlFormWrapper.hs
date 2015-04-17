{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Framework.HtmlFormWrapper (
    htmlFormWrapperHack,
    rqRealMethod,
  ) where

import Happstack.Server
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMap
import Control.Concurrent.MVar

import Distribution.Server.Framework.HappstackUtils (showContentType)


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
htmlFormWrapperHack :: (Functor m, MonadIO m, MonadPlus m) => ServerPartT m Response -> ServerPartT m Response
htmlFormWrapperHack rest = do
    res <- getDataFn $ body $
      (,,) <$> optional (do m <- look "_method"
                            case m of
                              "PUT"    -> return PUT
                              "POST"   -> return POST
                              "DELETE" -> return DELETE
                              _        -> mzero)
           <*> optional (look "_return")
           <*> optional (look "_transform")
    case res of
      Right (Just mthd, mreturnLoc, mtransform)
        -> setRedirect mreturnLoc $
             doTransform mtransform $
               setMethod mthd rest
      _ -> mzero
  where
    setMethod m = localRq (\req -> setHeader "_method" "" req { rqMethod = m })

    setRedirect Nothing               action = action
    setRedirect (Just returnLocation) action = do
      rsp <- action
      composeFilter (redirectReturnLocation returnLocation)
      return rsp

    redirectReturnLocation returnLocation rsp@Response { rsCode = code }
      | code == 200 || code == 201 || code == 204
                                 = redirect 303 returnLocation rsp
    redirectReturnLocation _ rsp = rsp

    doTransform Nothing            action = action
    doTransform (Just "form2json") action = formDataToJSON action
    doTransform (Just "file2raw")  action = fileUploadToRaw action
    doTransform (Just other)      _action = escape $ badRequest $ toResponse $
                                            "Unknown _transform value " ++ other

    formDataToJSON action = do
      mjsonval <- requestFormDataAsJSON
      case mjsonval of
        Right jsonval -> do
          putRequestBody (JSON.encode jsonval)
          localRq (setHeader "Content-Type" "application/json") action
        Left intermediate ->
          escape $ badRequest $
            toResponse $
              "The form-data is not in the expected syntax for the form2json transform:\n\n"
              ++ unlines (map show intermediate)

    fileUploadToRaw action = do
        (filePath, _name, contentType) <- body $ lookFile "_file"
        putRequestBody =<< liftIO (BS.readFile filePath)
        localRq (setHeader "Content-Type" (showContentType contentType)) action

-- | Very simple translation from form-data key value pairs to a single JSON
-- object with equivalent field names and string values.
--
requestFormDataAsJSON :: (MonadIO m, MonadPlus m) => ServerPartT m (Either [((String, BS.ByteString), JPath)]
                                                            JSON.Value)
requestFormDataAsJSON = do
    (_, mbody, _) <- askRqEnv
    let keyvals = [ (k, v)
                  | (k, Input { inputValue = Right v }) <- fromMaybe [] mbody
                  , case k of '_':_ -> False; _ -> True ]
        paths   = [ (parsePathTmpl (BS8.unpack v) k)
                  | (k, v) <- keyvals ]
    case accumJPaths paths of
      Nothing -> return $ Left (zip keyvals paths )
      Just jv -> return $ Right jv

data JPath = JField T.Text JPath | JVal JSON.Value | JPathErr
  deriving Show

parsePathTmpl :: String -> String -> JPath
parsePathTmpl v = parseKey
  where
    parseKey s =
      case break (\c -> c == '.' || c == '=') s of
        ("%f",    '.':r)             -> JField (T.pack v) (parseKey r)
        (c@(_:_), '.':r)             -> JField (T.pack c) (parseKey r)
        ("%f",    '=':r)             -> JField (T.pack v) (parseVal r)
        (c@(_:_), '=':r)             -> JField (T.pack c) (parseVal r)
        _                            -> JPathErr
    parseVal "%s"                         = JVal (JSON.String (T.pack v))
    parseVal "%n" | [(n,"")] <- reads v   = JVal (JSON.Number (fromIntegral (n :: Int)))
    parseVal "%v" | Just j <- parseJVal v = JVal j
    parseVal s    | Just j <- parseJVal s = JVal j
    parseVal _                            = JPathErr

    parseJVal "true"                     = Just (JSON.Bool True)
    parseJVal "false"                    = Just (JSON.Bool False)
    parseJVal "null"                     = Just  JSON.Null
    parseJVal s | [(str,"")] <- reads s  = Just (JSON.String (T.pack str))
    parseJVal s | [(n,  "")] <- reads s  = Just (JSON.Number (fromIntegral (n :: Int)))
    parseJVal s                          = JSON.decode (BS8.pack s)

accumJPaths :: [JPath] -> Maybe JSON.Value
accumJPaths js = foldr (\j r v -> case insertJPath j v of
                                    Nothing -> Nothing
                                    Just v' -> r v') Just js JSON.Null

insertJPath :: JPath -> JSON.Value -> Maybe JSON.Value
insertJPath (JField f p) JSON.Null = do
  v <- insertJPath p JSON.Null
  return (JSON.object [(f, v)])

insertJPath (JField f p) (JSON.Object obj) = do
  case HMap.lookup f obj of
    Nothing -> do
      v <- insertJPath p JSON.Null
      return (JSON.Object (HMap.insert f v obj))
    Just v0 -> do
      v <- insertJPath p v0
      return (JSON.Object (HMap.insert f v obj))

insertJPath (JVal v) JSON.Null = return v
insertJPath _        _         = Nothing

-- | Replace the request body with something else.
putRequestBody :: (ServerMonad m, MonadIO m) => BS.ByteString -> m ()
putRequestBody newBody = do
    req <- askRq
    liftIO $ putMVar (rqBody req) (Body newBody)

-- | For use with 'htmlFormWrapperHack': tries to report the original method
-- of a request before the hack was applied.
rqRealMethod :: Request -> Method
rqRealMethod rq | Just _ <- getHeader "_method" rq = POST
                | otherwise                        = rqMethod rq
