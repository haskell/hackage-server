{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Features.ServerIntrospect (
    serverIntrospectFeature
  ) where

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Pages.Template (hackagePage)

import Text.XHtml.Strict
         ( Html, URL, (+++), concatHtml, noHtml, toHtml, (<<)
         , h2, h3, h4, p, tt, emphasize, bold, primHtmlChar
         , blockquote, thespan, thestyle
         , anchor, (!), href, name
         , ordList, unordList )
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text
import Data.List
import Data.Function (on)
import Control.Arrow (first)

-- | A feature to serve information and status about the server itself.
-- It has access to all the other features on the server so can do some
-- amount of introspection.
--
-- In particular it provides:
--
-- * API introspection: let people find out what the API of this hackage server
-- instance is. It lists all the active features and all the resources they
-- serve, including what methods and formats they support.
-- This should make things more obvious to people writing clients.
--
-- * Memory consumption by each of the features
--
serverIntrospectFeature :: [HackageFeature] -> HackageFeature
serverIntrospectFeature serverFeatures = (emptyHackageFeature "serverapi") {
    featureDesc = "Lists the resources available on this server."
  , featureResources =
      [ (resourceAt "/api.:format") {
            resourceDesc = [ (GET, "This page") ]
          , resourceGet  = [ ("html", \_ -> serveApiDocHtml serverFeatures)
                           , ("json", \_ -> serveApiDocJSON serverFeatures)
                           ]
          }
      , (resourceAt "/server-status/memory.:format") {
            resourceDesc = [ (GET, "Server memory usage") ]
          , resourceGet  = [ ("html", \_ -> serveMemSizeHtml serverFeatures)
                           ]
          }
      ]
  , featureState = []
  }

-------------------
-- Server API stuff
--

serveApiDocHtml :: [HackageFeature] -> ServerPartE Response
serveApiDocHtml = return . toResponse . Resource.XHtml . apiDocPageHtml

serveApiDocJSON :: [HackageFeature] -> ServerPartE Response
serveApiDocJSON = return . toResponse . apiDocJSON


-- TODO: all-resources from all features combined
--       and also for the JSON that's all we really want.

apiDocPageHtml :: [HackageFeature] -> Html
apiDocPageHtml serverFeatures = hackagePage title content
  where
    title = "Server API"
    descr = "This page lists all of the resources available on this server. "
         ++ "The same list is also available in machine readable formats "
         ++ "(see link below)."
    content = [ h2 << title
              , p  << descr
              , featureLinks
              , featureList ]

    featureLinks =
      h3 << "Enabled server features" +++
      unordList
        [ anchor ! [ href ('#' : featureName feature) ] << featureName feature
        | feature <- serverFeatures ]

    featureList =
      concatHtml
        [ anchor ! [ name (featureName feature) ] << h3 << ("The " +++ featureName feature +++ " feature")
          +++ p << (let desc = featureDesc feature
                    in if null desc then thespan ! [thestyle "color: red"] << "Feature description unavailable"
                                    else toHtml desc)
          +++ stateList feature
          +++ resourceList feature
        | feature <- serverFeatures ]

    stateList feature =
      let states = map abstractStateDesc (featureState feature) in
      if null states
        then     p << "This feature does not have any state."
        else     p << emphasize << "State"
             +++ unordList states

    resourceList feature =
          p << emphasize << "Resources"
      +++ unordList [
                  renderLocationTemplate resource
              +++ renderResourceWithExtensions (Just feature) resource
            | resource <- featureResources feature
            ]

    renderResourceWithExtensions mFeature resource =
            methodList resource
        +++ case mFeature of
               Nothing      -> mempty
               Just feature -> extensionsElsewhere feature resource

    methodList resource =
      unordList
        [ show httpMethod +++ ": " +++ formatList formats +++ " -- " +++ description
        | (httpMethod, formats, description) <- resourceMethodsAndFormats resource ]

    formatList formats =
      intersperse (toHtml ", ")
        [ tt << format | format <- formats ]

    renderLocationTemplate :: Resource -> Html
    renderLocationTemplate resource =
        let (components, mUrl) = renderComponents pathComponents
            trailer            = renderTrailer (resourceFormat resource) (resourcePathEnd resource)
        in tt << case mUrl of
             Nothing  -> components +++ trailer
             Just url -> anchor ! [href (url ++ trailer)] << (components +++ trailer)
      where
        pathComponents = reverse (resourceLocation resource)

        renderComponents :: [BranchComponent] -> (Html, Maybe URL)
        renderComponents (StaticBranch sdir:cs) =
          let (rest, url) = renderComponents cs
          in ("/" +++ sdir +++ rest, liftM (("/" ++ sdir) ++) url)
        renderComponents (DynamicBranch leaf:[])
          | ResourceFormat _ (Just (StaticBranch _)) <- resourceFormat resource =
              ("/" +++ leaf, Just ("/" ++ leaf))
        renderComponents (DynamicBranch ddir:cs) =
          let (rest, _) = renderComponents cs
          in ("/" +++ emphasize << (":" ++ ddir) +++ rest, Nothing)
        renderComponents (TrailingBranch :_ ) =
          (emphasize << "/..", Nothing)
        renderComponents [] =
          (noHtml, Just "")

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""

    extensionsElsewhere feature resource =
        if null matchingResources
          then mempty
          else p << "Methods defined in other features:"
               +++ blockquote << matchingResources
      where
        matchingResources :: [Html]
        matchingResources =
          [ toHtml [ p << ("In the " +++ featureName feature' +++ " feature")
                   , p << renderResourceWithExtensions Nothing resource' ]
          | feature'  <- serverFeatures
          , featureName feature' /= featureName feature
          , resource' <- featureResources feature'
          , resourceLocation resource' == resourceLocation resource
          , not (null (resourceMethods resource'))
          ]

resourceMethodsAndFormats :: Resource -> [(Method, [String], Html)]
resourceMethodsAndFormats (Resource _ rget rput rpost rdelete _ _ desc) =
    [ (httpMethod, [ formatName | (formatName, _) <- handlers ], descriptionFor httpMethod desc)
    | (handlers@(_:_), httpMethod) <- zip methodsHandlers methodsKinds ]
  where
    methodsHandlers = [rget, rput, rpost, rdelete]
    methodsKinds    = [GET,  PUT,  POST,  DELETE]

    descriptionFor :: Method -> [(Method, String)] -> Html
    descriptionFor _ [] = thespan ! [thestyle "color: red"] << "Method description unavailable"
    descriptionFor m ((m',d):ds)
      | m == m'   = toHtml d
      | otherwise = descriptionFor m ds

apiDocJSON :: [HackageFeature] -> Value
apiDocJSON serverFeatures = featureList
  where
    featureList =
      array
        [ object
            [ ("feature", string $ featureName feature)
            , ("resources", resourceList feature) ]
        | feature <- serverFeatures ]

    resourceList :: HackageFeature -> Value
    resourceList feature =
      array
        [ object
            [ ("location", string $ renderLocationTemplate resource)
            , ("methods", methodList resource) ]
        | resource <- featureResources feature ]

    methodList :: Resource -> Value
    methodList resource =
      array
        [ object
            [ ("method", string $ show httpMethod)
            , ("formats", formatList formats) ]
        | (httpMethod, formats, _) <- resourceMethodsAndFormats resource ]

    formatList formats =
      array
        [ object
            [ ("name", string format)
            ] -- could add here ("mimetype", ...)
        | format <- formats ]

    renderLocationTemplate :: Resource -> String
    renderLocationTemplate resource =
           renderComponents pathComponents
        ++ renderTrailer (resourceFormat resource) (resourcePathEnd resource)
      where
        pathComponents = reverse (resourceLocation resource)

        renderComponents (StaticBranch  sdir:cs) = "/" ++ sdir
                                                       ++ renderComponents cs
        renderComponents (DynamicBranch leaf:[])
          | ResourceFormat _ (Just (StaticBranch _)) <- resourceFormat resource
                                                 = "/" ++ leaf
        renderComponents (DynamicBranch ddir:cs) = "/" ++ ":" ++ ddir
                                                       ++ renderComponents cs
        renderComponents (TrailingBranch    :_ ) = "/.."
        renderComponents []                      = ""

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""

----------------------
-- Memory consumption
--

serveMemSizeHtml :: [HackageFeature] -> ServerPartE Response
serveMemSizeHtml serverFeatures =
      toResponse
    . Resource.XHtml
    . memSizePageHtml
  <$> liftIO (mapM getFeatureSizes serverFeatures)
  where
    getFeatureSizes feature =
      (,,,) <$> pure (featureName feature)
            <*> pure (featureDesc feature)
            <*> mapM getCanonicalStateSizes (featureState feature)
            <*> mapM getCacheStateSizes     (featureCaches feature)

    getCanonicalStateSizes component =
      (,)   <$> pure (abstractStateDesc component)
            <*> abstractStateSize component

    getCacheStateSizes component =
      (,)   <$> pure (cacheDesc component)
            <*> getCacheMemSize component

memSizePageHtml :: [(String, String, [(String, Int)], [(String, Int)])] -> Html
memSizePageHtml featureStateSizes =
    hackagePage title content
  where
    title = "Server memory use"
    descr = "This page lists the memory use all of the in-memory data stores "
         ++ "and caches on the server."
    content = [ h2 << title
              , p  << descr
              , sectionLinks
              , totalSection
              , bySizeList
              , byFeatureList
              ]

    sectionLinks =
      h3 << "Contents" +++
      unordList
        [ anchor ! [ href ('#' : ref) ] << section
        | (section, ref) <- [("Total",      "total")
                            ,("By size",    "by-size")
                            ,("By feature", "by-feature")] ]

    orderedStateSizes = sortBy (flip compare `on` (\(x,_,_,_) -> x))
      [ (sz, isCanonical, cname, fname)
      | (fname, _, canonical, caches) <- featureStateSizes
      , ((cname, sz), isCanonical) <- zip canonical (repeat True)
                                   ++ zip caches    (repeat False) ]

    totalSize = sum [ sz | (sz,_,_,_) <- orderedStateSizes ]

    totalSection =
      h3 << (anchor ! [ name "total" ] << "Total")
        +++ p << "Total memory use of all state components and all caches:"
        +++ unordList [ show (memSizeMb totalSize) ++ "MB" ]
        +++ p << ("Note that the real heap size is usually 2x-3x greater than "
              ++ "this due to the way the GC works.")

    bySizeList =
      h3 << (anchor ! [ name "by-size" ] << "By size") +++
      ordList
        [ bold << (show (memSizeMb sz) ++ " MB: ")
          +++ cname +++ " " +++ primHtmlChar "mdash" +++ " "
           ++ (if isCanonical then "state component" else "cache")
           ++ " from feature "
          +++ anchor ! [ href ('#' : fname) ] << fname
        | (sz, isCanonical, cname, fname) <- orderedStateSizes ]


    byFeatureList =
      h3 << (anchor ! [ name "by-feature" ] << "By feature") +++
      concatHtml
        [ anchor ! [ name fname ] << h4 << fname
          +++ p << fdesc
          +++ stateList "state"  "State components:" states
          +++ stateList "caches" "Cache components:" caches
        | (fname, fdesc, states, caches) <- featureStateSizes ]

    stateList thing1 thing2 states =
      if null states
        then p << ("This feature does not have any " ++ thing1 ++ ".")
        else p << emphasize << thing2
              +++ unordList [ bold << (show (memSizeMb sz) ++ " MB: ")
                                  +++ cname
                            | (cname, sz) <- states ]

{------------------------------------------------------------------------------
  Some aeson auxiliary functions
------------------------------------------------------------------------------}

array :: [Value] -> Value
array = Array . Vector.fromList

object :: [(String, Value)] -> Value
object = Object . HashMap.fromList . map (first Text.pack)

string :: String -> Value
string = String . Text.pack
