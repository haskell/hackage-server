{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Features.ServerApiDoc (
    serverApiDocFeature
  ) where

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import Distribution.Server.Pages.Template (hackagePage)

import Text.XHtml.Strict
         ( Html, (+++), concatHtml, noHtml, toHtml, (<<)
         , h2, h3, p, tt, emphasize, br
         , anchor, (!), href, name
         , unordList )
import Text.JSON
         ( JSValue(..), toJSObject, toJSString )
import Data.List

-- | A feature to let people find out what the API of this hackage server
-- instance is. It lists all the active features and all the resources they
-- serve, including what methods and formats they support.
--
-- This should make things more obvious to people writing clients.
--
serverApiDocFeature :: [HackageFeature] -> HackageFeature
serverApiDocFeature serverFeatures = (emptyHackageFeature "serverapi") {
    featureResources =
      [ (resourceAt "/api.:format") {
           resourceGet = [("html", \_ -> serveApiDocHtml serverFeatures)
                         ,("json", \_ -> serveApiDocJSON serverFeatures)]
        }
      ]
}

serveApiDocHtml :: [HackageFeature] -> ServerPart Response
serveApiDocHtml = return . toResponse . Resource.XHtml . apiDocPageHtml

serveApiDocJSON :: [HackageFeature] -> ServerPart Response
serveApiDocJSON = return . toResponse . Resource.JSON . apiDocJSON


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
        [ anchor ! [ name (featureName feature) ] << h3 << featureName feature
          +++ p << featureDescription feature
          +++ resourceList feature
        | feature <- serverFeatures ]

    resourceList feature =
      unordList
        [     renderLocationTemplate resource
          +++ p << renderResourceDescription (resourceDescription resource)
          +++ methodList resource
        | resource <- featureResources feature ]

    methodList resource =
      unordList
        [ show httpMethod +++ ": " +++ formatList formats
        | (httpMethod, formats) <- resourceMethodsAndFormats resource ]

    formatList formats =
      intersperse (toHtml ", ")
        [ tt << format | format <- formats ]

    renderResourceDescription :: [String] -> Html
    renderResourceDescription desc = p << concatHtml (map (+++ br) desc)

    renderLocationTemplate :: Resource -> Html
    renderLocationTemplate resource =
        tt << (renderComponents pathComponents
           +++ renderTrailer (resourceFormat resource) (resourcePathEnd resource))
      where
        pathComponents = reverse (resourceLocation resource)

        renderComponents (StaticBranch  sdir:cs) = "/" +++ sdir
                                                       +++ renderComponents cs
        renderComponents (DynamicBranch leaf:[])
          | ResourceFormat _ (Just (StaticBranch _)) <- resourceFormat resource
                                                 = "/" +++ leaf
        renderComponents (DynamicBranch ddir:cs) = "/" +++ emphasize << ("{" ++ ddir ++ "}")
                                                       +++ renderComponents cs
        renderComponents (TrailingBranch    :_ ) = emphasize << "*"
        renderComponents []                      = noHtml

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""

resourceMethodsAndFormats :: Resource -> [(Method, [String])]
resourceMethodsAndFormats (Resource _ rget rput rpost rdelete _ _ _) =
    [ (httpMethod, [ formatName | (formatName, _) <- handlers ])
    | (handlers@(_:_), httpMethod) <- zip methodsHandlers methodsKinds ]
  where
    methodsHandlers = [rget, rput, rpost, rdelete]
    methodsKinds    = [GET,  PUT,  POST,  DELETE]



apiDocJSON :: [HackageFeature] -> JSValue
apiDocJSON serverFeatures = featureList
  where
    featureList =
      JSArray
        [ JSObject $ toJSObject
            [ ("feature", JSString $ toJSString $ featureName feature)
            , ("resources", resourceList feature) ]
        | feature <- serverFeatures ]

    resourceList :: HackageFeature -> JSValue
    resourceList feature =
      JSArray
        [ JSObject $ toJSObject
            [ ("location", JSString $ toJSString $ renderLocationTemplate resource)
            , ("methods", methodList resource) ]
        | resource <- featureResources feature ]

    methodList :: Resource -> JSValue
    methodList resource =
      JSArray
        [ JSObject $ toJSObject
            [ ("method", JSString $ toJSString $ show httpMethod)
            , ("formats", formatList formats) ]
        | (httpMethod, formats) <- resourceMethodsAndFormats resource ]

    formatList formats =
      JSArray
        [ JSObject $ toJSObject
            [ ("name", JSString (toJSString format))
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
        renderComponents (DynamicBranch ddir:cs) = "/" ++ "{" ++ ddir ++ "}"
                                                       ++ renderComponents cs
        renderComponents (TrailingBranch    :_ ) = "*"
        renderComponents []                      = ""

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""
