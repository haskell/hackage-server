{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Features.ServerApiDoc (
    serverApiDocFeature
  ) where

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import Distribution.Server.Pages.Template (hackagePage)

import Text.XHtml.Strict
         ( Html, (+++), concatHtml, noHtml, toHtml, (<<)
         , h2, h3, p, tt, emphasize
         , blockquote, thespan, thestyle
         , anchor, (!), href, name
         , unordList, defList )
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
    featureDesc = "Lists the resources available on this server."
  , featureResources =
      [ (resourceAt "/api.:format") {
            resourceDesc = [ (GET, "This page") ]
          , resourceGet  = [ ("html", \_ -> serveApiDocHtml serverFeatures)
                           , ("json", \_ -> serveApiDocJSON serverFeatures)
                           ]
          }
      ]
  , featureState = []
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
        tt << (renderComponents pathComponents
           +++ renderTrailer (resourceFormat resource) (resourcePathEnd resource))
      where
        pathComponents = reverse (resourceLocation resource)

        renderComponents (StaticBranch  sdir:cs) = "/" +++ sdir
                                                       +++ renderComponents cs
        renderComponents (DynamicBranch leaf:[])
          | ResourceFormat _ (Just (StaticBranch _)) <- resourceFormat resource
                                                 = "/" +++ leaf
        renderComponents (DynamicBranch ddir:cs) = "/" +++ emphasize << (":" ++ ddir)
                                                       +++ renderComponents cs
        renderComponents (TrailingBranch    :_ ) = emphasize << "*"
        renderComponents []                      = noHtml

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""

    extensionsElsewhere feature resource =
        if null matchingResources
          then mempty
          else p << "Methods defined elsewhere:"
               +++ blockquote << defList (renderMatching matchingResources)
      where
        renderMatching :: [(HackageFeature, Resource)] -> [(Html, Html)]
        renderMatching = map $ \(f, r) ->
          (toHtml $ featureName f, renderResourceWithExtensions Nothing r)

        matchingResources :: [(HackageFeature, Resource)]
        matchingResources = [
            (feature', resource')
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
        | (httpMethod, formats, _) <- resourceMethodsAndFormats resource ]

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
        renderComponents (DynamicBranch ddir:cs) = "/" ++ ":" ++ ddir
                                                       ++ renderComponents cs
        renderComponents (TrailingBranch    :_ ) = "*"
        renderComponents []                      = ""

        renderTrailer (ResourceFormat (StaticFormat ext) _) _ = "." ++ ext
        renderTrailer _ Slash                                 = "/"
        renderTrailer _ _                                     = ""
