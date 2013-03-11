{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.StaticFiles (
    initStaticFilesFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Text.XHtml.Strict (Html, toHtml, anchor, (<<), (!), href, paragraph)
import qualified Text.XHtml.Strict as XHtml

import System.FilePath

-- | A feature to provide the top level files on the site (using templates)
-- and also serve the genuinely static files.
--
initStaticFilesFeature :: ServerEnv
                       -> IO HackageFeature
initStaticFilesFeature env@ServerEnv{serverTemplatesDir} = do

  -- Page templates
  templates <- loadTemplates DesignMode {- use DesignMode when working on templates -}
                 [serverTemplatesDir]
                 ["index.html", "hackageErrorPage.txt", "hackageErrorPage.html"]

  let feature = staticFilesFeature env templates

  return feature


staticFilesFeature :: ServerEnv -> Templates -> HackageFeature
staticFilesFeature ServerEnv{serverStaticDir} templates =
  (emptyHackageFeature "static-files") {
    featureResources =
      [ (resourceAt "/") {
            resourceGet  = [("", \_ -> serveStaticIndexTemplate)]
          }
-- TODO: we currently cannot use /.. here because then we cannot use it for
-- the legacy redirects feature.
--      , (resourceAt "/..") {
--            resourceGet  = [("", \_ -> serveStaticTemplates)]
--          }
      , (resourceAt "/static/..") {
            resourceGet  = [("", \_ -> serveStaticDirFiles)]
          }
      ] ++
      [ (resourceAt ("/" ++ filename)) {
            resourceGet  = [("", \_ -> serveStaticToplevelFile mimetype filename)]
          }
      | (filename, mimetype) <- toplevelFiles ]
        ++
      [ (resourceAt ("/" ++ dropExtension name)) {
            resourceGet  = [("", \_ -> serveStaticTemplate name)]
          }
      | name <- toplevelTemplates ]
  , featureState = []
  , featureErrHandlers = [("txt",  textErrorPage)
                         ,("html", htmlErrorPage)]
  }

  where
    serveStaticDirFiles :: ServerPartE Response
    serveStaticDirFiles =
      serveDirectory DisableBrowsing [] serverStaticDir

    serveStaticToplevelFile :: String -> FilePath -> ServerPartE Response
    serveStaticToplevelFile mimetype filename =
      serveFile (asContentType mimetype) (serverStaticDir </> filename)

    toplevelFiles = [("favicon.ico", "image/x-icon")]

-- TODO: we currently have to list the templates explicitly, rather than
-- just discovering them, see above
    toplevelTemplates = ["accounts.html", "admin.html", "upload.html"
                        ,"account-upgrade.html"]

    serveStaticTemplate = serveTemplate

--    serveStaticTemplates :: ServerPartE Response
--    serveStaticTemplates =
--      path $ \name -> do
--        nullDir
--        noTrailingSlash --TODO: redirect to non-slash version
--        serveTemplate (name ++ ".html")

    serveStaticIndexTemplate :: ServerPartE Response
    serveStaticIndexTemplate =
      serveTemplate "index.html"

    serveTemplate :: String -> ServerPartE Response
    serveTemplate name = do
      mtemplate <- tryGetTemplate templates name
      case mtemplate of
        Nothing       -> mzero
        Just template -> ok $ toResponse $ template []

    textErrorPage (ErrorResponse errCode errTitle message) = do
        template <- getTemplate templates "hackageErrorPage.txt"
        let formattedMessage = messageToText message
        resp errCode $ toResponse $ template
          [ "errorTitle"   $= errTitle
          , "errorMessage" $= formattedMessage
          ]

    htmlErrorPage :: ErrorResponse -> ServerPartE Response
    htmlErrorPage (ErrorResponse errCode errTitle message) = do
        template <- getTemplate templates "hackageErrorPage.html"
        let formattedMessage = paragraph << errorToHtml message
        resp errCode $ toResponse $ template
          [ "errorTitle"   $= errTitle
          , "errorMessage" $= XHtml.renderHtml formattedMessage
          ]

errorToHtml :: [MessageSpan] -> [Html]
errorToHtml []               = []
errorToHtml (MText x    :xs) = toHtml x: errorToHtml xs
errorToHtml (MLink x url:xs) = (anchor ! [href url] << x): errorToHtml xs
