{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.StaticFiles (
    initStaticFilesFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

-- | A feature to provide the top level files on the site (using templates)
-- and also serve the genuinely static files.
--
initStaticFilesFeature :: ServerEnv
                       -> IO HackageFeature
initStaticFilesFeature env@ServerEnv{serverTemplatesDir} = do

  -- Page templates
  templates <- loadTemplates DesignMode {- use DesignMode when working on templates -}
                 [serverTemplatesDir] ["index.html"]

  let feature = staticFilesFeature env templates

  return feature


staticFilesFeature :: ServerEnv -> Templates -> HackageFeature
staticFilesFeature ServerEnv{serverStaticDir} templates =
  (emptyHackageFeature "static-files") {
    featureResources =
      [ (resourceAt "/") {
            resourceGet  = [("", \_ -> serveStaticIndexTemplate)]
          }
      , (resourceAt "/..") {
            resourceGet  = [("", \_ -> serveStaticTemplates)]
          }
      , (resourceAt "/static/..") {
            resourceGet  = [("", \_ -> serveStaticDirFiles)]
          }
      ] ++
      [ (resourceAt ("/" ++ filename)) {
            resourceGet  = [("", \_ -> serveStaticToplevelFile mimetype filename)]
          }
      | (filename, mimetype) <- toplevelFiles ]
  , featureState = []
  }

  where
    serveStaticDirFiles :: ServerPart Response
    serveStaticDirFiles =
      serveDirectory DisableBrowsing [] serverStaticDir

    serveStaticToplevelFile :: String -> FilePath -> ServerPart Response
    serveStaticToplevelFile mimetype filename =
      serveFile (asContentType mimetype) (serverStaticDir </> filename)

    toplevelFiles = [("favicon.ico", "image/x-icon")]

    serveStaticTemplates :: ServerPart Response
    serveStaticTemplates =
      path $ \name -> do
        nullDir
        noTrailingSlash --TODO: redirect to non-slash version
        serveTemplate (name ++ ".html")

    serveStaticIndexTemplate :: ServerPart Response
    serveStaticIndexTemplate =
      serveTemplate "index.html"

    serveTemplate :: String -> ServerPart Response
    serveTemplate name = do
      mtemplate <- tryGetTemplate templates name
      case mtemplate of
        Nothing       -> mzero
        Just template -> ok $ toResponse $ template []

