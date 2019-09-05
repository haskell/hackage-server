{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.StaticFiles (
    initStaticFilesFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Text.XHtml.Strict (Html, toHtml, anchor, (<<), (!), href, paragraph)

import Data.List hiding (find)
import System.FilePath
import System.Directory (getDirectoryContents)

-- | A feature to provide the top level files on the site (using templates)
-- and also serve the genuinely static files.
--
initStaticFilesFeature :: ServerEnv
                       -> IO (IO HackageFeature)
initStaticFilesFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode} = do
  -- Page templates
  templates <- loadTemplates serverTemplatesMode
                 [serverTemplatesDir]
                 ["index.html", "hackageErrorPage.txt", "hackageErrorPage.html"]

  staticFiles <- find (isSuffixOf ".html.st") serverTemplatesDir

  return $ do
    let feature = staticFilesFeature env templates staticFiles

    return feature

-- Simpler version of Syhstem.FilePath.Find (which requires unix-compat)
find :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
find p dirPath = do
  contents <- getDirectoryContents dirPath
  return (filter p contents)

staticFilesFeature :: ServerEnv -> Templates -> [FilePath] -> HackageFeature
staticFilesFeature ServerEnv{serverStaticDir, serverTemplatesMode}
                   templates staticFiles =
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
            resourceGet  = [("", const serveStaticDirFiles)]
          }
      ]
        ++
      [ (resourceAt ("/" ++ dropExtension name)) {
            resourceGet  = [("", \_ -> serveStaticTemplate name)]
          }
      | name <- toplevelTemplates ]
  , featureState = []
  , featureErrHandlers = [("txt",  textErrorPage)
                         ,("html", htmlErrorPage)]
  , featureReloadFiles = reloadTemplates templates
  }

  where
    staticResourceCacheControls =
      case serverTemplatesMode of
        DesignMode -> [Public, maxAgeSeconds 0, NoCache]
        NormalMode -> [Public, maxAgeDays 1]

    serveStaticDirFiles :: ServerPartE Response
    serveStaticDirFiles = do
      cacheControlWithoutETag staticResourceCacheControls
      serveDirectory DisableBrowsing [] serverStaticDir

    serveStaticToplevelFile :: String -> FilePath -> ServerPartE Response
    serveStaticToplevelFile mimetype filename = do
      cacheControlWithoutETag staticResourceCacheControls
      serveFile (asContentType mimetype) (serverStaticDir </> filename)

    serveStaticTemplate :: String -> ServerPartE Response
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
        Just template -> do
          cacheControlWithoutETag staticResourceCacheControls
          ok $ toResponse $ template []

    textErrorPage (ErrorResponse errCode hdrs errTitle message) = do
        template <- getTemplate templates "hackageErrorPage.txt"
        let formattedMessage = messageToText message
            response = toResponse $ template
              [ "errorTitle"   $= errTitle
              , "errorMessage" $= formattedMessage
              ]
        return $ response {
          rsCode    = errCode,
          rsHeaders = addHeaders (rsHeaders response) hdrs
        }
    textErrorPage GenericErrorResponse =
      textErrorPage internalServerErrorResponse

    htmlErrorPage :: ErrorResponse -> ServerPartE Response
    htmlErrorPage (ErrorResponse errCode hdrs errTitle message) = do
        template <- getTemplate templates "hackageErrorPage.html"
        let formattedMessage = paragraph << errorToHtml message
            response = toResponse $ template
              [ "errorTitle"   $= errTitle
              , "errorMessage" $= formattedMessage
              ]
        return $ response {
          rsCode    = errCode,
          rsHeaders = addHeaders (rsHeaders response) hdrs
        }
    htmlErrorPage GenericErrorResponse =
      htmlErrorPage internalServerErrorResponse

    toplevelTemplates = map dropExtension staticFiles

addHeaders :: Headers -> [(String, String)] -> Headers
addHeaders hdrs hdrs' = foldl' (\h (k,v) -> addHeader k v h) hdrs (reverse hdrs')

errorToHtml :: [MessageSpan] -> [Html]
errorToHtml []               = []
errorToHtml (MText x    :xs) = toHtml x: errorToHtml xs
errorToHtml (MLink x url:xs) = (anchor ! [href url] << x): errorToHtml xs
