-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Framework.Templating
-- Copyright   :  (c) Duncan Coutts 2013
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Support for templates, html and text, based on @HStringTemplate@ package.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Framework.Templating (
    Template,
    renderTemplate,
    Templates,
    TemplatesMode(..),
    loadTemplates,
    reloadTemplates,
    getTemplate,
    tryGetTemplate,
    TemplateAttr,
    ($=),
    TemplateVal,
    templateDict,
    templateVal,
    utcTimeTemplateVal,
    templateEnumDesriptor,
    templateUnescaped,
    ToSElem(..),
  ) where

import Text.StringTemplate
import Text.StringTemplate.Base
import Text.StringTemplate.Classes
import Happstack.Server (ToMessage(..), toResponseBS)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T

--TODO: switch to bytestring builder, once we can depend on bytestring-0.10
--import qualified Data.ByteString.Lazy.Builder as Builder
--import Data.ByteString.Lazy.Builder (Builder)
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Html.Utf8 as Builder
import qualified Text.XHtml.Strict as XHtml
import Network.URI (URI)
import qualified Data.Aeson as JSON

import Distribution.Package (PackageName, PackageIdentifier)
import Distribution.Version (Version)
import Distribution.Text    (display)

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List
import Data.Maybe (isJust)
import Data.IORef
import System.FilePath ((<.>), takeExtension)
import qualified Data.Map as Map

import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)


type RawTemplate = StringTemplate Builder
type RawTemplateGroup = STGroup Builder

data Template = Template !TemplateKind !RawTemplate
data TemplateKind = TextTemplate | HtmlTemplate | XmlTemplate | OtherTemplate

renderTemplate :: Template -> LBS.ByteString
renderTemplate (Template _ st) = Builder.toLazyByteString (render st)

instance ToMessage Template where
  toResponse t@(Template tkind _) =
    toResponseBS (templateContentType tkind) (renderTemplate t)

newtype TemplateAttr = TemplateAttr (RawTemplate -> RawTemplate)

infix 0 $=
($=) :: ToSElem a => String -> a -> TemplateAttr
($=) k v = TemplateAttr (setAttribute k v)

templateUnescaped :: String -> LBS.ByteString -> TemplateAttr
templateUnescaped s x = TemplateAttr $ \st -> st {senv = envIns (SBLE (Builder.fromLazyByteString x)) (senv st)}
   where envIns v e = e {smp = Map.insert s v (smp e)}

newtype TemplateVal = TemplateVal (forall b. Stringable b => SElem b)

instance ToSElem TemplateVal where
    toSElem (TemplateVal se) = se

templateDict :: [(String, TemplateVal)] -> TemplateVal
templateDict kvs =
    TemplateVal (SM (Map.fromList (fmap (\(k, TemplateVal v) -> (k, v)) kvs)))

templateVal :: ToSElem a => String -> a -> (String, TemplateVal)
templateVal k v = (k, TemplateVal (toSElem v))

-- | Helper to make it easier to construct forms that use enumeration types
templateEnumDesriptor :: (Eq a, JSON.ToJSON a) => (a -> String) ->
                         [a] -> a -> [TemplateVal]
templateEnumDesriptor tostr xs x =
    [ templateDict
        [ templateVal "index"    i
        , templateVal "selected" (x' == x)
        , templateVal "asstring" (tostr x')
        , templateVal "asjson"   (JSON.encode x')
        ]
    | (i, x') <- zip [0::Int ..] xs ]


instance ToSElem XHtml.Html where
    -- The use of SBLE here is to prevent the html being escaped
    toSElem = SBLE . stFromString . XHtml.showHtmlFragment

instance ToSElem URI               where toSElem = toSElem . show
instance ToSElem PackageName       where toSElem = toSElem . display
instance ToSElem Version           where toSElem = toSElem . display
instance ToSElem PackageIdentifier where toSElem = toSElem . display

-- | Format 'UTCTime' value as HTML.
utcTimeTemplateVal :: String -> UTCTime -> (String, TemplateVal)
utcTimeTemplateVal k utime = (k, TemplateVal (SBLE (stFromString utcSpanEl)))
  where
    utcSpanEl :: String
    utcSpanEl = concat
        [ "<span title=\""
        , formatTime defaultTimeLocale "%c" utime
        , "\">"
        , formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utime
        , "</span>"
        ]

data Templates = TemplatesNormalMode !(IORef RawTemplateGroup)
                                     [FilePath] [String]
               | TemplatesDesignMode [FilePath] [String]

data TemplatesMode = NormalMode | DesignMode

loadTemplates :: TemplatesMode -> [FilePath] -> [String] -> IO Templates
loadTemplates templateMode templateDirs expectedTemplates = do
    templateGroup <- loadTemplateGroup templateDirs
    checkTemplates templateGroup templateDirs expectedTemplates
    case templateMode of
      NormalMode -> do
        templateGroupRef <- newIORef templateGroup
        return (TemplatesNormalMode templateGroupRef
                                    templateDirs expectedTemplates)

      DesignMode ->
        return (TemplatesDesignMode templateDirs expectedTemplates)

reloadTemplates :: Templates -> IO ()
reloadTemplates (TemplatesNormalMode templateGroupRef
                                     templateDirs expectedTemplates) = do
  templateGroup' <- loadTemplateGroup templateDirs
  checkTemplates templateGroup' templateDirs expectedTemplates
  writeIORef templateGroupRef templateGroup'

reloadTemplates (TemplatesDesignMode _ _) = return ()

getTemplate :: (MonadIO m, MonadFail m) => Templates -> String -> m ([TemplateAttr] -> Template)
getTemplate templates@(TemplatesNormalMode _ _ expectedTemplates) name = do
    when (name `notElem` expectedTemplates) $ failMissingTemplate name
    tryGetTemplate templates name >>= maybe (failMissingTemplate name) return

getTemplate templates@(TemplatesDesignMode _ expectedTemplates) name = do
    when (name `notElem` expectedTemplates) $ failMissingTemplate name
    tryGetTemplate templates name >>= maybe (failMissingTemplate name) return

tryGetTemplate :: (MonadIO m, MonadFail m) => Templates -> String -> m (Maybe ([TemplateAttr] -> Template))
tryGetTemplate (TemplatesNormalMode templateGroupRef _ _) name = do
    templateGroup <- liftIO $ readIORef templateGroupRef
    let tkind     = templateKindFromExt name
        mtemplate = fmap (\t -> Template tkind
                              . applyEscaping tkind
                              . applyTemplateAttrs t)
                         (getStringTemplate name templateGroup)
    return mtemplate

tryGetTemplate (TemplatesDesignMode templateDirs expectedTemplates) name = do
    templateGroup <- liftIO $ loadTemplateGroup templateDirs
    checkTemplates templateGroup templateDirs expectedTemplates
    let tkind     = templateKindFromExt name
        mtemplate = fmap (\t -> Template tkind
                              . applyEscaping tkind
                              . applyTemplateAttrs t)
                         (getStringTemplate name templateGroup)
    return mtemplate

templateKindFromExt :: FilePath -> TemplateKind
templateKindFromExt tname =
    case takeExtension tname of
      ".txt"  -> TextTemplate
      ".html" -> HtmlTemplate
      ".xml"  -> XmlTemplate
      _       -> OtherTemplate

applyEscaping :: TemplateKind -> RawTemplate -> RawTemplate
applyEscaping TextTemplate  = id
applyEscaping HtmlTemplate  = setEncoder escapeHtml
applyEscaping XmlTemplate   = setEncoder escapeHtml -- ok to reuse
applyEscaping OtherTemplate = id

escapeHtml :: Builder -> Builder
escapeHtml = Builder.fromHtmlEscapedLazyText
           . T.decodeUtf8With T.lenientDecode
           . Builder.toLazyByteString

templateContentType :: TemplateKind -> BS.ByteString
templateContentType TextTemplate  = "text/plain; charset=utf-8"
templateContentType HtmlTemplate  = "text/html; charset=utf-8"
templateContentType XmlTemplate   = "application/xml"
templateContentType OtherTemplate = "text/plain"

applyTemplateAttrs :: RawTemplate -> [TemplateAttr] -> RawTemplate
applyTemplateAttrs = foldl' (\t' (TemplateAttr a) -> a t')

failMissingTemplate :: (Monad m, MonadFail m) => String -> m a
failMissingTemplate name =
  fail $ "getTemplate: request for unexpected template " ++ name
      ++ ". So we can do load-time checking, all templates used "
      ++ "must be listed in the call to loadTemplates."

loadTemplateGroup :: [FilePath] -> IO RawTemplateGroup
loadTemplateGroup [] = return nullGroup
loadTemplateGroup templateDirs = do
    templateGroup <- mapM directoryGroup templateDirs
--                 `catchJust` IOError
    return (foldr1 (flip addSuperGroup) templateGroup)

checkTemplates :: (Monad m, MonadFail m) => RawTemplateGroup -> [FilePath] -> [String] -> m ()
checkTemplates templateGroup templateDirs expectedTemplates = do
    let checks    = [ (t, fmap checkTemplate
                               (getStringTemplate t templateGroup))
                    | t <- expectedTemplates ]
        missing   = [ t | (t,Nothing) <- checks ]
        problems  = [ (t, p) | (t,Just p@(es,_ma,mt)) <- checks
                             , isJust es || {-isJust ma ||-} isJust mt ]

    when (not (null missing)) $
      fail $ "Missing template files: " ++ intercalate ", " (map (<.> "st") missing)
         ++ ". Search path was: " ++ intercalate " " templateDirs

    when (not (null problems)) $
      fail $ reportTemplateProblems problems

  where
    reportTemplateProblems :: [(String, (Maybe String, Maybe [String], Maybe [String]))] -> String
    reportTemplateProblems problems =
      unlines
      [ "Problem with template " ++ t ++ ":\n"
        ++ formatTemplateProblem p
      | (t, p) <- problems ]

    formatTemplateProblem  :: (Maybe String, Maybe [String], Maybe [String]) -> String
    formatTemplateProblem (Just e, _ma, _mt) = e

    formatTemplateProblem (_es, _ma, Just mt) =
      "References to missing templates: " ++ intercalate ", " mt
    formatTemplateProblem _ = "Unknown error with template"
