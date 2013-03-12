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
module Distribution.Server.Framework.Templating (
    Template,
    renderTemplate,
    Templates,
    TemplatesMode(..),
    loadTemplates,
    getTemplate,
    tryGetTemplate,
    TemplateAttr,
    ($=),
  ) where

import Text.StringTemplate
import Happstack.Server (ToMessage(..), toResponseBS)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

--TODO: switch to bytestring builder, once we can depend on bytestring-0.10
--import qualified Data.ByteString.Lazy.Builder as Builder
--import Data.ByteString.Lazy.Builder (Builder)
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List
import Data.Maybe (isJust)
import System.FilePath ((<.>))


type RawTemplate = StringTemplate Builder
type RawTemplateGroup = STGroup Builder

newtype Template = Template RawTemplate

renderTemplate :: Template -> LBS.ByteString
renderTemplate (Template st) = Builder.toLazyByteString (render st)

instance ToMessage Template where
  toResponse t = toResponseBS contentType (renderTemplate t)
    where
      contentType = BS.pack "text/html; charset=utf-8"

newtype TemplateAttr = TemplateAttr (RawTemplate -> RawTemplate)

infix 0 $=
($=) :: ToSElem a => String -> a -> TemplateAttr
($=) k v = TemplateAttr (setAttribute k v)




data Templates = TemplatesNormalMode !RawTemplateGroup
               | TemplatesDesignMode [FilePath] [String]

data TemplatesMode = NormalMode | DesignMode

--FIXME: for html template we have to do escaping, this is essential!
loadTemplates :: TemplatesMode -> [FilePath] -> [String] -> IO Templates
loadTemplates templateMode templateDirs expectedTemplates = do
    templateGroup <- loadTemplateGroup templateDirs
    checkTemplates templateGroup templateDirs expectedTemplates
    case templateMode of
      NormalMode -> return $  TemplatesNormalMode templateGroup
      DesignMode -> return $! TemplatesDesignMode templateDirs expectedTemplates

getTemplate :: MonadIO m => Templates -> String -> m ([TemplateAttr] -> Template)
getTemplate templates@(TemplatesNormalMode _) name =
    tryGetTemplate templates name >>= maybe (failMissingTemplate name) return

getTemplate templates@(TemplatesDesignMode _ expectedTemplates) name = do
    when (name `notElem` expectedTemplates) $
      failMissingTemplate name
    tryGetTemplate templates name >>= maybe (failMissingTemplate name) return

tryGetTemplate :: MonadIO m => Templates -> String -> m (Maybe ([TemplateAttr] -> Template))
tryGetTemplate (TemplatesNormalMode templateGroup) name =
    let mtemplate = fmap (\t -> Template . applyTemplateAttrs t)
                         (getStringTemplate name templateGroup)
    in return mtemplate

tryGetTemplate (TemplatesDesignMode templateDirs expectedTemplates) name = do
    templateGroup <- liftIO $ loadTemplateGroup templateDirs
    checkTemplates templateGroup templateDirs expectedTemplates
    let mtemplate = fmap (\t -> Template . applyTemplateAttrs t)
                         (getStringTemplate name templateGroup)
    return mtemplate

applyTemplateAttrs :: RawTemplate -> [TemplateAttr] -> RawTemplate
applyTemplateAttrs = foldl' (\t' (TemplateAttr a) -> a t')

failMissingTemplate :: Monad m => String -> m a
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

checkTemplates :: Monad m => RawTemplateGroup -> [FilePath] -> [String] -> m ()
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
