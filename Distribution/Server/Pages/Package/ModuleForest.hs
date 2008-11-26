-- (C) Copyright by Bas van Dijk, v.dijk.bas@gmail.com, 2008
-- Inspiration (read: copied, renamed and simplified) from:
-- http://code.haskell.org/haddock/src/Haddock/ModuleTree.hs

module Distribution.Server.Pages.Package.ModuleForest ( moduleForest ) where

import Distribution.ModuleName ( ModuleName, components )
import Data.List ( intercalate )
import Text.XHtml

--------------------------------------------------------------------------------

moduleForest :: Maybe URL -> [ModuleName] -> Html
moduleForest mb_doc = renderModuleForest mb_doc . mkModuleForest

--------------------------------------------------------------------------------

type ModuleForest = [ModuleTree]

data ModuleTree = Node String       -- ^ Part of module name
                       Bool         -- ^ Is this an existing module?
                       ModuleForest -- ^ Sub modules

--------------------------------------------------------------------------------

mkModuleForest :: [ModuleName] -> ModuleForest
mkModuleForest = foldr (\m -> addToForest (components m)) []

addToForest :: [String] -> ModuleForest -> ModuleForest
addToForest [] ts = ts
addToForest ss [] = mkSubTree ss
addToForest s1ss@(s1:ss) (t@(Node s2 isModule subs) : ts)
  | s1 >  s2  = t : addToForest s1ss ts
  | s1 == s2  = Node s2 (isModule || null ss) (addToForest ss subs) : ts
  | otherwise = mkSubTree s1ss ++ t : ts

mkSubTree :: [String] -> ModuleForest
mkSubTree []     = []
mkSubTree (s:ss) = [Node s (null ss) (mkSubTree ss)]

--------------------------------------------------------------------------------

renderModuleForest :: Maybe URL -> ModuleForest -> Html
renderModuleForest mb_url = renderForest []
    where
      renderForest _       [] = noHtml
      renderForest pathRev ts = myUnordList $ map renderTree ts
          where
            renderTree (Node s isModule subs) =
                    ( if isModule then moduleEntry newPath else italics << s )
                +++ renderForest newPathRev subs
                where
                  newPathRev = s:pathRev
                  newPath = reverse newPathRev

      moduleEntry = maybe modName linkedName mb_url
      modName path = toHtml (intercalate "." path)
      linkedName url path = anchor ! [href modUrl] << modName path
          where
            modUrl = url ++ "/" ++ intercalate "-" path ++ ".html"

myUnordList :: HTML a => [a] -> Html
myUnordList = unordList ! [theclass "modules"]

--------------------------------------------------------------------------------
