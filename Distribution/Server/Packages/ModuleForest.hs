-- (C) Copyright by Bas van Dijk, v.dijk.bas@gmail.com, 2008
-- Inspiration (read: copied, renamed and simplified) from:
-- http://code.haskell.org/haddock/src/Haddock/ModuleTree.hs

module Distribution.Server.Packages.ModuleForest ( ModuleForest, ModuleTree(..), moduleForest ) where

import Distribution.ModuleName ( ModuleName, components )

--------------------------------------------------------------------------------

type ModuleForest = [ModuleTree]

data ModuleTree = Node String       -- Part of module name
                       Bool         -- Is this an existing module?
                       ModuleForest -- Sub modules
    deriving (Show, Eq)

--------------------------------------------------------------------------------

moduleForest :: [ModuleName] -> ModuleForest
moduleForest = foldr (addToForest . components) []

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

