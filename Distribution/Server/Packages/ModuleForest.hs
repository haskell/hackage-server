-- (C) Copyright by Bas van Dijk, v.dijk.bas@gmail.com, 2008
-- Inspiration (read: copied, renamed and simplified) from:
-- http://code.haskell.org/haddock/src/Haddock/ModuleTree.hs

module Distribution.Server.Packages.ModuleForest (
  ModuleForest,
  ModuleTree(..),
  moduleForest
  ) where

import Distribution.ModuleName ( ModuleName, components )

--------------------------------------------------------------------------------

type ModuleForest = [ModuleTree]

data ModuleTree = Node !String       -- Part of module name
                       !Bool         -- Is this an existing module?
                       !Bool         -- Are there corresponding API docs?
                        ModuleForest -- Sub modules
    deriving (Show, Eq)

--------------------------------------------------------------------------------

moduleForest :: [(ModuleName, Bool)] -> ModuleForest
moduleForest =
  foldr (\(modname, hasdocs) -> addToForest hasdocs (components modname)) []

addToForest :: Bool -> [String] -> ModuleForest -> ModuleForest
addToForest _        [] ts = ts
addToForest hasdocs' ss [] = mkSubTree hasdocs' ss
addToForest hasdocs' s1ss@(s1:ss) (t@(Node s2 isModule hasdocs subs) : ts) =
  case compare s1 s2 of
    GT -> t : addToForest hasdocs' s1ss ts
    EQ -> Node s2 (isModule || null ss) (hasdocs || null ss && hasdocs')
                  (addToForest hasdocs' ss subs) : ts
    LT -> mkSubTree hasdocs' s1ss ++ t : ts

mkSubTree :: Bool -> [String] -> ModuleForest
mkSubTree _       []     = []
mkSubTree hasdocs (s:ss) = [Node s (null ss) (null ss && hasdocs)
                                   (mkSubTree hasdocs ss)]

