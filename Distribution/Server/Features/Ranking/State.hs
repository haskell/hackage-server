{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RecordWildCards #-}

module Distribution.Server.Features.Ranking.State where

import Distribution.Package (PackageName)

import Distribution.Server.Features.Ranking.Types
  ( Stars(..)
  , addStar
  , removeStar
  )

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.State ()

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask)

$(deriveSafeCopy 0 'base ''Stars)

dbAddStar :: PackageName -> UserId -> Update Stars ()
dbAddStar pkgname uid = do
  state <- State.get
  State.put $ addStar pkgname uid state

dbRemoveStar :: PackageName -> UserId -> Update Stars ()
dbRemoveStar pkgName uid = do
  state <- State.get
  State.put $ removeStar pkgName uid state

dbGetStars :: Query Stars Stars
dbGetStars = ask

-- Replace the entire map
dbReplaceStars :: Stars -> Update Stars ()
dbReplaceStars = State.put

makeAcidic
  ''Stars
  [ 'dbAddStar
  , 'dbRemoveStar
  , 'dbGetStars
  , 'dbReplaceStars
  ]
