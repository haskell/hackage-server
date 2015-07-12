{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RecordWildCards #-}

module Distribution.Server.Features.Votes.State where

import Distribution.Package (PackageName)

import Distribution.Server.Features.Votes.Types
  ( Votes(..)
  , addVote
  , removeVote
  )

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.State ()

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask)

$(deriveSafeCopy 0 'base ''Votes)

dbAddVote :: PackageName -> UserId -> Update Votes ()
dbAddVote pkgname uid = do
  state <- State.get
  State.put $ addVote pkgname uid state

dbRemoveVote :: PackageName -> UserId -> Update Votes ()
dbRemoveVote pkgName uid = do
  state <- State.get
  State.put $ removeVote pkgName uid state

dbGetVotes :: Query Votes Votes
dbGetVotes = ask

-- Replace the entire map
dbReplaceVotes :: Votes -> Update Votes ()
dbReplaceVotes = State.put

makeAcidic
  ''Votes
  [ 'dbAddVote
  , 'dbRemoveVote
  , 'dbGetVotes
  , 'dbReplaceVotes
  ]
