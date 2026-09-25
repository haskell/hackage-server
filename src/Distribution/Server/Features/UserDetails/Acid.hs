{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Distribution.Server.Features.UserDetails.Acid
  ( module State
  , GetUserDetailsTable(..)
  , LookupUserDetails(..)
  , ReplaceUserDetailsTable(..)
  , SetUserDetails(..)
  , SetUserNameContact(..)
  , SetUserAdminInfo(..)
  , DeleteUserDetails(..)
  ) where

import Distribution.Server.Features.UserDetails.State as State
import Distribution.Server.Framework

------------------------------
-- Acid event types
--

-- This splice generates an orphan IsAcidic UserDetailsTable instance:
-- IsAcidic comes from acid-state, and UserDetailsTable is defined in
-- State.
--
-- makeAcidic generates the instance together with the event
-- types. The serialized event tags of the event types depend on the
-- name of the module where the splice is run, so for simplicity we
-- continue to run the splice here, despite UserDetailsTable no longer
-- being defined here.
makeAcidic ''UserDetailsTable [
    --queries
    'getUserDetailsTable,
    'lookupUserDetails,
    --updates
    'replaceUserDetailsTable,
    'setUserDetails,
    'setUserNameContact,
    'setUserAdminInfo,
    'deleteUserDetails
  ]


