
module Distribution.Server.ServerParts
    ( --guardAuth
    )
    where

{-import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import Distribution.Server.Users.Permissions (GroupName)

import qualified Distribution.Server.Auth.Basic as Auth

import Happstack.Server
import Happstack.State

guardAuth :: [GroupName] -> ServerPart ()
guardAuth gNames = do
  state <- query GetPackagesState
  group <- query $ LookupUserGroups gNames
  _ <- Auth.hackageAuth (userDb state) (Just group)
  return ()

-}
