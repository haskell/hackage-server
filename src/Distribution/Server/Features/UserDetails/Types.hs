{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Distribution.Server.Features.UserDetails.Types where

import Distribution.Server.Framework

import Data.SafeCopy (base, deriveSafeCopy)

import Data.Text (Text)
import Data.Aeson.TH


-------------------------
-- Types of stored data
--

data AccountDetails = AccountDetails {
                        accountName         :: !Text,
                        accountContactEmail :: !Text,
                        accountKind         :: Maybe AccountKind,
                        accountAdminNotes   :: !Text
                      }
  deriving (Eq, Show)



data AccountKind = AccountKindRealUser | AccountKindSpecial
  deriving (Eq, Show, Enum, Bounded)

data NameAndContact = NameAndContact { ui_name  :: Text, ui_contactEmailAddress :: Text }
data AdminInfo      = AdminInfo      { ui_accountKind :: Maybe AccountKind, ui_notes :: Text }


instance MemSize AccountDetails where
    memSize (AccountDetails a b c d) = memSize4 a b c d

instance MemSize AccountKind where
    memSize _ = memSize0

deriveJSON (compatAesonOptionsDropPrefix "ui_") ''NameAndContact
deriveJSON  compatAesonOptions                  ''AccountKind
deriveJSON (compatAesonOptionsDropPrefix "ui_") ''AdminInfo

$(deriveSafeCopy 0 'base ''AccountKind)
$(deriveSafeCopy 0 'base ''AccountDetails)
