-- | This module re-exports enough of "Data.Acid" to give manual(ish) instances
-- of 'IsAcidic' and related classes. The only reason to give manual instances
-- is so that we can implement 'methodTag' via 'movedMethodTag'.
--
-- Among its many flaws, 'makeAcidic' gives an implementation of 'methodTag'
-- which in turn bakes the /module name/ into the serialisation schema. This
-- means calls to 'makeAcidic' are not referentially transparent, thus changing
-- the defining module will prevent deserialisation.
--
-- Unfortunately, as part of #1486, it's very desirable to move these instances
-- out of the main library component and into a deprecatable migration
-- component.
module Data.Acid.Compat
  ( module Data.Acid.Compat
  , module Data.Acid.Common
  , module Data.Acid.Core
  , module Data.SafeCopy
  ) where

import Data.Acid.Common
import Data.Acid.Core
import Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString.Lazy.Char8 as Lazy ( pack )
import Data.Typeable
import Data.SafeCopy

-- Note [Acid Migration]
-- ~~~~~~~~~~~~~~~~~~~~~
--
-- Due to silly technical reasons deep within @acid-state@, it's impossible to
-- move a call to 'makeAcidic' into a different module. If you found
-- a reference to this Note, it means we have /manually/ moved this
-- 'makeAcidic' call.
--
-- If you find yourself needing to update one of these, uncomment the call to
-- 'makeAcidic', turn on @-ddump-splices@, and then manually inline the
-- resulting splice. You MUST change the splice and give a non-default
-- implementation of 'methodTag':
--
-- @methodTag = 'movedMethodTag' "Original.Qualified.Module.Name"@
--
-- where @My.Qualified.Module@ is the full module name of the /original/
-- location of the call to 'makeAcidic'. The original module name should be
-- found in the reference to this note.


-- | Generate a @Data.Acid.Core.Tag@ for some type @a@, explicitly overriding
-- its module path to trick @acid-state@ into doing the right thing.
movedMethodTag
    :: Typeable a
    => String
    -- ^ Qualified module name where 'makeAcidic' used to be called.
    -> a
    -> Lazy.ByteString
movedMethodTag modul = Lazy.pack . showQualifiedTypeRep . typeOf
  where
    showQualifiedTypeRep tr = modul <> "." <> show tr
