-- | Module providing standard Haskell vocabulary
--
-- This provides an extended "Prelude" re-exporting the most common
-- @base@ symbols to reduce the @import@-boilerplate as well as
-- unused-import warnings.
module Distribution.Server.Prelude (module X) where

import           Control.Applicative as X
import           Control.Monad       as X
import           Data.Int            as X
import           Data.Maybe          as X
import           Data.Semigroup      as X
import           Data.Typeable       as X (Typeable)
import           Data.Word           as X
import           Prelude             as X
