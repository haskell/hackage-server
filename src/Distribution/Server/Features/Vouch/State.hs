{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Vouch.State where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..))

import Data.SafeCopy (base, deriveSafeCopy)
import Distribution.Server.Framework (MemSize(..), memSize2)
import Distribution.Server.Framework (Query, Update)
import Distribution.Server.Framework (makeAcidic)
import Distribution.Server.Users.Types (UserId(..))

data VouchData =
  VouchData
    { vouches :: Map.Map UserId [(UserId, UTCTime)]
    , notNotified :: Set.Set UserId
    }
  deriving (Show, Eq)

instance MemSize VouchData where
  memSize (VouchData vouches notified) = memSize2 vouches notified

putVouch :: UserId -> (UserId, UTCTime) -> Update VouchData ()
putVouch vouchee (voucher, now) = do
  VouchData tbl notNotified <- get
  let oldMap = fromMaybe [] (Map.lookup vouchee tbl)
      newMap = (voucher, now) : oldMap
  put $ VouchData (Map.insert vouchee newMap tbl) notNotified

getVouchesFor :: UserId -> Query VouchData [(UserId, UTCTime)]
getVouchesFor needle = do
  VouchData tbl _notNotified <- ask
  pure . fromMaybe [] $ Map.lookup needle tbl

getVouchesData :: Query VouchData VouchData
getVouchesData = ask

replaceVouchesData :: VouchData -> Update VouchData ()
replaceVouchesData = put

$(deriveSafeCopy 0 'base ''VouchData)

makeAcidic ''VouchData
  [ 'putVouch
  , 'getVouchesFor
  -- Stock
  , 'getVouchesData
  , 'replaceVouchesData
  ]
