module Distribution.Server.Util.ActionLog where

import Distribution.Server.Framework.Error (MessageSpan)
import Data.Set (Set)
import qualified Data.Set as Set
-- other imports..

-- Sketch of a datatype to log server actions
type ActionLog a = Set (ActionEntry a)

data ActionEntry a = ActionEntry {
    entryTime :: UTCTime,
    entryId :: UserId,
    entryData :: a
}
instance Ord (ActionEntry a) where
    compare a b = compare (entryTime b) (entryTime a)

data SomeLog = forall a. SomeLog {
    someLog :: ActionLog a,
    displayLog :: a -> [MessageSpan]
}

addEntryNow :: UserId -> a -> ActionLog a -> IO (ActionLog a)
addEntryNow uid datum (ActionLog alog) = do
    time <- getCurrentTime
    return $ ActionLog $ Set.insert (ActionEntry time uid datum) alog
