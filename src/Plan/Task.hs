module Plan.Task where

import Data.Time
import Plan.TimeRange

data Task = Task
  { scheduled :: Maybe TimeRange
  , timeNeeded :: DiffTime
  , importance :: Int
  , deadline :: Day
  , taskName :: String
  } deriving (Eq, Show)

instance Ord Task where
  Task (Just (TimeRange s _)) _ _ _ _ <= Task (Just (TimeRange s' _)) _ _ _ _ =
    s <= s'
  _ <= _ = True

data Event = Event
  { eventName :: String
  , eventScheduled :: TimeRange
  }

eventToTask :: Day -> Event -> Task
eventToTask today (Event n s) = Task (Just s) (timeRangeSize s) maxBound today n
