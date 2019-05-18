module Plan.Task.Event where

import Data.Time
import Plan.Task.Type
import Plan.TimeRange
import RIO

data Event = Event
  { eventName :: String
  , eventScheduled :: TimeRange
  }

eventToTask :: Day -> Event -> Task
eventToTask today (Event n s) = Task (Just s) (timeRangeSize s) maxBound today n
