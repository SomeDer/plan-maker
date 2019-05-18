module Plan.Event where

import Data.Time
import Plan.Task.Type
import Plan.TimeRange
import RIO

data Event = Event
  { eventName :: String
  , eventDate :: Day
  , eventScheduled :: TimeRange
  }

eventToTask :: Day -> Event -> Task
eventToTask today (Event n d s) =
  Task
    (Just s)
    (if d == today
       then timeRangeSize s
       else 0)
    maxBound
    (addDays 1 today)
    n
