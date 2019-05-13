module Plan.TimeRange where

import Data.Time

data TimeRange = TimeRange
  { start :: TimeOfDay
  , end :: TimeOfDay
  } deriving (Eq, Show)

timeRangeSize :: TimeRange -> DiffTime
timeRangeSize (TimeRange s e) = timeOfDayToTime e - timeOfDayToTime s
