{-# LANGUAGE DeriveGeneric #-}

module Plan.TimeRange where

import Data.Aeson
import Data.Time
import RIO

data TimeRange = TimeRange
  { start :: TimeOfDay
  , end :: TimeOfDay
  } deriving (Eq, Show, Generic)

instance ToJSON TimeRange

instance FromJSON TimeRange

timeRangeSize :: TimeRange -> DiffTime
timeRangeSize (TimeRange s e) = timeOfDayToTime e - timeOfDayToTime s
