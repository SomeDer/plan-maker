{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.TimeRange where

import Data.Time
import Data.Yaml
import Lens.Micro.TH
import RIO

data TimeRange = TimeRange
  { timeRangeStart :: TimeOfDay
  , timeRangeEnd :: TimeOfDay
  } deriving (Eq, Show, Generic)

instance ToJSON TimeRange

instance FromJSON TimeRange

timeRangeSize :: TimeRange -> DiffTime
timeRangeSize (TimeRange s e) = timeOfDayToTime e - timeOfDayToTime s

makeFields ''TimeRange
