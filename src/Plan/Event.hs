{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Event where

import Data.Time
import Data.Yaml
import Lens.Micro.TH
import Plan.Task
import Plan.TimeRange
import RIO

data Event = Event
  { eventName :: String
  , eventDate :: Day
  , eventScheduled :: TimeRange
  } deriving (Eq, Show, Generic)

instance ToJSON Event

instance FromJSON Event

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

data OptEvent = OptEvent
  { optEventName :: String
  , optEventDaysTil :: Integer
  , optEventStart :: String
  , optEventEnd :: String
  } deriving (Show)

makeFields ''Event

makeFields ''OptEvent
