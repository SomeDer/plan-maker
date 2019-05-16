{-# LANGUAGE DeriveGeneric #-}

module Plan.Task where

import Data.Aeson
import Data.Time
import GHC.Generics
import Plan.TimeRange

data Task = Task
  { scheduled :: Maybe TimeRange
  , timeNeeded :: DiffTime
  , importance :: Int
  , deadline :: Day
  , taskName :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Task
instance FromJSON Task

instance Ord Task where
  Task (Just (TimeRange s e)) _ _ _ _ <= Task (Just (TimeRange s' e')) _ _ _ _ =
    if s == s' then e <= e' else s < s'
  _ <= _ = True

data Event = Event
  { eventName :: String
  , eventScheduled :: TimeRange
  }

eventToTask :: Day -> Event -> Task
eventToTask today (Event n s) = Task (Just s) (timeRangeSize s) maxBound today n
