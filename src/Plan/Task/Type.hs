{-# LANGUAGE DeriveGeneric #-}

module Plan.Task.Type where

import Data.Aeson
import Data.Time
import Plan.TimeRange
import RIO

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
    if s == s'
      then e <= e'
      else s < s'
  _ <= _ = True

data OptTask = OptTask
  { optName :: String
  , optImportance :: Int
  , optDeadline :: Int
  , optTime :: Double
  } deriving (Show)
