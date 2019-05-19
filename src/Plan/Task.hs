{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Task where

import Data.Time
import Data.Yaml
import Lens.Micro.TH
import Plan.TimeRange
import RIO

data Task = Task
  { taskScheduled :: Maybe TimeRange
  , taskTimeNeeded :: DiffTime
  , taskImportance :: Int
  , taskDeadline :: Day
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
  { optTaskName :: String
  , optTaskImportance :: Int
  , optTaskDeadline :: Int
  , optTaskTimeNeeded :: Double
  } deriving (Show)

makeFields ''Task

makeFields ''OptTask
