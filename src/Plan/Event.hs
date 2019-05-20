{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Event where

import Lens.Micro.TH
import Plan.Task
import Plan.TimeRange
import RIO

data OptEvent = OptEvent
  { optEventName :: String
  , optEventDaysTil :: Integer
  , optEventStart :: String
  , optEventEnd :: String
  } deriving (Show)

makeFields ''OptEvent
