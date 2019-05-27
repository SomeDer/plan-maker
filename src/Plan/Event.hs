{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Event where

import Control.Lens
import Plan.Task
import Plan.TimeRange

data OptEvent = OptEvent
  { optEventName :: String
  , optEventDaysTil :: Integer
  , optEventStart :: String
  , optEventEnd :: String
  , optEventRecur :: Bool
  } deriving (Show)

makeFields ''OptEvent
