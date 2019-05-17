{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plan.Env where

import Data.Aeson
import Data.Has
import Data.Time
import Plan.Task.Type
import RIO

data Env = Env
  { tasks :: [Task]
  , events :: [Event]
  , situation :: Situation
  }

data Situation = Situation
  { configFile :: ConfigFile
  , currentTime :: CurrentTime
  }

newtype ConfigFile = ConfigFile
  { getConfigFile :: FilePath
  } deriving (Eq, Show, ToJSON, FromJSON)

newtype CurrentTime = CurrentTime
  { getTime :: UTCTime
  } deriving (Eq, Show, ToJSON, FromJSON)

instance Has [Task] Env where
  hasLens = lens tasks (\x y -> x {tasks = y})

instance Has [Event] Env where
  hasLens = lens events (\x y -> x {events = y})

instance Has Situation Env where
  hasLens = lens situation (\x y -> x {situation = y})

instance Has ConfigFile Situation where
  hasLens = lens configFile (\x y -> x {configFile = y})

instance Has CurrentTime Situation where
  hasLens = lens currentTime (\x y -> x {currentTime = y})

instance Has ConfigFile Env where
  hasLens =
    lens (configFile . situation) $ \x y ->
      x {situation = (situation x) {configFile = y}}

instance Has CurrentTime Env where
  hasLens =
    lens (currentTime . situation) $ \x y ->
      x {situation = (situation x) {currentTime = y}}
