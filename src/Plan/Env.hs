{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plan.Env where

import Data.Has
import Data.Time
import Data.Yaml
import Plan.Event
import Plan.Task.Type
import RIO

data Env = Env
  { config :: Config
  , situation :: Situation
  }

data Config = Config
  { tasks :: [Task]
  , events :: [Event]
  } deriving (Eq, Show, Generic)

instance ToJSON Config

instance FromJSON Config

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

instance Has Situation Env where
  hasLens = lens situation (\x y -> x {situation = y})

instance Has Config Env where
  hasLens = lens config (\x y -> x {config = y})

instance Has [Task] Config where
  hasLens = lens tasks (\x y -> x {tasks = y})

instance Has [Event] Config where
  hasLens = lens events (\x y -> x {events = y})

instance Has ConfigFile Situation where
  hasLens = lens configFile (\x y -> x {configFile = y})

instance Has CurrentTime Situation where
  hasLens = lens currentTime (\x y -> x {currentTime = y})

instance Has [Task] Env where
  hasLens = lens (tasks . config) $ \x y -> x {config = (config x) {tasks = y}}

instance Has [Event] Env where
  hasLens =
    lens (events . config) $ \x y -> x {config = (config x) {events = y}}

instance Has ConfigFile Env where
  hasLens =
    lens (configFile . situation) $ \x y ->
      x {situation = (situation x) {configFile = y}}

instance Has CurrentTime Env where
  hasLens =
    lens (currentTime . situation) $ \x y ->
      x {situation = (situation x) {currentTime = y}}
