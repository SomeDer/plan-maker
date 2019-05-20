{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Env where

import Data.Time
import Data.Yaml
import Lens.Micro.TH
import Plan.Event
import Plan.Task
import RIO

data Env = Env
  { envConfig :: Config
  , envSituation :: Situation
  }

data Config = Config
  { configTasks :: [Task]
  } deriving (Eq, Show, Generic)

instance ToJSON Config

instance FromJSON Config

data Situation = Situation
  { situationConfigLocation :: FilePath
  , situationTime :: UTCTime
  }

makeFields ''Env

makeFields ''Config

makeFields ''Situation

instance HasConfigLocation Env FilePath where
  configLocation = situation . configLocation

instance HasTime Env UTCTime where
  time = situation . time

instance HasTasks Env [Task] where
  tasks = config . tasks
