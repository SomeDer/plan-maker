{-# LANGUAGE TemplateHaskell, FunctionalDependencies #-}

module Plan.Env where

import Control.Lens
import Data.Time
import Data.Yaml
import GHC.Generics
import Plan.Task

data Env = Env
  { envConfig :: Config
  , envSituation :: Situation
  } deriving (Show)

data Config = Config
  { configTasks :: [Task]
  , configTodayIs :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON Config

instance FromJSON Config

data Situation = Situation
  { situationConfigLocation :: FilePath
  , situationTime :: LocalTime
  } deriving (Show)

makeFields ''Env

makeFields ''Config

makeFields ''Situation

instance HasConfigLocation Env FilePath where
  configLocation = situation . configLocation

instance HasTime Env LocalTime where
  time = situation . time

instance HasTasks Env [Task] where
  tasks = config . tasks
