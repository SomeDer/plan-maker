module TestData where

import Data.Time
import Plan.Env
import Plan.Task

baseTime :: UTCTime
baseTime = read "2019-06-07 15:00:00"

simpleTask :: Day -> DiffTime -> String -> Task
simpleTask d t n = Task Nothing t 0 d n Nothing 1 [] Nothing

simplestTask :: String -> Task
simplestTask =
  simpleTask (utctDay baseTime) $
  picosecondsToDiffTime $ 3600 * 10 ^ (12 :: Int)

baseSit :: Situation
baseSit = Situation ".plan.yaml" baseTime

con1 :: Config
con1 = Config [simplestTask "A", simplestTask "B", simplestTask "C"] $ utctDay baseTime
