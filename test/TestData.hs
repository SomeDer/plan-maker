module TestData where

import Data.Time
import Plan.Env
import Plan.Task

hoursToDiffTime :: Integer -> DiffTime
hoursToDiffTime h = picosecondsToDiffTime $ 3600 * 10 ^ (12 :: Int) * h

baseTime :: UTCTime
baseTime = read "2019-06-07 15:00:00"

simpleTask :: Day -> DiffTime -> Int -> String -> Task
simpleTask d t i n = Task Nothing t i d n Nothing 1 [] Nothing

simpleTask' :: Int -> String -> Task
simpleTask' = simpleTask (utctDay baseTime) (hoursToDiffTime 1)

simpleTask'' :: String -> Task
simpleTask'' = simpleTask' 0

baseSit :: Situation
baseSit = Situation ".plan.yaml" baseTime

con :: [Task] -> Config
con = flip Config $ utctDay baseTime

con1 :: Config
con1 = con [simpleTask'' "A", simpleTask'' "B", simpleTask'' "C"]

con2 :: Config
con2 = con [simpleTask' 3 "A", simpleTask' 2 "B", simpleTask' 1 "C"]

con3 :: Config
con3 = con [simpleTask (addDays 5 $ utctDay baseTime) (hoursToDiffTime 2) 0 "A"]
