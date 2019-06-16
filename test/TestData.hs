module TestData where

import Control.Lens
import Data.Time
import Plan.Env
import Plan.Task
import Plan.TimeRange

hoursToDiffTime :: Integer -> DiffTime
hoursToDiffTime h = picosecondsToDiffTime $ 3600 * 10 ^ (12 :: Int) * h

daysLater :: LocalTime
daysLater = read "2019-06-10 15:00:00"

baseTime :: LocalTime
baseTime = read "2019-06-07 15:00:00"

middleTime :: LocalTime
middleTime = read "2019-06-07 13:30:00"

earlierTime :: LocalTime
earlierTime = read "2019-06-07 13:00:00"

simpleTask :: Day -> DiffTime -> Int -> String -> Task
simpleTask d t i n = Task Nothing t i d n Nothing 1 [] Nothing

simpleTask' :: Int -> String -> Task
simpleTask' = simpleTask (localDay baseTime) (hoursToDiffTime 1)

simpleTask'' :: String -> Task
simpleTask'' = simpleTask' 0

baseSit :: Situation
baseSit = Situation ".plan.yaml" baseTime

con :: [Task] -> Config
con = flip Config $ localDay baseTime

con1 :: Config
con1 = con [simpleTask'' "A", simpleTask'' "B", simpleTask'' "C"]

con2 :: Config
con2 = con [simpleTask' 3 "A", simpleTask' 2 "B", simpleTask' 1 "C"]

con3 :: Config
con3 =
  con [simpleTask (addDays 5 $ localDay baseTime) (hoursToDiffTime 2) 0 "A"]

con4 :: Config
con4 =
  con [set workingFrom (Just $ localTimeOfDay earlierTime) $ simpleTask'' "A"]

con5 :: Config
con5 = over tasks (simpleTask'' "B" :) con4

con6 :: Config
con6 =
  flip (over tasks) con3 $
  fmap $
  set
    workedToday
    [TimeRange (localTimeOfDay earlierTime) $ localTimeOfDay middleTime]
