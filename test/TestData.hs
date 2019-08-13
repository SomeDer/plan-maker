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

simpleTask :: Day -> DiffTime -> Int -> Word -> String -> Task
simpleTask d t i ide n = Task Nothing t i d n Nothing ide [] Nothing

simpleTask' :: Int -> Word -> String -> Task
simpleTask' = simpleTask (localDay baseTime) (hoursToDiffTime 1)

simpleTask'' :: Word -> String -> Task
simpleTask'' = simpleTask' 0

baseSit :: Situation
baseSit = Situation ".plan.yaml" baseTime

con :: [Task] -> Config
con = flip Config $ localDay baseTime

con1 :: Config
con1 = con [simpleTask'' 1 "A", simpleTask'' 2 "B", simpleTask'' 3 "C"]

con2 :: Config
con2 = con [simpleTask' 3 1 "A", simpleTask' 2 2 "B", simpleTask' 1 3 "C"]

con3 :: Config
con3 =
  con [simpleTask (addDays 5 $ localDay baseTime) (hoursToDiffTime 2) 0 1 "A"]

con4 :: Config
con4 =
  con [set workingFrom (Just $ localTimeOfDay earlierTime) $ simpleTask'' 1 "A"]

con5 :: Config
con5 = over tasks (simpleTask'' 2 "B" :) con4

con6 :: Config
con6 =
  flip (over tasks) con3 $
  fmap $
  set
    workedToday
    [TimeRange (localTimeOfDay earlierTime) $ localTimeOfDay middleTime]
