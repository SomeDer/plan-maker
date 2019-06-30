{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

import Control.Lens
import Data.List (sortOn, partition)
import Data.Maybe
import Data.Set (elemAt, fromList, insert, toList)
import Data.Time
import Plan.Task
import Plan.TimeRange

displayTask :: Bool -> Task -> String
displayTask b t =
  if t ^. identifier == 0
    then ""
    else show (t ^. identifier) <> ") " <> ti <> t ^. name
  where
    (Just s, Just e) = getSchedule t
    f = take 5 . show
    ti =
      if b
        then f s <> "-" <> f e <> ": "
        else ""

timeWorked :: Task -> TimeOfDay -> DiffTime
timeWorked n t =
  sum $
  fmap timeRangeSize $
  mappend (n ^. workedToday) $
  maybe [] ((: []) . flip TimeRange t) $ n ^. workingFrom

timeNeededToday :: LocalTime -> Task -> Integer
timeNeededToday (LocalTime day t) n =
  if daysLeft == 0
    then 0
    else subtract (diffTimeToPicoseconds $ timeWorked n t) $
         flip div daysLeft $ diffTimeToPicoseconds $ n ^. timeNeeded
  where
    daysLeft = diffDays (n ^. deadline) day + 1

planDay :: LocalTime -> [Task] -> [Task]
planDay tim@(LocalTime day t) ts' =
  let xs =
        sortOn (view importance) $ snd $ finishedUnfinished tim ts'
      f n ts =
        case n ^. scheduled of
          Just e
            | e ^. start >= t -> insert n ts
            | n ^. deadline /= day -> ts
            | e ^. end < t ->
              flip insert ts $ set identifier 0 n
            | otherwise ->
              flip insert ts $
              set (scheduled . _Just . start) t n
          Nothing ->
            let need = timeNeededToday tim n
                attemptInsert i
                  | i + 1 >= length ts = ts
                  | convert planEnd - convert planStart >= need =
                    flip insert ts $
                    set timeNeeded (picosecondsToDiffTime need) $
                    set scheduled (Just plannedTimeRange) n
                  | otherwise = attemptInsert (i + 1)
                  where
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    planPart p i' =
                      view p $ fromJust $ view scheduled $ elemAt (i + i') ts
                    planStart = planPart end 0
                    planEnd = planPart start 1
                    plannedTimeRange =
                      TimeRange planStart $
                      timeToTimeOfDay $
                      timeOfDayToTime planStart + picosecondsToDiffTime need
             in if need <= 0
                  then ts
                  else attemptInsert 0
      dummyTask n ti =
        Task (Just $ TimeRange ti ti) 0 0 day n Nothing 0 [] Nothing
    in filter ((/= 0) . view identifier) $ toList $ foldr
        f
        (fromList
           [ dummyTask "Now" t
           , dummyTask "Midnight" $ TimeOfDay 23 59 59
           ])
        xs
init' :: [a] -> [a]
init' [] = []
init' xs = init xs

finishedUnfinished :: LocalTime -> [Task] -> ([Task], [Task])
finishedUnfinished t = partition $ (<= 0) . timeNeededToday t
