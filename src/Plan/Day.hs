module Plan.Day where

import Data.List (sortOn)
import Data.Maybe
import Data.Set
import Data.Time
import Plan.Task
import Plan.TimeRange

planDay :: UTCTime -> [Task] -> [Event] -> Set Task
planDay (UTCTime day time) ts' es =
  Prelude.foldr
    f
    (fromList
       [ Task (Just $ TimeRange timeNow timeNow) 0 0 day "Now"
       , Task (Just $ TimeRange midnight' midnight') 0 0 day "Midnight"
       ])
    xs
  where
    midnight' = TimeOfDay 23 59 59
    timeNow = timeToTimeOfDay time
    xs = sortOn importance $ ts' <> fmap (eventToTask day) es
    f n ts =
      case scheduled n of
        Just _ -> insert n ts
        Nothing ->
          let need =
                diffTimeToPicoseconds (timeNeeded n) `div`
                diffDays (deadline n) day
              attemptInsert i =
                let planStart = end $ fromJust $ scheduled $ elemAt i ts
                    planEnd = start $ fromJust $ scheduled $ elemAt (i + 1) ts
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    plannedTimeRange =
                      TimeRange planStart $
                      timeToTimeOfDay $
                      timeOfDayToTime planStart + picosecondsToDiffTime need
                 in if need <=
                       convert planEnd - convert planStart
                      then insert (n {scheduled = Just plannedTimeRange}) ts
                      else attemptInsert (i + 1)
           in attemptInsert 0
