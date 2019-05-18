module Plan.Plan where

import Data.Has
import Data.List (sortOn)
import Data.Maybe
import Data.Set (elemAt, fromList, insert)
import Data.Time
import Plan.Env
import Plan.Event
import Plan.Task
import Plan.TimeRange
import Prelude (putStrLn)
import RIO

planDay :: UTCTime -> [Task] -> [Event] -> Set Task
planDay (UTCTime day time) ts' es =
  foldr
    f
    (fromList
       [ Task (Just $ TimeRange timeNow timeNow) 0 0 day "Now"
       , Task (Just $ TimeRange midnight' midnight') 0 0 day "Midnight"
       ])
    xs
  where
    midnight' = TimeOfDay 23 59 59
    timeNow = timeToTimeOfDay time
    xs =
      sortOn importance $
      ts' <>
      fmap
        ((eventToTask day .) $ \e ->
           if start (eventScheduled e) < timeToTimeOfDay time
             then e
                    { eventScheduled =
                        (eventScheduled e) {start = timeToTimeOfDay time}
                    }
             else e)
        (filter ((== day) . eventDate) es)
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
                 in if need <= convert planEnd - convert planStart
                      then insert (n {scheduled = Just plannedTimeRange}) ts
                      else attemptInsert (i + 1)
           in attemptInsert 0

printPlan ::
     (Has CurrentTime env, Has [Task] env, Has [Event] env) => RIO env ()
printPlan = do
  env <- ask
  forM_ (planDay (getTime $ getter env) (getter env) (getter env)) $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        liftIO $ putStrLn $ f s <> "-" <> f e <> ": " <> n
