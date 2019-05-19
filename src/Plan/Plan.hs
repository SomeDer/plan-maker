{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

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

planDay :: (MonadReader a1 m, HasSituation a1 a2,
              HasTime a2 UTCTime, HasConfig a1 a3, HasTasks a3 [Task],
              HasEvents a3 [Event]) =>
             m (Set Task)
planDay = do
  env <- ask
  let UTCTime day t = env^.situation.time
      ts' = env^.config.tasks
      es = env^.config.events
      midnight' = TimeOfDay 23 59 59
      timeNow = timeToTimeOfDay t
      xs =
        sortOn (view importance) $
        ts' <>
        fmap
          ((eventToTask day .) $ \e ->
             if e^.scheduled.start < timeToTimeOfDay t
               then set (scheduled.start) (timeToTimeOfDay t) e
               else e)
          (filter ((== day) . eventDate) es)
      f n ts =
        case n^.scheduled of
          Just _ -> insert n ts
          Nothing ->
            let need =
                  diffTimeToPicoseconds (n^.timeNeeded) `div`
                  diffDays (n^.deadline) day
                attemptInsert i
                  | i + 1 >= length ts = ts
                  | convert planEnd - convert planStart >= need =
                    insert (set scheduled (Just plannedTimeRange) n) ts
                  | otherwise = attemptInsert (i + 1)
                  where
                    planStart = view end $ fromJust $ view scheduled $ elemAt i ts
                    planEnd = view start $ fromJust $ view scheduled $ elemAt (i + 1) ts
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    plannedTimeRange =
                      TimeRange planStart $
                      timeToTimeOfDay $
                      timeOfDayToTime planStart + picosecondsToDiffTime need
             in attemptInsert 0

  return $ foldr
    f
    (fromList
       [ Task (Just $ TimeRange timeNow timeNow) 0 0 day "Now"
       , Task (Just $ TimeRange midnight' midnight') 0 0 day "Midnight"
       ])
    xs

printPlan :: (MonadReader a1 m, HasSituation a1 a2,
                HasTime a2 UTCTime, HasConfig a1 a3, HasTasks a3 [Task],
                HasEvents a3 [Event], MonadIO m) =>
               m ()
printPlan = do
  d <- planDay
  forM_ d $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        liftIO $ putStrLn $ f s <> "-" <> f e <> ": " <> n
