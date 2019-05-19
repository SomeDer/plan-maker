{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

import Data.List (sortOn)
import Data.Maybe
import Data.Set (elemAt, fromList, insert)
import Data.Time
import Plan.Env
import Plan.Event
import Plan.Functions
import Plan.Task
import Plan.TimeRange
import Prelude (putStrLn)
import RIO

planDay ::
     ( MonadReader a m
     , HasTime a UTCTime
     , HasTasks a [Task]
     , HasEvents a [Event]
     )
  => m (Set Task)
planDay = do
  env <- ask
  let UTCTime day t = env ^. time
      ts' = env ^. tasks
      es = env ^. events
      midnight' = TimeOfDay 23 59 59
      timeNow = timeToTimeOfDay t
      xs =
        sortOn (view importance) $ ts' <>
        fmap
          ((eventToTask day .) $ \e ->
             if e ^. scheduled . start >= timeToTimeOfDay t
               then e
               else if e ^. scheduled . end < timeToTimeOfDay t
                      then set identifier 0 e
                      else set (scheduled . start) (timeToTimeOfDay t) e)
          (filter ((== day) . eventDate) es)
      f n ts =
        case n ^. scheduled of
          Just _ -> insert n ts
          Nothing ->
            let need =
                  diffTimeToPicoseconds (n ^. timeNeeded) `div`
                  diffDays (n ^. deadline) day
                attemptInsert i
                  | i + 1 >= length ts = ts
                  | convert planEnd - convert planStart >= need =
                    insert (set scheduled (Just plannedTimeRange) n) ts
                  | otherwise = attemptInsert (i + 1)
                  where
                    planStart =
                      view end $ fromJust $ view scheduled $ elemAt i ts
                    planEnd =
                      view start $ fromJust $ view scheduled $ elemAt (i + 1) ts
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    plannedTimeRange =
                      TimeRange planStart $ timeToTimeOfDay $
                      timeOfDayToTime planStart +
                      picosecondsToDiffTime need
             in attemptInsert 0
  return $
    foldr
      f
      (fromList
         [ Task (Just $ TimeRange timeNow timeNow) 0 0 day "Now" 0
         , Task (Just $ TimeRange midnight' midnight') 0 0 day "Midnight" 0
         ])
      xs

printPlan ::
     ( MonadReader a m
     , HasConfigLocation a String
     , HasTime a UTCTime
     , HasTasks a [Task]
     , HasEvents a [Event]
     , MonadIO m
     )
  => m ()
printPlan = do
  env <- ask
  mapM_ removeItem $ fmap (^. identifier) $
    filter
      (\e -> e ^. scheduled . end < timeToTimeOfDay (utctDayTime $ env ^. time)) $
    env ^.
    events
  d <- planDay
  forM_ d $ \(Task (Just (TimeRange s e)) _ _ _ n i) ->
    let f = take 5 . show
     in unless (i == 0) $ liftIO $ putStrLn $ show i <> ") " <> f s <> "-" <>
        f e <>
        ": " <>
        n
