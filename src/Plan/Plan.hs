{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

import Control.Lens
import Data.List (sortOn)
import Data.Maybe
import Data.Set (elemAt, fromList, insert)
import Data.Time
import Plan.Env
import Plan.Task
import Plan.TimeRange
import Prelude (putStrLn)
import RIO hiding ((^.), over, set, view)

planDay ::
     (MonadReader a m, HasTime a UTCTime, HasTasks a [Task]) => m (Set Task)
planDay = do
  env <- ask
  let UTCTime day t = env ^. time
      ts' = env ^. tasks
      midnight' = TimeOfDay 23 59 59
      timeNow = timeToTimeOfDay t
      xs = sortOn (view importance) ts'
      f n ts =
        case n ^. scheduled of
          Just e
            | e ^. start >= timeToTimeOfDay t -> insert n ts
            | n ^. deadline /= day -> ts
            | e ^. end < timeToTimeOfDay t ->
              flip insert ts $ set identifier 0 n
            | otherwise ->
              flip insert ts $
              set (scheduled . _Just . start) (timeToTimeOfDay t) n
          Nothing ->
            let m =
                  (n &) $
                  over timeNeeded $
                  subtract $
                  sum $
                  fmap timeRangeSize $
                  mappend (n ^. workedToday) $
                  maybe [] ((: []) . flip TimeRange (timeToTimeOfDay t)) $
                  n ^. workingFrom
                need =
                  diffTimeToPicoseconds (m ^. timeNeeded) `div`
                  diffDays (m ^. deadline) day
                attemptInsert i
                  | i + 1 >= length ts = ts
                  | convert planEnd - convert planStart >= need =
                    insert (set scheduled (Just plannedTimeRange) m) ts
                  | otherwise = attemptInsert (i + 1)
                  where
                    planStart =
                      view end $ fromJust $ view scheduled $ elemAt i ts
                    planEnd =
                      view start $ fromJust $ view scheduled $ elemAt (i + 1) ts
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    plannedTimeRange =
                      TimeRange planStart $
                      timeToTimeOfDay $
                      timeOfDayToTime planStart + picosecondsToDiffTime need
             in attemptInsert 0
  return $
    foldr
      f
      (fromList
         [ Task (Just $ TimeRange timeNow timeNow) 0 0 day "Now" 0 [] Nothing
         , Task
             (Just $ TimeRange midnight' midnight')
             0
             0
             day
             "Midnight"
             0
             []
             Nothing
         ])
      xs

printPlan ::
     ( MonadReader a m
     , HasConfigLocation a String
     , HasTime a UTCTime
     , HasTasks a [Task]
     , MonadIO m
     )
  => m ()
printPlan
    {-
  mapM_ removeItem $ fmap (^. identifier) $
    filter
      (\e -> e ^. scheduled . end < timeToTimeOfDay (utctDayTime $ env ^. time)) $
    env ^.
    events
    -}
 = do
  d <- planDay
  forM_ d $ \(Task (Just (TimeRange s e)) _ _ _ n i _ _) ->
    let f = take 5 . show
     in unless (i == 0) $
        liftIO $ putStrLn $ show i <> ") " <> f s <> "-" <> f e <> ": " <> n
