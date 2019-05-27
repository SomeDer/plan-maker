{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.List (sortOn)
import Data.Maybe
import Data.Set (Set, elemAt, fromList, insert, toList)
import Data.Time
import Plan.Env
import Plan.Event
import Plan.Functions
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

timeWorked :: Task -> DiffTime -> DiffTime
timeWorked n t =
  sum $
  fmap timeRangeSize $
  mappend (n ^. workedToday) $
  maybe [] ((: []) . flip TimeRange (timeToTimeOfDay t)) $ n ^. workingFrom

timeNeededToday :: UTCTime -> Task -> Integer
timeNeededToday (UTCTime day t) n =
  if daysLeft == 0
    then 0
    else flip div daysLeft $
         subtract (diffTimeToPicoseconds $ timeWorked n t) $
         diffTimeToPicoseconds $ n ^. timeNeeded
  where
    daysLeft = diffDays (n ^. deadline) day + 1

planDay ::
     (MonadReader a m, HasTasks a [Task], HasTime a UTCTime) => m (Set Task)
planDay = do
  env <- ask
  let UTCTime day t = env ^. time
      ts' = env ^. tasks
      xs =
        filter
          (\x ->
             (bool (==) (<=) $ isNothing $ x ^. scheduled) day $ x ^. deadline) $
        sortOn (view importance) ts'
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
            let need = timeNeededToday (UTCTime day t) n
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
  let dummyTask n ti =
        Task (Just $ TimeRange ti ti) 0 0 day n Nothing 0 [] Nothing
  return $
    foldr
      f
      (fromList
         [ dummyTask "Now" $ timeToTimeOfDay t
         , dummyTask "Midnight" $ TimeOfDay 23 59 59
         ])
      xs

printPlan ::
     ( MonadState Config m
     , MonadError String m
     , MonadReader a m
     , HasTasks a [Task]
     , HasTime a UTCTime
     )
  => m String
printPlan = do
  env <- ask
  c <- get
  d <- fmap toList planDay
  let UTCTime day t = env ^. time
      toRemove =
        flip filter (c ^. tasks) $ \x ->
          day > (x ^. deadline) || x ^. timeNeeded <= 0
      finished = filter ((<= 0) . timeNeededToday (UTCTime day t)) $ c ^. tasks
      aboutFinished =
        bool "Some tasks are finished for today:" "" (null finished) :
        fmap (displayTask False) finished
  put $
    if c ^. todayIs == day
      then c
      else set todayIs day $
           flip (over tasks) c $
           fmap $ \task -> over timeNeeded (subtract $ timeWorked task t) task
  forM_ finished $ \x -> do
    _ <-
      if isJust $ x ^. workingFrom
        then stopWork $ x ^. identifier
        else return ""
    if isNothing $ x ^. scheduled
      then case x ^. recur of
             Nothing -> return ""
             Just (dead, ti) ->
               addTask' Nothing (x ^. name) (x ^. importance) dead True ti
      else return ""
  forM_ (filter (isJust . view scheduled) finished) $ \x ->
    case x ^. recur of
      Nothing -> return ""
      Just (days, _) ->
        case getSchedule x of
          (Just s, Just e) ->
            addEvent $ OptEvent (x ^. name) (fromIntegral days) (f s) (f e) True
          _ -> return ""
        where f = take 5 . show
  fmap (init' . unlines . filter (/= "")) $
    if null toRemove
      then if length d <= 2
             then throwError $
                  "You don't have any tasks/events for today.\n" <>
                  "Run plan task --help or plan event --help to see how to add them."
             else return $ fmap (displayTask True) d <> aboutFinished
      else do
        a <- mapM removeItem $ fmap (view identifier) toRemove
        return $ "Some tasks were finished and are going to be removed." : a

init' :: [a] -> [a]
init' [] = []
init' xs = init xs
