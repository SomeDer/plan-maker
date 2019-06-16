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

planDay :: LocalTime -> [Task] -> Set Task
planDay (LocalTime day t) ts' =
  let xs =
        sortOn (view importance) ts' & flip filter $ \x ->
          case x ^. scheduled of
            Nothing -> day <= x ^. deadline
            Just (TimeRange _ e) ->
              day == x ^. deadline && e >= t
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
            let need = timeNeededToday (LocalTime day t) n
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
   in foldr
        f
        (fromList
           [ dummyTask "Now" t
           , dummyTask "Midnight" $ TimeOfDay 23 59 59
           ])
        xs

printPlan ::
     ( MonadError String m
     , MonadState Config m
     , MonadReader a m
     , HasTime a LocalTime
     )
  => m String
printPlan = do
  env <- ask
  c <- get
  let LocalTime day t = env ^. time
      finished = filter ((<= 0) . timeNeededToday (LocalTime day t)) $ c ^. tasks
      toRemove =
        flip filter (c ^. tasks) $ \x ->
          day > (x ^. deadline) || x ^. timeNeeded <= 0
      aboutFinished =
        bool "Some tasks are finished for today:" "" (null finished) :
        fmap (displayTask False) finished
  put $
    if c ^. todayIs == day
      then c
      else set todayIs day $
           flip (over tasks) c $
           fmap $ \task ->
             set workingFrom Nothing $
             set workedToday [] $
             over timeNeeded (subtract $ timeWorked task t) task
  forM_ finished $ \x ->
    flip catchError (const $ return "") $
    stopWork $ fromIntegral $ x ^. identifier
  forM_ toRemove $ \x ->
    case x ^. recur of
      Just (dead, ti) ->
        case getSchedule x of
          (Just s, Just e) ->
            addEvent $ OptEvent (x ^. name) (fromIntegral dead) (f s) (f e) True
          _ -> addTask' Nothing (x ^. name) (x ^. importance) dead True ti
        where f = take 5 . show
      Nothing -> return ""
  c' <- get
  let d = toList $ planDay (env ^. time) (c' ^. tasks)
  fmap (init' . unlines . filter (/= "")) $
    if null toRemove
      then if null finished && length d <= 2
             then throwError $
                  "You don't have any tasks/events for today.\n" <>
                  "Run plan task --help or plan event --help to see how to add them."
             else return $ fmap (displayTask True) d <> aboutFinished
      else do
        a <- mapM removeItem $ fmap (fromIntegral . view identifier) toRemove
        return $ "Some tasks were finished and are going to be removed." : a

init' :: [a] -> [a]
init' [] = []
init' xs = init xs
