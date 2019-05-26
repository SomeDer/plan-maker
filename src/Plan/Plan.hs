{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Plan where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (sortOn)
import Data.Maybe
import Data.Set (Set, elemAt, fromList, insert, toList)
import Data.Time
import Plan.Env
import Plan.Functions
import Plan.Task
import Plan.TimeRange

planDay ::
     (MonadReader a m, HasTasks a [Task], HasTime a UTCTime) => m (Set Task)
planDay = do
  env <- ask
  let UTCTime day t = env ^. time
      ts' = env ^. tasks
      xs = filter ((day <) . view deadline) $ sortOn (view importance) ts'
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
                    convert = diffTimeToPicoseconds . timeOfDayToTime
                    planPart p i' =
                      view p $ fromJust $ view scheduled $ elemAt (i + i') ts
                    planStart = planPart end 0
                    planEnd = planPart start 1
                    plannedTimeRange =
                      TimeRange planStart $
                      timeToTimeOfDay $
                      timeOfDayToTime planStart + picosecondsToDiffTime need
             in attemptInsert 0
  let dummyTask n ti = Task (Just $ TimeRange ti ti) 0 0 day n 0 [] Nothing
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
  Config ts <- get
  d <- planDay
  let day = utctDay $ env ^. time
      ts' = filter ((day >=) . view deadline) ts
  forM_ d $ \(Task (Just (TimeRange s e)) _ _ _ n i _ _) ->
    let f = take 5 . show
     in if i == 0
          then return ""
          else return $ show i <> ") " <> f s <> "-" <> f e <> ": " <> n
  fmap (init . unlines . filter (/= "")) $
    if null ts'
      then forM (toList d) $ \(Task (Just (TimeRange s e)) _ _ _ n i _ _) ->
             let f = take 5 . show
              in return $
                 if i == 0
                   then ""
                   else show i <> ") " <> f s <> "-" <> f e <> ": " <> n
      else do
        a <- mapM removeItem $ fmap (view identifier) ts'
        return $ "Some tasks were finished and are going to be removed." : a
