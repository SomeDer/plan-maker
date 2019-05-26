{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Functions where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Time
import Data.Yaml
import Plan.Env
import Plan.Event
import Plan.Task
import Plan.TimeRange
import Prelude
import System.Directory
import System.Exit
import Text.Read (readMaybe)

getID :: (MonadReader a1 m, HasTasks a1 [Task]) => m Int
getID = do
  env <- ask
  let f = fmap (view identifier)
      ids = f (env ^. tasks)
  return $
    if null ids
      then 1
      else maximum ids + 1

addTask' ::
     ( MonadReader a1 m
     , MonadState Config m
     , HasTasks a1 [Task]
     , Integral a
     , HasTime a1 UTCTime
     )
  => Maybe TimeRange
  -> String
  -> Int
  -> a
  -> DiffTime
  -> m String
addTask' s n i d t = do
  env <- ask
  Config c <- get
  taskId <- getID
  let new =
        Task
          s
          t
          i
          (addDays (toInteger d) $ utctDay (env ^. time))
          n
          taskId
          []
          Nothing
  put $ Config $ new : c
  return $
    if isNothing s
      then "Adding task '" <> n <> "'"
      else ""

addTask ::
     ( MonadReader a1 m
     , MonadState Config m
     , HasTasks a1 [Task]
     , HasTime a1 UTCTime
     )
  => Maybe TimeRange
  -> OptTask
  -> m String
addTask s (OptTask n i d t) =
  addTask' s n i d $ picosecondsToDiffTime (round $ t * 3600 * 10 ^ (12 :: Int))

addEvent ::
     ( MonadReader a1 m
     , MonadState Config m
     , HasTasks a1 [Task]
     , HasTime a1 UTCTime
     , MonadError String m
     )
  => OptEvent
  -> m String
addEvent (OptEvent n d s e) = do
  let f = (<> ":00")
      s' = f s
      e' = f e
  case liftM2 TimeRange (readMaybe s') (readMaybe e') of
    Just r -> do
      _ <- addTask' (Just r) n maxBound d $ timeRangeSize r
      return $ "Adding event '" <> n <> "'"
    Nothing ->
      throwError "Input time in the format hh:mm. Examples: 07:58, 18:00."

getIndex :: (MonadState Config m, MonadError String m) => Int -> m (Task, Int)
getIndex i = do
  Config c <- get
  n <-
    case findIndex ((== i) . view identifier) c of
      Just x -> return x
      Nothing -> noSuchIndex i
  return (c !! n, n)

startWork ::
     ( MonadState Config m
     , MonadReader s m
     , MonadError String m
     , HasTime s UTCTime
     )
  => Int
  -> m String
startWork i = do
  env <- ask
  c <- get
  (item, n) <- getIndex i
  put $
    Config $
    set
      (ix n . workingFrom)
      (Just $ timeToTimeOfDay $ utctDayTime $ env ^. time) $
    c ^. tasks
  if isJust $ item ^. workingFrom
    then throwError "Already working on this task"
    else if isJust $ item ^. scheduled
           then throwError "This is an event, not a task"
           else return $ "Starting task '" <> item ^. name <> "'"

stopWork ::
     ( MonadError String m
     , MonadReader s m
     , MonadState Config m
     , HasTime s UTCTime
     )
  => Int
  -> m String
stopWork i = do
  env <- ask
  c <- get
  (item, n) <- getIndex i
  case item ^. workingFrom of
    Just x -> do
      put $
        Config $
        set (ix n . workingFrom) Nothing $
        over
          (ix n . workedToday)
          (++ [TimeRange x (timeToTimeOfDay $ utctDayTime $ env ^. time)]) $
        c ^. tasks
      return $ "Stopping task '" <> item ^. name <> "'"
    Nothing -> throwError "You are not working on this"

noSuchIndex :: (MonadError String m, Show a1) => a1 -> m a2
noSuchIndex i = throwError $ "There is no task/event with index " <> show i

removeItem :: (MonadState Config m, MonadError String m) => Int -> m String
removeItem i = do
  Config c <- get
  (item, _) <- getIndex i
  put $ Config $ filter (/= item) c
  return $ "Removed task '" <> item ^. name <> "'"

getConfig ::
     (MonadReader a m, MonadIO m, HasConfigLocation a String) => m Config
getConfig = do
  env <- ask
  let f = env ^. configLocation
  e <- liftIO $ doesFileExist f
  if e
    then liftIO $ do
           d <- decodeFileEither f
           case d of
             Left err -> putStrLn (prettyPrintParseException err) >> exitFailure
             Right x -> return x
    else return $ Config []

setConfig ::
     (MonadReader s m, MonadIO m, ToJSON a, HasConfigLocation s String)
  => a
  -> m ()
setConfig c = do
  env <- ask
  liftIO $ encodeFile (env ^. configLocation) c
