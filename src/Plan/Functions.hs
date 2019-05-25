{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Functions where

import Control.Lens
import Data.List
import Data.Time
import Data.Yaml
import GHC.IO.Exception
import Plan.Env
import Plan.Event
import Plan.Task
import Plan.TimeRange
import Prelude
import RIO hiding ((^.), over, set, view)
import System.Directory
import System.IO.Error

getID :: (MonadReader a1 m, HasIdentifier a2 Int, HasTasks a1 [a2]) => m Int
getID = do
  env <- ask
  let f :: HasIdentifier a Int => [a] -> [Int]
      f = fmap (^. identifier)
      ids = f (env ^. tasks)
  return $
    if null ids
      then 1
      else maximum ids + 1

addTask' ::
     ( HasTasks s [Task]
     , HasConfigLocation s String
     , MonadIO m
     , MonadReader s m
     , Integral a
     , HasTime s UTCTime
     )
  => Maybe TimeRange
  -> String
  -> Int
  -> a
  -> DiffTime
  -> m ()
addTask' s n i d t = do
  env <- ask
  when (isNothing s) $ liftIO $ putStrLn $ "Adding task '" <> n <> "'"
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
  setConfig $ Config $ new : env ^. tasks

addTask ::
     ( HasTasks s [Task]
     , HasConfigLocation s String
     , MonadIO m
     , MonadReader s m
     , HasTime s UTCTime
     )
  => Maybe TimeRange
  -> OptTask
  -> m ()
addTask s (OptTask n i d t) =
  addTask' s n i d $ picosecondsToDiffTime (round $ t * 3600 * 10 ^ (12 :: Int))

addEvent ::
     ( MonadIO m
     , HasTasks s [Task]
     , HasConfigLocation s String
     , MonadReader s m
     , HasTime s UTCTime
     )
  => OptEvent
  -> m ()
addEvent (OptEvent n d s e) = do
  let f = (<> ":00")
      s' = f s
      e' = f e
  case liftA2 TimeRange (readMaybe s') (readMaybe e') of
    Just r -> do
      liftIO $ putStrLn $ "Adding event '" <> n <> "'"
      addTask' (Just r) n maxBound d $ timeRangeSize r
    Nothing ->
      liftIO $
      ioError $
      userError "Input time in the format hh:mm. Examples: 07:58, 18:08."

getItem ::
     (MonadIO m, HasConfigLocation a String, MonadReader a m) => Int -> m Task
getItem i = do
  Config c <- getConfig
  n <-
    case findIndex ((== i) . view identifier) c of
      Just x -> return x
      Nothing -> liftIO (noSuchIndex i) >> return 0
  return $ c !! n

startWork ::
     (MonadReader s m, MonadIO m, HasConfigLocation s String, HasTime s UTCTime)
  => Int
  -> m ()
startWork i = do
  env <- ask
  Config c <- getConfig
  item <- getItem i
  when (isJust $ item ^. workingFrom) $
    liftIO $ ioError $ userError "already working on this task"
  when (isJust $ item ^. scheduled) $
    liftIO $ ioError $ userError "this is an event, not a task"
  liftIO $ putStrLn $ "Starting task '" <> item ^. name <> "'"
  setConfig $
    Config $
    set
      (ix i . workingFrom)
      (Just $ timeToTimeOfDay $ utctDayTime $ env ^. time)
      c

stopWork ::
     (HasConfigLocation s String, MonadIO m, MonadReader s m, HasTime s UTCTime)
  => Int
  -> m ()
stopWork i = do
  env <- ask
  Config c <- getConfig
  item <- getItem i
  case item ^. workingFrom of
    Just x -> do
      liftIO $ putStrLn $ "Starting task '" <> item ^. name <> "'"
      setConfig $
        Config $
        set (ix i . workingFrom) Nothing $
        over
          (ix i . workedToday)
          (++ [TimeRange x (timeToTimeOfDay $ utctDayTime $ env ^. time)])
          c
    Nothing -> liftIO $ ioError $ userError "you are not working on this"

noSuchIndex :: Show a1 => a1 -> IO a2
noSuchIndex i =
  ioError $
  mkIOError NoSuchThing "task/event" Nothing (Just $ show i)

removeItem ::
     (MonadReader s m, MonadIO m, HasConfigLocation s String, HasTasks s [Task])
  => Int
  -> m ()
removeItem i = do
  env <- ask
  item <- getItem i
  liftIO $ putStrLn $ "Removed task '" <> item ^. name <> "'"
  setConfig $ Config $ filter (/= item) $ env ^. tasks

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
