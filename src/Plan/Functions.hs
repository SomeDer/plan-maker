module Plan.Functions where

import Data.Time
import Data.Yaml
import GHC.IO.Exception
import Plan.Env
import Plan.Event
import Plan.Task
import Plan.TimeRange
import Prelude
import RIO
import System.Directory
import System.IO.Error

getID :: (MonadReader s m, HasTasks s [Task]) => m Int
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
     ( MonadReader s m
     , MonadIO m
     , HasTasks s [Task]
     , HasConfigLocation s String
     , Integral a2
     , HasTime s UTCTime
     )
  => Maybe TimeRange
  -> String
  -> Int
  -> a2
  -> DiffTime
  -> m ()
addTask' s n i d t = do
  env <- ask
  when (isNothing s) $ liftIO $ putStrLn $ "Adding task '" <> n <> "'"
  taskId <- getID
  let new = Task s t i (addDays (toInteger d) $ utctDay (env ^. time)) n taskId [] Nothing
  setConfig $ Config $ new : env ^. tasks

addTask ::
     ( MonadReader s m
     , MonadIO m
     , HasTasks s [Task]
     , HasConfigLocation s String
     , HasTime s UTCTime
     )
  => Maybe TimeRange
  -> OptTask
  -> m ()
addTask s (OptTask n i d t) =
  addTask' s n i d $ picosecondsToDiffTime (round $ t * 3600 * 10 ^ (12 :: Int))

addEvent ::
     ( MonadReader s m
     , MonadIO m
     , HasConfigLocation s String
     , HasTasks s [Task]
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
      -- let new = Event n (addDays d $ utctDay $ env ^. time) r eventId
      addTask' (Just r) n maxBound d $ timeRangeSize r
    Nothing ->
      liftIO $
      ioError $
      userError "Input time in the format hh:mm. Examples: 07:58, 18:08."

removeItem ::
     (MonadReader s m, MonadIO m, HasConfigLocation s String, HasTasks s [Task])
  => Int
  -> m ()
removeItem i = do
  env <- ask
  let byID f g = filter (g i . view identifier) $ env ^. f
      ts = byID tasks (/=)
  liftIO $
    if ts == env ^. tasks
      then ioError $
           mkIOError
             NoSuchThing
             ("task/event with ID " <> show i)
             Nothing
             Nothing
      else case byID tasks (==) of
             [t] -> putStrLn $ "Removed task '" <> t ^. name <> "'"
             -- ([], [e]) -> putStrLn $ "Removed event '" <> e ^. name <> "'"
             _ ->
               error
                 "Mutliple tasks/events have the same ID. This is impossible."
  setConfig $ Config ts

getConfig :: (HasConfigLocation env FilePath) => RIO env Config
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
