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

addTask ::
     ( MonadReader s m
     , MonadIO m
     , HasConfigLocation s String
     , HasTasks s [Task]
     , HasEvents s [Event]
     , HasTime s UTCTime
     )
  => OptTask
  -> m ()
addTask (OptTask n i d t) = do
  env <- ask
  liftIO $ putStrLn $ "Adding task '" <> n <> "'"
  let new =
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays (toInteger d) $ utctDay (env ^. time))
          n
  setConfig $ Config (new : env ^. tasks) $ env ^. events

addEvent ::
     ( MonadReader s m
     , MonadIO m
     , HasConfigLocation s String
     , HasTasks s [Task]
     , HasEvents s [Event]
     , HasTime s UTCTime
     )
  => OptEvent
  -> m ()
addEvent (OptEvent n d s e) = do
  env <- ask
  let f = (<> ":00")
      s' = f s
      e' = f e
  case liftA2 TimeRange (readMaybe s') (readMaybe e') of
    Just r -> do
      liftIO $ putStrLn $ "Adding event '" <> n <> "'"
      let new = Event n (addDays d $ utctDay $ env ^. time) r
      setConfig $ Config (env ^. tasks) $ new : env ^. events
    Nothing ->
      liftIO $
      ioError $
      userError "Input time in the format hh:mm. Examples: 07:58, 18:08."

removeItem ::
     ( MonadReader s m
     , MonadIO m
     , HasConfigLocation s String
     , HasTasks s [Task]
     , HasEvents s [Event]
     )
  => String
  -> m ()
removeItem n = do
  env <- ask
  let noName f = filter ((/= n) . view name) $ env ^. f
      t = noName tasks
      e = noName events
  liftIO $
    if t == env ^. tasks && e == env ^. events
      then ioError $
           mkIOError NoSuchThing ("task/event '" <> n <> "'") Nothing Nothing
      else putStrLn $ "Removed '" <> n <> "'"
  setConfig $ Config t e

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
    else return $ Config [] []

setConfig ::
     (MonadReader s m, MonadIO m, ToJSON a, HasConfigLocation s String)
  => a
  -> m ()
setConfig c = do
  env <- ask
  liftIO $ encodeFile (env ^. configLocation) c
