{-# LANGUAGE NoMonomorphismRestriction #-}

module Plan.Functions where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.List
import Data.Maybe
import Data.Time
import Data.Yaml
import Plan.Env
import Plan.Event
import Plan.Task
import Plan.TimeRange
import System.Directory
import System.Exit
import Text.Read (readMaybe)

getID :: (MonadState Config m) => m Int
getID = do
  env <- get
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
     , HasTime a1 UTCTime
     )
  => Maybe TimeRange
  -> String
  -> Int
  -> Int
  -> Bool
  -> DiffTime
  -> m String
addTask' s n i d r t = do
  env <- ask
  c <- get
  taskId <- getID
  let new =
        Task
          s
          t
          i
          (addDays (toInteger d) $ utctDay $ env ^. time)
          n
          (bool Nothing (Just (d, t)) r)
          taskId
          []
          Nothing
  put $ over tasks (new :) c
  return $
    if isNothing s
      then "Adding task " <> show n
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
addTask s (OptTask n i d t r) =
  addTask' s n i d r $
  picosecondsToDiffTime (round $ t * 3600 * 10 ^ (12 :: Int))

addEvent ::
     ( MonadReader a1 m
     , MonadState Config m
     , HasTasks a1 [Task]
     , HasTime a1 UTCTime
     , MonadError String m
     )
  => OptEvent
  -> m String
addEvent (OptEvent n d s e r) = do
  let f = (<> ":00")
      s' = f s
      e' = f e
  case liftM2 TimeRange (readMaybe s') (readMaybe e') of
    Just range -> do
      _ <-
        addTask' (Just range) n maxBound (fromIntegral d) r $
        timeRangeSize range
      return $ "Adding event " <> show n
    Nothing ->
      throwError "Input time in the format hh:mm. Examples: 07:58, 18:00."

getIndex :: (MonadState Config m, MonadError String m) => Int -> m (Task, Int)
getIndex i = do
  c <- get
  n <-
    case findIndex ((== i) . view identifier) $ c ^. tasks of
      Just x -> return x
      Nothing -> noSuchIndex i
  return ((c ^. tasks) !! n, n)

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
    flip (over tasks) c $
    set (ix n . workingFrom) $
    Just $ timeToTimeOfDay $ utctDayTime $ env ^. time
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
        flip (over tasks) c $
        set (ix n . workingFrom) Nothing .
        over
          (ix n . workedToday)
          (++ [TimeRange x $ timeToTimeOfDay $ utctDayTime $ env ^. time])
      return $ "Stopping task " <> show (item ^. name)
    Nothing -> throwError "You are not working on this"

noSuchIndex :: (MonadError String m, Show a1) => a1 -> m a2
noSuchIndex i = throwError $ "There is no task/event with index " <> show i

removeItem :: (MonadState Config m, MonadError String m) => Int -> m String
removeItem i = do
  c <- get
  (item, _) <- getIndex i
  put $ over tasks (filter (/= item)) c
  return $ "Removing " <> show (item ^. name)

getConfig ::
     (MonadReader a m, MonadIO m, HasConfigLocation a String, HasTime a UTCTime)
  => m Config
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
    else do
      setConfig $ Config [] $ utctDay $ env ^. time
      liftIO $ do
        putStrLn "When do you go to sleep (hh:mm, e.g. 23:00)?"
        s <- getLine
        putStrLn "When do you wake up?"
        en <- getLine
        let fn x y = runMonads $ addEvent $ OptEvent "Sleep" 0 x y True
        fn "00:00" en
        fn s "23:59"
      getConfig

setConfig ::
     (MonadReader s m, MonadIO m, HasConfigLocation s String) => Config -> m ()
setConfig c = do
  env <- ask
  liftIO $ encodeFile (env ^. configLocation) c

runMonads :: ReaderT Env (ExceptT String (StateT Config IO)) String -> IO ()
runMonads f = do
  t <- getCurrentTime
  home <- getHomeDirectory
  let save = home <> "/.plan.yaml"
      sit = Situation save t
  c <- runReaderT getConfig sit
  let env = Env c sit
  (a, s) <- flip runStateT c $ runExceptT $ runReaderT f env
  msg a
  runReaderT (setConfig s) sit

msg :: Either String String -> IO ()
msg (Left l) = putStrLn l >> exitFailure
msg (Right r) = putStrLn r
