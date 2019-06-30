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
import Plan.Plan
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
      else fromIntegral (maximum ids) + 1

addTask' ::
     (MonadReader a1 m, MonadState Config m, HasTime a1 LocalTime)
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
          (addDays (toInteger d) $ localDay $ env ^. time)
          n
          (bool Nothing (Just (d, t)) r)
          (fromIntegral taskId)
          []
          Nothing
  put $ over tasks (new :) c
  return $ "Adding task " <> show n

addTask ::
     (MonadReader a1 m, MonadState Config m, HasTime a1 LocalTime)
  => Maybe TimeRange
  -> OptTask
  -> m String
addTask s (OptTask n i d t r) =
  addTask' s n i d r $
  picosecondsToDiffTime (round $ t * 3600 * 10 ^ (12 :: Int))

addEvent ::
     ( MonadReader a1 m
     , MonadState Config m
     , HasTime a1 LocalTime
     , MonadError String m
     )
  => OptEvent
  -> m String
addEvent (OptEvent n d s e r) = do
  let f = (<> ":00")
      s' = f s
      e' = f e
      g (TimeOfDay h m sec) = isJust $ makeTimeOfDayValid h m sec
  case liftM2 TimeRange (readMaybe s') (readMaybe e') of
    Just range@(TimeRange st en) -> do
      _ <-
        addTask' (Just range) n maxBound (fromIntegral d) r $
        timeRangeSize range
      if g st && g en
        then return $ "Adding event " <> show n
        else throwError
               "The range for hours is 0 - 23 and the range for minutes is 0 to 59"
    Nothing ->
      throwError "Input time in the format hh:mm. Examples: 07:58, 18:00."

getIndex :: (MonadState Config m, MonadError String m) => Int -> m (Task, Int)
getIndex i = do
  c <- get
  n <-
    case findIndex ((== fromIntegral i) . view identifier) $ c ^. tasks of
      Just x -> return x
      Nothing -> noSuchIndex i
  return ((c ^. tasks) !! n, n)

startWork ::
     ( MonadState Config m
     , MonadReader s m
     , MonadError String m
     , HasTime s LocalTime
     )
  => Int
  -> m String
startWork i = do
  env <- ask
  c <- get
  (item, n) <- getIndex i
  put $
    flip (over tasks) c $
    set (ix n . workingFrom) $ Just $ localTimeOfDay $ env ^. time
  if isJust $ item ^. workingFrom
    then throwError "Already working on this task"
    else if isJust $ item ^. scheduled
           then throwError "This is an event, not a task"
           else return $ "Starting task '" <> item ^. name <> "'"

startNext ::
     ( MonadError String m
     , MonadState Config m
     , MonadReader s m
     , HasTime s LocalTime
     )
  => m String
startNext = do
  env <- ask
  c <- get
  _ <- printPlan
  let t = head $ planDay (env ^. time) (c ^. tasks)
  startWork $ fromIntegral $ t ^. identifier

stopWork ::
     ( MonadError String m
     , MonadReader s m
     , MonadState Config m
     , HasTime s LocalTime
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
          (++ [TimeRange x $ localTimeOfDay $ env ^. time])
      return $ "Stopping task " <> show (item ^. name)
    Nothing -> throwError "You are not working on this"

stopAll ::
     ( MonadState Config m
     , MonadError String m
     , HasTime s LocalTime
     , MonadReader s m
     )
  => m String
stopAll = do
  c <- get
  fmap (init . unlines) $
    forM (filter (isJust . view workingFrom) $ c ^. tasks) $
    stopWork . fromIntegral . view identifier

noSuchIndex :: (MonadError String m, Show a1) => a1 -> m a2
noSuchIndex i = throwError $ "There is no task/event with index " <> show i

removeItem :: (MonadState Config m, MonadError String m) => Int -> m String
removeItem i = do
  c <- get
  (item, _) <- getIndex i
  put $ over tasks (filter (/= item)) c
  return $ "Removing " <> show (item ^. name)

getConfig ::
     ( MonadReader a m
     , MonadIO m
     , HasConfigLocation a String
     , HasTime a LocalTime
     )
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
      setConfig $ Config [] $ localDay $ env ^. time
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
  let tim@(LocalTime day t) = env ^. time
      finished = fst $ finishedUnfinished tim $ c ^. tasks
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
  let d = planDay tim (c' ^. tasks)
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

runMonads :: ReaderT Env (ExceptT String (StateT Config IO)) String -> IO ()
runMonads f = do
  t <- fmap zonedTimeToLocalTime getZonedTime
  home <- getHomeDirectory
  let save = home <> "/.plan.yaml"
      sit = Situation save t
  c <- runReaderT getConfig sit
  let env = Env c sit
  (a, s) <- runMonads' f c env
  msg a
  runReaderT (setConfig s) sit

runMonads' ::
     ReaderT Env (ExceptT String (StateT Config m)) String
  -> Config
  -> Env
  -> m (Either String String, Config)
runMonads' f c = flip runStateT c . runExceptT . runReaderT f

msg :: Either String String -> IO ()
msg (Left l) = putStrLn l >> exitFailure
msg (Right r) = putStrLn r
