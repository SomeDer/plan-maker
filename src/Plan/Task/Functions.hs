module Plan.Task.Functions where

import Data.Has
import Data.Time
import Data.Yaml
import Plan.Env
import Plan.Task.Type
import Prelude
import RIO
import System.Directory

addTask ::
     (Has [Task] env, Has ConfigFile env, Has CurrentTime env)
  => OptTask
  -> RIO env ()
addTask (OptTask n i d t) = do
  env <- ask
  let new =
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays (toInteger d) $ utctDay (getTime $ getter env))
          n
  setTasks $ new : getter env

removeTask :: (Has ConfigFile env, Has [Task] env) => String -> RIO env ()
removeTask n = setTasks . filter ((/= n) . taskName) . getter =<< ask

getTasks :: (Has ConfigFile env) => RIO env [Task]
getTasks = do
  env <- ask
  let f = getConfigFile $ getter env
  e <- liftIO $ doesFileExist f
  if e
    then liftIO $ do
           d <- decodeFileEither f
           case d of
             Left err -> putStrLn (prettyPrintParseException err) >> exitFailure
             Right x -> return x
    else return []

setTasks :: (Has ConfigFile env) => [Task] -> RIO env ()
setTasks t = do
  env <- ask
  liftIO $ encodeFile (getConfigFile $ getter env) t
