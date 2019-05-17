module Plan.Task.Functions where

import Data.Aeson
import Data.Has
import Data.Time
import Plan.Env
import Plan.Task.Type
import Prelude (putStrLn, readFile, writeFile)
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
    then do
      t <- liftIO $ readFile f
      case decode =<< readMaybe t of
        Just y -> return y
        Nothing ->
          liftIO $ do
            putStrLn "You edited the plan.json file and now it doesn't work!"
            exitFailure
    else return []

setTasks :: (Has ConfigFile env) => [Task] -> RIO env ()
setTasks t = do
  env <- ask
  liftIO $ writeFile (getConfigFile $ getter env) $ show $ encode t
