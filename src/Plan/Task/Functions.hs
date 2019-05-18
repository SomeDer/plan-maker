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
  setConfig $ Config (new : getter env) []

removeTask :: (Has ConfigFile env, Has [Task] env) => String -> RIO env ()
removeTask n =
  setConfig . flip Config [] . filter ((/= n) . taskName) . getter =<< ask

getConfig :: (Has ConfigFile env) => RIO env Config
getConfig = do
  env <- ask
  let f = getConfigFile $ getter env
  e <- liftIO $ doesFileExist f
  if e
    then liftIO $ do
           d <- decodeFileEither f
           case d of
             Left err -> putStrLn (prettyPrintParseException err) >> exitFailure
             Right x -> return x
    else return $ Config [] []

setConfig :: (Has ConfigFile env) => Config -> RIO env ()
setConfig c = do
  env <- ask
  liftIO $ encodeFile (getConfigFile $ getter env) c
