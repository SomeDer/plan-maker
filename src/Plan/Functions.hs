module Plan.Functions where

import Data.Has
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
     (Has [Task] env, Has [Event] env, Has ConfigFile env, Has CurrentTime env)
  => OptTask
  -> RIO env ()
addTask (OptTask n i d t) = do
  env <- ask
  liftIO $ putStrLn $ "Adding task '" <> n <> "'"
  let new =
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays (toInteger d) $ utctDay (getTime $ getter env))
          n
  setConfig $ Config (new : getter env) $ getter env

addEvent ::
     (Has [Task] env, Has [Event] env, Has ConfigFile env, Has CurrentTime env)
  => OptEvent
  -> RIO env ()
addEvent (OptEvent n d s e) = do
  env <- ask
  let f = (<> ":00")
      s' = f s
      e' = f e
  case liftA2 TimeRange (readMaybe s') (readMaybe e') of
    Just r -> do
      liftIO $ putStrLn $ "Adding event '" <> n <> "'"
      let new = Event n (addDays d $ utctDay $ getTime $ getter env) r
      setConfig $ Config (getter env) $ new : getter env
    Nothing ->
      liftIO $
      ioError $
      userError "Input time in the format hh:mm. Examples: 07:58, 18:08."

removeItem ::
     (Has ConfigFile env, Has [Task] env, Has [Event] env)
  => String
  -> RIO env ()
removeItem n = do
  env <- ask
  let noName f = filter ((/= n) . f) $ getter env
      t = noName taskName
      e = noName eventName
  liftIO $
    if t == getter env && e == getter env
      then ioError $
           mkIOError NoSuchThing ("task/event '" <> n <> "'") Nothing Nothing
      else putStrLn $ "Removed '" <> n <> "'"
  setConfig $ Config t e

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
