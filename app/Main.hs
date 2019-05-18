module Main where

import Control.Monad
import Data.Time
import Options.Applicative
import Plan.Env
import Plan.Event
import Plan.Plan
import Plan.Task.Functions
import Plan.Task.Type
import Prelude (putStrLn)
import RIO
import System.Directory
import System.Environment

nameOpt :: Parser String
nameOpt = strOption (long "name" <> short 'n')

taskOpts :: Parser OptTask
taskOpts =
  OptTask <$> nameOpt <*>
  option
    auto
    (long "importance" <> short 'i' <> value 0 <> help "Task importance") <*>
  option
    auto
    (long "deadline" <> short 'd' <> value 1 <> help "Days until deadline") <*>
  option
    auto
    (long "time" <> short 't' <> value 1 <> help "Hours needed to complete task")

eventOpts :: Parser OptEvent
eventOpts =
  OptEvent <$> nameOpt <*>
  option
    auto
    (long "days" <> short 'd' <> value 0 <>
     help "Days until event. 0 means it's happening today.") <*>
  strOption
    (long "start" <> short 's' <>
     help "Time when the event starts. Format: hh:mm") <*>
  strOption
    (long "end" <> short 'e' <> help "Time when the event ends. Format: hh:mm")

main :: IO ()
main = do
  time <- getCurrentTime
  home <- getHomeDirectory
  let save = home <> "/.plan.yaml"
      sit = Situation (ConfigFile save) (CurrentTime time)
  c@(Config ts es) <- runRIO sit getConfig
  let env = Env c sit
      opts =
        hsubparser $
        command "task" (info (addTask <$> taskOpts) (progDesc "Add a new task")) <>
        command
          "event"
          (info (addEvent <$> eventOpts) (progDesc "Add a new event")) <>
        command "plan" (info (pure printPlan) (progDesc "Print the plan")) <>
        command "rm" (info (removeItem <$> nameOpt) (progDesc "Remove task"))
  args <- getArgs
  case args of
    "task":_ -> return ()
    "event":_ -> return ()
    _ ->
      when (null ts && null es) $ do
        putStrLn "You don't have any tasks/events defined."
        putStrLn
          "Run plan task --help or plan event --help to see how to add them."
        exitFailure
  if null args
    then runRIO env printPlan
    else runRIO env =<< execParser (info (opts <**> helper) idm)
