module Main where

import Control.Monad
import Data.Time
import Options.Applicative
import Plan.Env
import Plan.Plan
import Plan.Task.Functions
import Plan.Task.Type
import Prelude (putStrLn)
import RIO
import System.Directory
import System.Environment

nameOpt :: Parser String
nameOpt = strOption (long "name" <> short 'n' <> help "Name of the task")

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

main :: IO ()
main = do
  time <- getCurrentTime
  home <- getHomeDirectory
  let save = home <> "/.plan.json"
      sit = Situation (ConfigFile save) (CurrentTime time)
  ts <- runRIO sit getTasks
  let env = Env ts [] sit
      opts =
        hsubparser $
        command "add" (info (addTask <$> taskOpts) (progDesc "Add a new task")) <>
        command "plan" (info (pure printPlan) (progDesc "Print the plan")) <>
        command "rm" (info (removeTask <$> nameOpt) (progDesc "Remove task"))
  args <- getArgs
  case args of
    "add":_ -> return ()
    _ ->
      when (null ts) $ do
        putStrLn "You don't have any tasks defined."
        putStrLn "Run plan add --help to see how to add them."
        exitFailure
  if null args
    then runRIO env printPlan
    else runRIO env =<< execParser (info (opts <**> helper) idm)
