{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Time
import Options.Applicative
import Plan.Env
import Plan.Event
import Plan.Functions
import Plan.Plan
import Plan.Task
import System.Directory
import System.Environment
import System.Exit

nameOpt :: Parser String
nameOpt = strOption (long "name" <> short 'n')

idOpt :: Parser Int
idOpt =
  option
    auto
    (long "id" <> short 'i' <>
     help "Task/event ID. This is shown to the left of its scheduled time.")

taskOpts :: Parser OptTask
taskOpts =
  OptTask <$> nameOpt <*>
  option
    auto
    (long "importance" <> short 'i' <> value 0 <> help "Task importance") <*>
  option
    auto
    (long "deadline" <> short 'd' <> value 0 <> help "Days until deadline") <*>
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

opts ::
     (MonadReader Env m, MonadState Config m, MonadError String m)
  => Parser (m String)
opts =
  hsubparser $
  command
    "task"
    (info (addTask Nothing <$> taskOpts) (progDesc "Add a new task")) <>
  command "event" (info (addEvent <$> eventOpts) (progDesc "Add a new event")) <>
  command "plan" (info (pure printPlan) (progDesc "Print the plan")) <>
  command "rm" (info (removeItem <$> idOpt) (progDesc "Remove task")) <>
  command
    "start"
    (info (startWork <$> idOpt) (progDesc "Start working on a task")) <>
  command "stop" (info (stopWork <$> idOpt) (progDesc "Stop working on a task"))

main :: IO ()
main = do
  t <- getCurrentTime
  home <- getHomeDirectory
  let save = home <> "/.plan.yaml"
      sit = Situation save t
  c@(Config ts) <- runReaderT getConfig sit
  let env = Env (Config ts) sit
  args <- getArgs
  if null args
    then do
      (a, s) <- flip runStateT c $ runExceptT $ runReaderT printPlan env
      err a
      runReaderT (setConfig s) sit
    else do
      (a, s) <-
        do p <- execParser (info (opts <**> helper) idm)
           flip runStateT c $ runExceptT $ runReaderT p env
      err a
      runReaderT (setConfig s) sit

err :: Either String String -> IO ()
err (Left l) = putStrLn l >> exitFailure
err (Right r) = putStrLn r
