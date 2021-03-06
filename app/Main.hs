module Main where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Function
import Options.Applicative
import Plan.Env
import Plan.Event
import Plan.Functions
import Plan.Task

nameOpt :: Parser String
nameOpt = strOption $ long "name" <> short 'n'

recOpt :: Parser Bool
recOpt =
  switch $
  long "recur" <> short 'r' <>
  help
    "If passed, this would make the task restart once the deadline is reached, with the new deadline being as far away as the old."

idOpt :: Parser Int
idOpt =
  option
    auto
    (long "id" <> short 'i' <>
     help "Task/event ID. This is shown to the left of its scheduled time." <>
     metavar "INT")

taskOpts :: Parser OptTask
taskOpts =
  OptTask <$> nameOpt <*>
  option
    auto
    (long "importance" <> short 'i' <> value 0 <>
     help
       "Task importance. Can be any reasonable integer (positive or negative) and the default is 0. Tasks with a higher importance are done first." <>
     metavar "INT") <*>
  option
    auto
    (long "deadline" <> short 'd' <> value 0 <>
     help
       "Days until deadline. It is assumed that you can work on the last day, so a value of 0 (default) means that you will do all of it today (but it is not overdue)." <>
     metavar "DAYS") <*>
  option
    auto
    (long "time" <> short 't' <> value 1 <>
     help "Hours needed to complete the task. Default: 1." <>
     metavar "HOURS") <*>
  switch
    (long "recur" <> short 'r' <>
     help
       "If passed, this would make the task restart once the deadline is reached, with the new deadline being as far away as the old.")

eventOpts :: Parser OptEvent
eventOpts =
  OptEvent <$> nameOpt <*>
  option
    auto
    (long "days" <> short 'd' <> value 0 <>
     help "Days until event. 0 (default) means it's happening today.") <*>
  strOption
    (long "start" <> short 's' <>
     help "Time when the event starts. Format: hh:mm" <>
     metavar "TIME") <*>
  strOption
    (long "end" <> short 'e' <> help "Time when the event ends. Format: hh:mm" <>
     metavar "TIME") <*>
  recOpt

opts ::
     (MonadReader Env m, MonadState Config m, MonadError String m)
  => Parser (m String)
opts =
  pure printPlan & (<|>) $
  hsubparser $
  command
    "task"
    (info
       (addTask Nothing <$> taskOpts)
       (progDesc
          "Adds a task. A task can be done at any time when there isn't an event.")) <>
  command
    "event"
    (info
       (addEvent <$> eventOpts)
       (progDesc "Add a new event. An event has to be done at a specific time.")) <>
  command "plan" (info (pure printPlan) (progDesc "Print the plan")) <>
  command "rm" (info (removeItem <$> idOpt) (progDesc "Remove task")) <>
  command
    "start"
    (info (pure startNext) (progDesc "Start working on the next task")) <>
  command "stop" (info (pure stopAll) (progDesc "Stop working on all tasks"))

main :: IO ()
main = execParser (info (opts <**> helper) idm) >>= runMonads
