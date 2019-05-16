module Main where

import Control.Monad
import Data.Aeson
import Data.Time
import Options.Applicative
import Plan.Day
import Plan.Task
import Plan.TimeRange
import System.Directory
import System.Environment
import System.Exit
import Text.Read

data OptTask = OptTask
  { optName :: String
  , optImportance :: Int
  , optDeadline :: Int
  , optTime :: Double
  } deriving (Show)

nameOpt :: Parser String
nameOpt =
  strOption
    (long "name" <> short 'n' <> help "Name of the task" <> metavar "NAME")

taskOpts :: Parser OptTask
taskOpts =
  OptTask <$> nameOpt <*>
  option
    auto
    (long "importance" <> short 'i' <> value 0 <> help "Task importance" <>
     metavar "IMPORTANCE") <*>
  option
    auto
    (long "deadline" <> short 'd' <> value 1 <> help "Days until deadline" <>
     metavar "DEADLINE") <*>
  option
    auto
    (long "time" <> short 't' <> value 1 <> help "Hours needed to complete task" <>
     metavar "TIME")

opts :: Parser (IO ())
opts =
  hsubparser $
  command "add" (info (addTask <$> taskOpts) (progDesc "Add a new task")) <>
  command "plan" (info (pure printPlan) (progDesc "Print the plan")) <>
  command "rm" (info (removeTask <$> nameOpt) (progDesc "Remove task"))

input :: String -> IO String
input s = do
  putStrLn s
  getLine

checkRead :: Read b => String -> IO b
checkRead s = do
  x <- input s
  case readMaybe x of
    Just y -> return y
    Nothing -> do
      putStrLn "Invalid input!"
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  if null args
    then printPlan
    else join $ execParser (info (opts <**> helper) idm)

printPlan :: IO ()
printPlan = do
  t <- getTasks
  time <- getCurrentTime
  forM_ (planDay time t []) $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        putStrLn $ f s <> "-" <> f e <> ": " <> n

addTask :: OptTask -> IO ()
addTask (OptTask n i d t) = do
  time <- getCurrentTime
  old <- getTasks
  let new =
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays (toInteger d) $ utctDay time)
          n
  setTasks $ new : old

getTasks :: IO [Task]
getTasks = do
  home <- getHomeDirectory
  let f = home <> "/.plan.json"
  e <- doesFileExist f
  if e
    then do
      t <- readFile $ home <> "/.plan.json"
      case decode $ read t of
        Just y -> return y
        Nothing -> do
          putStrLn "You edited the plan.json file and now it doesn't work!"
          exitFailure
    else do
      putStrLn "You don't have any tasks defined."
      putStrLn "Run plan add --help to see how to add them."
      exitFailure

setTasks :: [Task] -> IO ()
setTasks t = do
  home <- getHomeDirectory
  writeFile (home <> "/.plan.json") $ show $ encode t

removeTask :: String -> IO ()
removeTask n = do
  t <- getTasks
  setTasks $ filter ((/= n) . taskName) t
