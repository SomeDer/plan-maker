module Main where

import Control.Monad
import Data.Aeson
import Data.Time
import Plan.Day
import Plan.Task
import Plan.TimeRange
import System.Directory
import System.Environment
import System.Exit
import Text.Read

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
  case args of
    [] -> printPlan
    ["add"] -> addTask
    _ -> error "Invalid!"

printPlan :: IO ()
printPlan = do
  t <- getTasks
  time <- getCurrentTime
  forM_ (planDay time t []) $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        putStrLn $ f s <> "-" <> f e <> ": " <> n

addTask :: IO ()
addTask = do
  time <- getCurrentTime
  n <- input "Task name: "
  i <- checkRead "Task importance: "
  d <- checkRead "Days until deadline: "
  t <- checkRead "Time needed (hours): " :: IO Double
  old <- getTasks
  let new =
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays d $ utctDay time)
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
    else return []

setTasks :: [Task] -> IO ()
setTasks t = do
  home <- getHomeDirectory
  writeFile (home <> "/.plan.json") $ show $ encode t
