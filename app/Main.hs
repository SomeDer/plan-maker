module Main where

import Control.Monad
import Data.Aeson
import Data.Time
import Plan.Day
import Plan.Task
import Plan.TimeRange
import System.Directory
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
  time <- getCurrentTime
  c <- input "Number of tasks: "
  x <-
    forM [1 .. (read c :: Int)] $ \_ -> do
      n <- input "Task name: "
      i <- checkRead "Task importance: "
      d <- checkRead "Days until deadline: "
      t <- checkRead "Time needed (hours): " :: IO Double
      return $
        Task
          Nothing
          (picosecondsToDiffTime $ round $ t * 3600 * 10 ^ (12 :: Int))
          i
          (addDays d $ utctDay time)
          n
  let plan = planDay time x []
  hd <- getHomeDirectory
  writeFile (hd <> "/.plan.json") $ show $ encode plan
  forM_ plan $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        putStrLn $ f s <> "-" <> f e <> ": " <> n
