module Main where

import Control.Monad
import Data.Time
import Plan.Day
import Plan.Task
import Plan.TimeRange

input :: String -> IO String
input s = do
  putStrLn s
  getLine

main :: IO ()
main = do
  time <- getCurrentTime
  c <- input "Number of tasks: "
  x <-
    forM [1 .. (read c :: Int)] $ \_ -> do
      n <- input "Task name: "
      i <- input "Task importance: "
      d <- input "Days until deadline: "
      t <- input "Time needed (hours): "
      return $
        Task
          Nothing
          (picosecondsToDiffTime $ read t * 3600 * 10 ^ (12 :: Int))
          (read i)
          (addDays (read d) $ utctDay time)
          n
  forM_ (planDay time x []) $ \(Task (Just (TimeRange s e)) _ _ _ n) ->
    let f = take 5 . show
     in unless (n `elem` ["Now", "Midnight"]) $
        putStrLn $ f s <> "-" <> f e <> ": " <> n
