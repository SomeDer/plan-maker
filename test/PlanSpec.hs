module PlanSpec where

import Control.Lens
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Time
import Instances ()
import Plan.Env
import Plan.Functions
import Plan.Plan
import Plan.Task
import Test.Hspec
import Test.QuickCheck
import TestData

spec :: SpecWith ()
spec =
  describe "Planning" $ do
    it
      "removes overdue tasks and tasks with no time needed, unless they are recurring" $
      property $ \(c, s) -> do
        (e, c') <- runMonads' printPlan c $ Env c s
        when (isRight e) $
          length (c' ^. tasks) & shouldBe $
          length $
          filter
            (\x ->
               utctDay (s ^. time) <= x ^. deadline && x ^. timeNeeded > 0 ||
               isJust (x ^. recur)) $
          c ^. tasks
    it "only fails for empty task lists, unless there isn't enough time" $
      property $ \(c, s) -> do
        (e, c') <- runMonads' printPlan c $ Env c s
        when (isLeft e) $
          shouldBe 0 $
          length $
          flip filter (c' ^. tasks) $ \x ->
            utctDayTime (s ^. time) +
            picosecondsToDiffTime (timeNeededToday (s ^. time) x) <=
            timeOfDayToTime (TimeOfDay 23 59 59)
    it "prints simple tasks consecutively" $ do
      (Right r, _) <- runMonads' printPlan con1 $ Env con1 baseSit
      r `shouldBe`
        "1) 15:00-16:00: C\n\
        \1) 16:00-17:00: B\n\
        \1) 17:00-18:00: A"
    it "prints simple tasks in importance order" $ do
      (Right r, _) <- runMonads' printPlan con2 $ Env con2 baseSit
      r `shouldBe`
        "1) 15:00-16:00: A\n\
        \1) 16:00-17:00: B\n\
        \1) 17:00-18:00: C"
    it "gives 20 minutes for a 6 day, 2 hour task" $ do
      (Right r, _) <- runMonads' printPlan con3 $ Env con3 baseSit
      r `shouldBe` "1) 15:00-15:20: A"
    it "gives starting time as current time if currently working on task" $ do
      (Right r, _) <-
        runMonads' printPlan con4 $ Env con4 $ Situation ".plan.yaml" middleTime
      r `shouldBe` "1) 13:30-14:00: A"
    it "prints that tasks are done" $ do
      (Right r, _) <-
        runMonads' printPlan con4 $ Env con4 baseSit
      r `shouldBe`
        "Some tasks are finished for today:\n\
        \1) A"
    it "prints both tasks that are done and not done" $ do
      (Right r, _) <-
        runMonads' printPlan con5 $ Env con5 baseSit
      r `shouldBe`
        "1) 15:00-16:00: B\n\
        \Some tasks are finished for today:\n\
        \1) A"
