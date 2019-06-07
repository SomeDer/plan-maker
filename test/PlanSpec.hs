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
