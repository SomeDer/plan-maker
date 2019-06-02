module PlanSpec where

import Control.Lens
import Data.Maybe
import Instances ()
import Plan.Env
import Plan.Functions
import Plan.Plan
import Plan.Task
import Test.Hspec
import Test.QuickCheck

spec :: SpecWith ()
spec =
  describe "Planning" $
  it
    "Removes overdue tasks and tasks with no time needed, unless they are recurring" $
  property $ \(c, s) -> do
    (e, c') <- runMonads' printPlan c $ Env c s
    case e of
      Left _ -> return ()
      Right _ ->
        length (c' ^. tasks) & shouldBe $
        length $
        filter
          (\x ->
             c ^. todayIs <= x ^. deadline && x ^. timeNeeded > 0 ||
             isJust (x ^. recur)) $
        c ^. tasks
