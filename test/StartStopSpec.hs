module StartStopSpec where

import Control.Lens
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Time
import Instances ()
import Plan.Env
import Plan.Functions
import Plan.Task
import Test.Hspec
import Test.QuickCheck

taskWithId1 :: [Task] -> Task
taskWithId1 = head . filter ((== 1) . view identifier)

spec :: SpecWith ()
spec = do
  describe "Starting tasks" $ do
    it "works only once" $
      property $ \(c, s) -> do
        (_, c') <- runMonads' (startWork 1) c $ Env c s
        (e, _) <- runMonads' (startWork 1) c' $ Env c' s
        e `shouldSatisfy` isLeft
    it "sets workingFrom to current time if succesful" $
      property $ \(c, s) -> do
        (e, c') <- runMonads' (startWork 1) c $ Env c s
        if isRight e
          then taskWithId1 (c' ^. tasks) ^. workingFrom `shouldBe`
               (Just $ timeToTimeOfDay $ utctDayTime $ s ^. time)
          else c `shouldBe` c'
    it "only fails for empty task lists and already started tasks" $
      property $ \(c, s) -> do
        (e, _) <- runMonads' (startWork 1) c $ Env c s
        when (isLeft e) $
          shouldSatisfy c $ \x ->
            null (x ^. tasks) ||
            isJust (taskWithId1 (x ^. tasks) ^. workingFrom)
