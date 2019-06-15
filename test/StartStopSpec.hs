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
import Plan.TimeRange
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
  describe "Stopping tasks" $ do
    it "fails if workingFrom doesn't exist" $
      property $ \(c, s) -> do
        (e, _) <- runMonads' (stopWork 1) c $ Env c s
        when
          (null (c ^. tasks) & (||) $
           isNothing $ taskWithId1 (c ^. tasks) ^. workingFrom) $
          e `shouldSatisfy` isLeft
    it "succeeds if task was started before" $
      property $ \(c, s) -> do
        (_, c') <- runMonads' (startWork 1) c $ Env c s
        (e, _) <- runMonads' (stopWork 1) c' $ Env c' s
        unless (null $ c ^. tasks) $ e `shouldSatisfy` isRight
    it "records start as workingFrom and end as current time" $
      property $ \(c, s) -> do
        (_, c') <- runMonads' (startWork 1) c $ Env c s
        (e, c'') <- runMonads' (stopWork 1) c' $ Env c' s
        when (isRight e) $
          last (taskWithId1 (c'' ^. tasks) ^. workedToday) `shouldBe`
          TimeRange
            (fromJust $ taskWithId1 (c' ^. tasks) ^. workingFrom)
            (timeToTimeOfDay $ utctDayTime $ s ^. time)
