module RemoveSpec where

import Control.Lens
import Data.Bool
import Data.Either
import Instances
import Plan.Env
import Plan.Functions
import Test.Hspec
import Test.QuickCheck

spec :: SpecWith ()
spec =
  describe "Removing tasks" $ do
    it "fails for negative identifiers" $
      property $ \(x, c, s) -> do
        (e, _) <-
          runMonads'
            (removeItem $ fromIntegral $ negate (bool x 1 $ (x :: Word) == 0))
            c
            (Env c s)
        e `shouldSatisfy` isLeft
    it "decreases amount of tasks by 1 for success, 0 for failure" $
      property $ \(x, c, s) -> do
        (e, c') <-
          runMonads' (removeItem $ fromIntegral (x :: Word)) c $ Env c s
        length (c' ^. tasks) `shouldBe` length (c ^. tasks) -
          bool 0 1 (isRight e)
    it "decreases amount of tasks by 1 if identifier exists" $
      property $ \(WithIndex x c, s) -> do
        (_, c') <-
          runMonads' (removeItem $ fromIntegral x) c $ Env c s
        length (c' ^. tasks) `shouldBe` length (c ^. tasks) - 1
    it "fails if there are no tasks" $
      property $ \(x, s, d) -> do
        (e, _) <-
          runMonads' (removeItem x) (Config [] d) $ Env (Config [] d) s
        e `shouldSatisfy` isLeft
