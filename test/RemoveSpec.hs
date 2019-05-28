module RemoveSpec where

import Control.Lens
import Data.Bool
import Data.Either
import Instances ()
import Plan.Env
import Plan.Functions
import Test.Hspec
import Test.QuickCheck

spec :: SpecWith ()
spec =
  describe "Removing tasks" $ do
    it "fails for negative identifiers" $
      property $ \(x, c, env) -> do
        (r, _) <-
          runMonads'
            (removeItem $ fromIntegral $ negate (bool x 1 $ (x :: Word) == 0))
            c
            env
        r `shouldSatisfy` isLeft
    it "decreases amount of tasks by 1 for success, 0 for failure" $
      property $ \(x, c, env) -> do
        (e, c') <- runMonads' (removeItem $ fromIntegral (x :: Word)) c env
        length (c' ^. tasks) `shouldBe` length (c ^. tasks) -
          bool 0 1 (isRight e)
