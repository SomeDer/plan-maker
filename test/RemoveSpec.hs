module RemoveSpec where

import Data.Bool
import Data.Either
import Instances ()
import Plan.Functions
import Test.Hspec
import Test.QuickCheck

spec :: SpecWith ()
spec =
  describe "Removing tasks" $
  it "fails for negative identifiers" $
  property $ \(x, c, env) -> do
    (r, _) <-
      runMonads'
        (removeItem $ fromIntegral $ negate (bool x 1 $ (x :: Word) == 0))
        c
        env
    r `shouldSatisfy` isLeft
