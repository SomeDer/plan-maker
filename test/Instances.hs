module Instances where

import Control.Applicative
import Control.Monad
import Plan.Env
import Plan.Functions
import Plan.Task
import Plan.TimeRange
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

instance Arbitrary Task where
  arbitrary =
    Task <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary

instance Arbitrary TimeRange where
  arbitrary = liftA2 TimeRange arbitrary arbitrary

instance Arbitrary Env where
  arbitrary = liftA2 Env arbitrary arbitrary

instance Arbitrary Situation where
  arbitrary = liftA2 Situation arbitrary arbitrary

instance Arbitrary OptTask where
  arbitrary = liftM5 OptTask arbitrary arbitrary arbitrary arbitrary arbitrary

instance Arbitrary Config where
  arbitrary = do
    t <- arbitrary
    d <- arbitrary
    let conf = flip Config d
        env ts = Env (conf ts) $ Situation "" t
        f ts = do
          opt <- arbitrary
          s <- arbitrary
          (_, Config ts' _) <-
            runMonads' (addTask s opt) (conf ts) (env ts)
          frequency [(1, return ts'), (10, f ts')]
    ts <- f []
    return $ Config ts d
