module Instances where

import Control.Applicative
import Plan.Env
import Plan.Task
import Plan.TimeRange
import Test.QuickCheck.Arbitrary
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

instance Arbitrary Config where
  arbitrary = liftA2 Config arbitrary arbitrary
