module Test.QuickCheck.Data.Bounded where

import Test.QuickCheck
import Test.QuickCheck.Laws

checkBounded :: forall m. (Bounded m, Monoid m, Arbitrary m, Ord m, Eq m, Show m) => Proxy m -> TestGroup
checkBounded _ = TestGroup "Bounded" [Named "bounded" bounded]
  where bounded :: m -> Bool
        bounded x = (minBound <= x) && (maxBound >= x)
