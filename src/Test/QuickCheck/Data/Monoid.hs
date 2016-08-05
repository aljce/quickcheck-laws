module Test.QuickCheck.Data.Monoid where

import Test.QuickCheck
import Test.QuickCheck.Laws

import Data.Monoid

checkMonoid :: forall m. (Monoid m, Arbitrary m, Eq m, Show m) => Proxy m -> TestGroup
checkMonoid _ = TestGroup "Data.Monoid.Monoid" [Named "Left Identity" leftIdentity,
                                                Named "Right Identity" rightIdentity,
                                                Named "Associativity" associativity]
  where leftIdentity  :: m -> Bool
        leftIdentity  x = (mempty <> x) == x
        rightIdentity :: m -> Bool
        rightIdentity x = (x <> mempty) == x
        associativity :: m -> m -> m -> Bool
        associativity x y z = x <> (y <> z) == (x <> y) <> z
