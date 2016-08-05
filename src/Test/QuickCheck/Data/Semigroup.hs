module Test.QuickCheck.Data.Semigroups where

import Test.QuickCheck
import Test.QuickCheck.Laws

import Data.Monoid

checkSemigroup :: forall m. (Monoid m, Arbitrary m, Eq m, Show m) => Proxy m -> TestGroup
checkSemigroup _ = TestGroup "Semigroup" [Named "Associativity" associativity]
  where associativity :: m -> m -> m -> Bool
        associativity x y z = x <> (y <> z) == (x <> y) <> z
