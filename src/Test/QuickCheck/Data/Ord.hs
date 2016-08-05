module Test.QuickCheck.Data.Ord where

import Test.QuickCheck
import Test.QuickCheck.Laws

checkOrd :: forall m. (Monoid m, Arbitrary m, Ord m, Eq m, Show m) => Proxy m -> TestGroup
checkOrd _ = TestGroup "Ord" [Named "Antisymmetry" antisymmetry,
                              Named "Transitivity" transitivity,
                              Named "Relexivity" reflexivity]
  where antisymmetry :: m -> m -> Bool
        antisymmetry a b = (a <= b) && (b <= a) ===> (a == b)
        transitivity :: m -> m -> m -> Bool
        transitivity a b c = (a <= b) && (b <= c) ===> (a <= b)
        reflexivity :: m -> Bool
        reflexivity a = a <= a
