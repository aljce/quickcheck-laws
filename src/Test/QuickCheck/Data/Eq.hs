module Test.QuickCheck.Data.Eq where

import Test.QuickCheck
import Test.QuickCheck.Laws

checkEq :: forall m. (Monoid m, Arbitrary m, Ord m, Eq m, Show m) => Proxy m -> TestGroup
checkEq _ = TestGroup "Eq" [Named "Reflexivity" reflexivity,
                            Named "Symmetry" symmetry,
                            Named "Transitivity" transitivity
                            Named "Negation" negation]
  where reflexivity :: m -> Bool
        reflexivity x = x == x
        symmetry :: m -> m -> Bool
        symmetry x y = (x == y) == (y == x)
        transitivity :: m -> m -> m -> Bool
        transitivity x y z = ((x == y) && (y == z)) {-NSERT IMPLICATION OPERATOR-} (x == z)
        negation :: m -> m -> Bool
        negation x y = (x == y) == not (x /= y)
