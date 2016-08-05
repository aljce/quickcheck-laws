module Test.QuickCheck.Laws.Control.Applicative where

import Test.QuickCheck
import Test.QuickCheck.Laws

checkApplicative :: forall f.
                    (Applicative f, Arbitrary (f A), Arbitrary (f (A -> B))
                    ,Arbitrary (f (B -> C)), Eq (f A), Eq (f B), Eq (f C)
                    ,Show (f A),Show (f (B -> C)),Show (f (A -> B))) =>
                    Proxy f -> TestGroup
checkApplicative _ = TestGroup "Control.Applicative.Applicative"
  [Named "Checking 'Identity' law for Applicative" identity
  ,Named "Checking 'Composition' law for Applicative" composition ]
  where identity :: f A -> Bool
        identity v = (pure id <*> v) == v
        composition :: f (B -> C) -> f (A -> B) -> f A -> Bool
        composition f g x = ((.) <$> f <*> g <*> x) == (f <*> (g <*> x))
