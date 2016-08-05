module Test.QuickCheck.Control.Monad where

import Test.QuickCheck
import Test.QuickCheck.Laws

checkMonad :: forall f.
              (Applicative f, Arbitrary (f A), Arbitrary (f (A -> B))
              ,Arbitrary (f (B -> C)), Eq (f A), Eq (f B), Eq (f C)
              ,Show (f A), Show (f (B -> C)),Show (f (A -> b))) =>
              Proxy f -> TestGroup
checkMonad _ = TestGroup "Control.Monad.Monad"
  [Named "Checking 'Left Identity' law for Monad" leftIdentity
  ,Named "Checking 'Right Identity' law for Monad" rightIdentity
  ,Named "Checking 'Associativity' law for Monad" associativity]
