{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ExistentialQuantification #-}
module Test.QuickCheck.Laws (A
                            ,B
                            ,C
                            ,D
                            ,E
                            ,NamedProp(..)
                            ,TestGroup(..)
                            ,module X
                            ,testGroups
                            )where

import Data.Semigroup
import Test.QuickCheck
import Data.Text
import qualified Data.Text.IO as T
import Data.Proxy as X

newtype A = A Ordering deriving (Eq,Ord,Bounded,Show,Semigroup,Monoid,Arbitrary,CoArbitrary)
newtype B = B Ordering deriving (Eq,Ord,Bounded,Show,Semigroup,Monoid,Arbitrary,CoArbitrary)
newtype C = C Ordering deriving (Eq,Ord,Bounded,Show,Semigroup,Monoid,Arbitrary,CoArbitrary)
newtype D = D Ordering deriving (Eq,Ord,Bounded,Show,Semigroup,Monoid,Arbitrary,CoArbitrary)
newtype E = E Ordering deriving (Eq,Ord,Bounded,Show,Semigroup,Monoid,Arbitrary,CoArbitrary)

data NamedProp where
  Named :: (Testable p) => Text -> p -> NamedProp

data TestGroup where
  TestGroup :: (Foldable f) => Text -> f NamedProp -> TestGroup

testGroups :: (Foldable f) => f TestGroup -> IO ()
testGroups = mapM_ testGroup
  where testGroup (TestGroup groupName props) = do
          T.putStrLn $ "Testing group: " <> groupName
          mapM_ check props
        check (Named name prop) = do
          T.putStrLn $ "  Prop: " <> name
          quickCheck prop
