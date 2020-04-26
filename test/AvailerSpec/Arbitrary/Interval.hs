{-# OPTIONS_GHC -fno-warn-orphans #-}

module AvailerSpec.Arbitrary.Interval where

-- QuickCheck
import Test.QuickCheck

import Availer.Boundary
import Availer.Interval
import AvailerSpec.Arbitrary.Boundary()

genOrderedPair :: (Arbitrary a, Ord a) => Gen (a, a)
genOrderedPair = arbitrary `suchThat` uncurry (<)

genOrderedBoundaries :: (Arbitrary a, Ord a) => Gen (Boundary a, Boundary a)
genOrderedBoundaries = (\(i1, i2) (inc1, inc2) -> (Boundary i1 inc1, Boundary i2 inc2))
  <$> genOrderedPair
  <*> ((,) <$> arbitrary <*> arbitrary)

genNonEmptyInterval :: (Arbitrary a, Ord a) => Gen (Interval a)
genNonEmptyInterval = uncurry boundsInterval <$> genOrderedBoundaries

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
  arbitrary = oneof [pure empty, genNonEmptyInterval]
