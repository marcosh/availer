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

genNonEmptyIntervalPair :: (Arbitrary a, Ord a) =>
  ((Boundary a, Boundary a, Boundary a, Boundary a) -> Bool) ->  Gen (Interval a, Interval a)
genNonEmptyIntervalPair boundariesComparer =
  (\(start1, end1, start2, end2) -> (boundsInterval start1 end1, boundsInterval start2 end2))
  <$> arbitrary `suchThat` boundariesComparer
