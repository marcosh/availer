{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.RelationSpec where

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Availer.Interval
import Availer.Relation
import AvailerSpec.Arbitrary.Interval
import AvailerSpec.Arbitrary.Relation()

spec :: Spec
spec =
  describe "Relation" $ do

    describe "relate" $ do

      it "detects equal intervals" $ property $
        \(interval :: Interval Int) -> relate interval interval == Equal

      it "detects that Empty is contained in any non-empty interval" $ property $
        forAll genNonEmptyInterval $
          \(interval :: Interval Int) -> relate interval empty == Contains

    describe "invert" $ do

      it "is an involution" $ property $
        \relation -> invert (invert relation) == relation

      it "inverts relations" $ property $
        \(interval1 :: Interval Int) interval2 -> relate interval1 interval2 == invert (relate interval2 interval1)
