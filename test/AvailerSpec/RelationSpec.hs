{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.RelationSpec where

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Availer.Interval
import Availer.Relation
import AvailerSpec.Arbitrary.Interval()
import AvailerSpec.Arbitrary.Relation()

spec :: Spec
spec =
  describe "invert" $ do

    it "is an involution" $ property $
      \relation -> invert (invert relation) == relation

    it "inverts relations" $ property $
      \(interval1 :: Interval Int) interval2 -> relate interval1 interval2 == invert (relate interval2 interval1)
