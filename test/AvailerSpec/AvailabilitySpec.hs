{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.AvailabilitySpec where

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Availer.Availability
import Availer.Boundary
import Availer.Interval
import AvailerSpec.Arbitrary.Availability()
import AvailerSpec.Arbitrary.Interval

spec :: Spec
spec =
  describe "Availability" $ do

    it "creates a never availability" $
      isNever never

    it "creates a never availability adding an empty interval to a never availabilities" $
      isNever $ addInterval (empty :: Interval Int) never

    it "creates an availability from a single interval" $ property $ forAll genNonEmptyInterval $
      \(interval :: Interval Int) -> intervals (addInterval interval never) == [interval]

    it "creates an availability from two disjoint intervals" $
      let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 1 Included)
        interval2 = boundsInterval (Boundary 2 Included) (Boundary 3 Included)
      in
        intervals (addInterval interval1 $ addInterval interval2 never) == [interval1, interval2]

    it "creates an availability from two overlapping intervals" $
      let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 2 Included)
        interval2 = boundsInterval (Boundary 1 Included) (Boundary 3 Included)
        union12   = boundsInterval (Boundary 0 Included) (Boundary 3 Included)
      in
        intervals (addInterval interval1 $ addInterval interval2 never) == [union12]

    it "creates an availability from two meeting intervals" $
      let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 1 Included)
        interval2 = boundsInterval (Boundary 1 Excluded) (Boundary 2 Included)
        union12   = boundsInterval (Boundary 0 Included) (Boundary 2 Included)
      in
        intervals (addInterval interval1 $ addInterval interval2 never) == [union12]

    it "addInterval is an idempotent operation" $ property $
      \(interval :: Interval Int) -> addInterval interval (addInterval interval never) == addInterval interval never

    it "creates an availability from three disjoint intervals" $
      let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 1 Included)
        interval2 = boundsInterval (Boundary 2 Included) (Boundary 3 Included)
        interval3 = boundsInterval (Boundary 4 Included) (Boundary 5 Included)
      in
        intervals (addInterval interval3 $ addInterval interval2 $ addInterval interval1 never) == [interval1, interval2, interval3]

    it "creates an availability from three overlapping intervals" $
      let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 2 Included)
        interval2 = boundsInterval (Boundary 3 Included) (Boundary 5 Included)
        interval3 = boundsInterval (Boundary 1 Included) (Boundary 4 Included)
        union123  = boundsInterval (Boundary 0 Included) (Boundary 5 Included)
      in
        intervals (addInterval interval3 $ addInterval interval2 $ addInterval interval1 never) == [union123]

    it "creates an availability from three intervals where two overlaps" $
       let
        interval1 = boundsInterval (Boundary (0 :: Int) Included) (Boundary 2 Included)
        interval2 = boundsInterval (Boundary 4 Included) (Boundary 6 Included)
        interval3 = boundsInterval (Boundary 1 Included) (Boundary 3 Included)
        union13  = boundsInterval (Boundary 0 Included) (Boundary 3 Included)
      in
        intervals (addInterval interval3 $ addInterval interval2 $ addInterval interval1 never) == [union13, interval2]

    describe "semigroup" $ do

      it "is associative" $ property $
        \(availability1 :: Availability Int) availability2 availability3 ->
          availability1 <> (availability2 <> availability3) == (availability1 <> availability2) <> availability3

      it "is commutative" $ property $
        \(availability1 :: Availability Int) availability2 ->
          availability1 <> availability2 == availability2 <> availability1
