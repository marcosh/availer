{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.RelationSpec where

-- hspec
import Test.Hspec

-- Quickcheck
import Test.QuickCheck

import Availer.Boundary
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

      it "detects that one interval is before another" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          start1 < end1 && GTBoundary end1 < GTBoundary start2 && start2 < end2)) $
            \(interval1 :: Interval Int, interval2) -> relate interval1 interval2 == Before

      it "detects that two open intervals with a common boundary are one before the other" $
        forAll (arbitrary `suchThat` \(boundary1, boundary2, i) ->
          boundary1 < Boundary i Included && Boundary i Included < boundary2) $
            \(boundary1 :: Boundary Int, boundary2, i) ->
              relate (boundsInterval boundary1 (Boundary i Excluded)) (boundsInterval (Boundary i Excluded) boundary2) == Before

      it "detects that one right-closed interval is just before another" $ property $
        forAll (arbitrary `suchThat` \(boundary1, boundary2, i) ->
          boundary1 < Boundary i Included && Boundary i Included < boundary2) $
            \(boundary1 :: Boundary Int, boundary2, i) ->
              relate (boundsInterval boundary1 (Boundary i Included)) (boundsInterval (Boundary i Excluded) boundary2) == JustBefore

      it "detects that one right-open interval is just before another" $ property $
        forAll (arbitrary `suchThat` \(boundary1, boundary2, i) ->
          boundary1 < Boundary i Included && Boundary i Included < boundary2) $
            \(boundary1 :: Boundary Int, boundary2, i) ->
              relate (boundsInterval boundary1 (Boundary i Excluded)) (boundsInterval (Boundary i Included) boundary2) == JustBefore

      it "detects that two intervals overlap" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          start1 < start2 && start2 < end1 && end1 < end2)) $
            \(interval1 :: Interval Int, interval2) -> relate interval1 interval2 == Overlaps

      it "detects that one interval starts another" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          start1 == start2 && start1 < end1 && end1 < end2)) $
            \(interval1 :: Interval Int, interval2) -> relate interval1 interval2 == Starts

      it "detects that one interval finishes another" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          start2 < start1 && start1 < end1 && end1 == end2)) $
            \(interval1 :: Interval Int, interval2) -> relate interval1 interval2 == Finishes

      it "detects that one interval contains another" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          start1 < start2 && start2 < end2 && end2 < end1)) $
            \(interval1 :: Interval Int, interval2) -> relate interval1 interval2 == Contains

    describe "invert" $ do

      it "is an involution" $ property $
        \relation -> invert (invert relation) == relation

      it "inverts relations" $ property $
        \(interval1 :: Interval Int) interval2 -> relate interval1 interval2 == invert (relate interval2 interval1)
