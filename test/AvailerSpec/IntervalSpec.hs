{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.IntervalSpec where

-- base
import Data.Either (isLeft)

-- hspec
import Test.Hspec

-- lens
import Control.Lens hiding (Empty, _Empty)

-- Quickcheck
import Test.QuickCheck

import Availer.Boundary
import Availer.Interval
import AvailerSpec.Arbitrary.Boundary()
import AvailerSpec.Arbitrary.Interval

spec :: Spec
spec =
  describe "Interval" $ do

    describe "empty" $

      it "creates an emtpy interval" $
        isEmpty empty

    describe "boundsInterval" $ do

      it "starting boundary > ending boundary => empty" $ property $ forAll genOrderedPair $
        \(a1 :: Int, a2) isStartIncluded isEndIncluded ->
          isEmpty $ boundsInterval (Boundary a2 isStartIncluded) (Boundary a1 isEndIncluded)

      it "starting boundary > ending boundary => interval" $ property $ forAll genOrderedPair $
        \(a1 :: Int, a2) isStartIncluded isEndIncluded ->
          let
            startBoundary = Boundary a1 isStartIncluded
            endBoundary = Boundary a2 isEndIncluded
            interval = boundsInterval startBoundary endBoundary
          in
            preview start interval == Just startBoundary &&
            preview end   interval == Just endBoundary

      it "starting boundary excluded = ending boundary excluded => empty" $ property $
        \(a :: Int) -> isEmpty $ boundsInterval (Boundary a Excluded) (Boundary a Excluded)

      it "starting boundary excluded = ending boundary included => empty" $ property $
        \(a :: Int) -> isEmpty $ boundsInterval (Boundary a Excluded) (Boundary a Included)

      it "starting boundary included = ending boundary excluded => empty" $ property $
        \(a :: Int) -> isEmpty $ boundsInterval (Boundary a Included) (Boundary a Excluded)

      it "starting boundary included = ending boundary included => degenerate interval" $ property $
        \(a :: Int) ->
          let
            startBoundary = Boundary a Included
            endBoundary = Boundary a Included
            interval = boundsInterval startBoundary endBoundary
          in
            preview start interval == Just startBoundary &&
            preview end   interval == Just endBoundary

    describe "intersection" $ do

      it "is symmetric" $ property $
        \(interval1 :: Interval Int) interval2 ->
          intersection interval1 interval2 == intersection interval2 interval1

      it "is associative" $ property $
        \(interval1 :: Interval Int) interval2 interval3 ->
          interval1 `intersection` (interval2 `intersection` interval3) ==
          (interval1 `intersection` interval2) `intersection` interval3

      it "Empty intersects _ == Empty" $ property $
        \(interval :: Interval Int) ->
          isEmpty $ intersection empty interval

      it "is idempotent" $ property $
        \(interval1 :: Interval Int) interval2 ->
          (interval1 `intersection` interval2) `intersection` interval2 == interval1 `intersection` interval2

      it "returns empty for disjoint intervals" $ property $
        forAll (genNonEmptyIntervalPair (\(_, end1, start2, _) -> LTBoundary end1 < LTBoundary start2)) $
          \(interval1 :: Interval Int, interval2) -> interval1 `intersection` interval2 == empty

      it "returns the first interval if it is contained in the second" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          StartBoundary start1 >= StartBoundary start2 && EndBoundary end1 <= EndBoundary end2)) $
            \(interval1 :: Interval Int, interval2) -> interval1 `intersection` interval2 == interval1

    describe "union" $ do

      it "is symmetric" $ property $
        \(interval1 :: Interval Int) interval2 ->
          union interval1 interval2 == union interval2 interval1

      it "Empty union _ == _" $ property $
        \(interval :: Interval Int) ->
          union empty interval == Left interval

      it "returns the two intervals for distant non-empty intervals" $ property $
        forAll (genNonEmptyIntervalPair (\(_, end1, start2, _) -> UnionBoundary end1 < UnionBoundary start2)
          `suchThat` (\(interval1, interval2) -> not (isEmpty interval1) && not (isEmpty interval2))) $
            \(interval1 :: Interval Int, interval2) -> interval1 `union` interval2 == Right (interval1, interval2)

      it "returns a single interval if the two intervals are adjacent" $ property $
        forAll (genNonEmptyIntervalPair (\(_, end1, start2, _) -> compare (UnionBoundary end1) (UnionBoundary start2) == EQ)) $
          \(interval1 :: Interval Int, interval2) -> isLeft $ interval1 `union` interval2

      it "returns the second interval if the first is contained in the second" $ property $
        forAll (genNonEmptyIntervalPair (\(start1, end1, start2, end2) ->
          StartBoundary start1 >= StartBoundary start2 && EndBoundary end1 <= EndBoundary end2)) $
            \(interval1 :: Interval Int, interval2) -> interval1 `union` interval2 == Left interval2
