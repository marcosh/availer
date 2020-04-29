{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.BoundarySpec where

-- hspec
import Test.Hspec

-- lens
import Control.Lens

-- Quickcheck
import Test.QuickCheck

import Availer.Boundary
import AvailerSpec.Arbitrary.Boundary()

spec :: Spec
spec =
  describe "Boundary" $ do
    describe "compare" $ do

      it "compares two boundaries looking at their values" $ property $
        \(b1 :: Boundary Int) b2 -> compare b1 b2 == compare (view boundaryValue b1) (view boundaryValue b2)

    describe "StartBoundary" $ do

      it "compares two included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Excluded)) == compare a1 a2

      it "compares an included and an excluded boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included and an excluded equal boundary returning LT" $ property $
        \(a1 :: Int) -> StartBoundary (Boundary a1 Included) < StartBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Included)) == compare a1 a2 <> GT

      it "compares an excluded and an included equal boundary returning GT" $ property $
        \(a1 :: Int) -> StartBoundary (Boundary a1 Excluded) > StartBoundary (Boundary a1 Included)

    describe "EndBoundary" $ do

      it "compares two included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Excluded)) == compare a1 a2

      it "compares an included and an excluded boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included and an excluded equal boundary returning GT" $ property $
        \(a1 :: Int) -> EndBoundary (Boundary a1 Included) > EndBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Included)) == compare a1 a2 <> LT

      it "compares an excluded and an included equal boundary returning LT" $ property $
        \(a1 :: Int) -> EndBoundary (Boundary a1 Excluded) < EndBoundary (Boundary a1 Included)

    describe "GTBoundary" $ do

      it "compares two included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded boundaries looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Excluded)) (GTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included and an excluded boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included and an excluded boundary with equal values returning GT" $ property $
        \(a1 :: Int) -> GTBoundary (Boundary a1 Included) > GTBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Excluded)) (GTBoundary (Boundary a2 Included)) == compare a1 a2 <> GT

      it "compares an excluded and an included boundary with equal values returning GT" $ property $
        \(a1 :: Int) -> GTBoundary (Boundary a1 Excluded) > GTBoundary (Boundary a1 Included)

    describe "LTBoundary" $ do

      it "compares two included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded boundaries looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Excluded)) (LTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included and an excluded boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included and an excluded boundary with equal values returning LT" $ property $
        \(a1 :: Int) -> LTBoundary (Boundary a1 Included) < LTBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Excluded)) (LTBoundary (Boundary a2 Included)) == compare a1 a2 <> LT

      it "compares an excluded and an included boundary with equal values returning LT" $ property $
        \(a1 :: Int) -> LTBoundary (Boundary a1 Excluded) < LTBoundary (Boundary a1 Included)

    describe "UnionBoundary" $ do

      it "compares two included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (UnionBoundary (Boundary a1 Included)) (UnionBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded boundaries looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (UnionBoundary (Boundary a1 Excluded)) (UnionBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included and an excluded boundary looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (UnionBoundary (Boundary a1 Included)) (UnionBoundary (Boundary a2 Excluded)) == compare a1 a2

      it "compares an included and an excluded boundary with equal values returning EQ" $ property $
        \(a1 :: Int) -> compare (UnionBoundary (Boundary a1 Included)) (UnionBoundary (Boundary a1 Excluded)) == EQ

      it "compares an excluded and an included boundary looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (UnionBoundary (Boundary a1 Excluded)) (UnionBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares an excluded and an included boundary with equal values returning EQ" $ property $
        \(a1 :: Int) -> compare (UnionBoundary (Boundary a1 Excluded)) (UnionBoundary (Boundary a1 Included)) == EQ
