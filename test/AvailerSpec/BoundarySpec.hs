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

      it "compares two included starting boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded starting boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Excluded)) == compare a1 a2

      it "compares an included and an excluded starting boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Included)) (StartBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included and an excluded equal starting boundary returning LT" $ property $
        \(a1 :: Int) -> StartBoundary (Boundary a1 Included) < StartBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included starting boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (StartBoundary (Boundary a1 Excluded)) (StartBoundary (Boundary a2 Included)) == compare a1 a2 <> GT

      it "compares an excluded and an included equal starting boundary returning GT" $ property $
        \(a1 :: Int) -> StartBoundary (Boundary a1 Excluded) > StartBoundary (Boundary a1 Included)

    describe "EndBoundary" $ do

      it "compares two included ending boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares two excluded ending boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Excluded)) == compare a1 a2

      it "compares an included and an excluded ending boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Included)) (EndBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included and an excluded equal ending boundary returning GT" $ property $
        \(a1 :: Int) -> EndBoundary (Boundary a1 Included) > EndBoundary (Boundary a1 Excluded)

      it "compares an excluded and an included ending boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (EndBoundary (Boundary a1 Excluded)) (EndBoundary (Boundary a2 Included)) == compare a1 a2 <> LT

      it "compares an excluded and an included equal ending boundary returning LT" $ property $
        \(a1 :: Int) -> EndBoundary (Boundary a1 Excluded) < EndBoundary (Boundary a1 Included)

    describe "GTBoundary" $ do

      it "compares a starting and an ending included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares a starting and an ending excluded boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Excluded)) (GTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included starting and an excluded ending boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Included)) (GTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> GT

      it "compares an included starting and an excluded ending boundary with equal values returning GT" $ property $
        \(a1 :: Int) -> GTBoundary (Boundary a1 Included) > GTBoundary (Boundary a1 Excluded)

      it "compares an excluded starting and an included ending boundary looking at their values and returning GT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (GTBoundary (Boundary a1 Excluded)) (GTBoundary (Boundary a2 Included)) == compare a1 a2 <> GT

      it "compares an excluded starting and an included ending boundary with equal values returning GT" $ property $
        \(a1 :: Int) -> GTBoundary (Boundary a1 Excluded) > GTBoundary (Boundary a1 Included)

    describe "LTBoundary" $ do

      it "compares an ending and a starting included boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Included)) == compare a1 a2

      it "compares an ending and a starting excluded boundaries looking at their values" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Excluded)) (LTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included ending and an excluded starting boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Included)) (LTBoundary (Boundary a2 Excluded)) == compare a1 a2 <> LT

      it "compares an included ending and an excluded starting boundary with equal values returning LT" $ property $
        \(a1 :: Int) -> LTBoundary (Boundary a1 Included) < LTBoundary (Boundary a1 Excluded)

      it "compares an excluded ending and an included starting boundary looking at their values and returning LT if they are equal" $ property $
        \(a1 :: Int) a2 -> compare (LTBoundary (Boundary a1 Excluded)) (LTBoundary (Boundary a2 Included)) == compare a1 a2 <> LT

      it "compares an excluded ending and an included starting boundary with equal values returning LT" $ property $
        \(a1 :: Int) -> LTBoundary (Boundary a1 Excluded) < LTBoundary (Boundary a1 Included)
