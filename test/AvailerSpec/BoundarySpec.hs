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
    it "compares two boundaries looking at their values" $ property $
      \(b1 :: Boundary Int) b2 -> compare b1 b2 == compare (view boundaryValue b1) (view boundaryValue b2)

    it "compares two included starting boundaries looking at their values" $ property $
      \(a1 :: Int) a2 -> compareStart (Boundary a1 Included) (Boundary a2 Included) == compare a1 a2

    it "compares two excluded starting boundaries looking at their values" $ property $
      \(a1 :: Int) a2 -> compareStart (Boundary a1 Excluded) (Boundary a2 Excluded) == compare a1 a2

    it "compares an included and an excluded starting boundary looking at their values and returning LT if they are equal" $ property $
      \(a1 :: Int) a2 -> compareStart (Boundary a1 Included) (Boundary a2 Excluded) == compare a1 a2 <> LT

    it "compares an included and an excluded equal starting boundary returning LT" $ property $
      \(a1 :: Int) -> compareStart (Boundary a1 Included) (Boundary a1 Excluded) == LT

    it "compares an excluded and an included starting boundary looking at their values and returning GT if they are equal" $ property $
      \(a1 :: Int) a2 -> compareStart (Boundary a1 Excluded) (Boundary a2 Included) == compare a1 a2 <> GT

    it "compares an excluded and an included equal starting boundary returning GT" $ property $
      \(a1 :: Int) -> compareStart (Boundary a1 Excluded) (Boundary a1 Included) == GT

    it "compares two included ending boundaries looking at their values" $ property $
      \(a1 :: Int) a2 -> compareEnd (Boundary a1 Included) (Boundary a2 Included) == compare a1 a2

    it "compares two excluded ending boundaries looking at their values" $ property $
      \(a1 :: Int) a2 -> compareEnd (Boundary a1 Excluded) (Boundary a2 Excluded) == compare a1 a2

    it "compares an included and an excluded ending boundary looking at their values and returning GT if they are equal" $ property $
      \(a1 :: Int) a2 -> compareEnd (Boundary a1 Included) (Boundary a2 Excluded) == compare a1 a2 <> GT

    it "compares an included and an excluded equal ending boundary returning GT" $ property $
      \(a1 :: Int) -> compareEnd (Boundary a1 Included) (Boundary a1 Excluded) == GT

    it "compares an excluded and an included ending boundary looking at their values and returning LT if they are equal" $ property $
      \(a1 :: Int) a2 -> compareEnd (Boundary a1 Excluded) (Boundary a2 Included) == compare a1 a2 <> LT

    it "compares an excluded and an included equal ending boundary returning LT" $ property $
      \(a1 :: Int) -> compareEnd (Boundary a1 Excluded) (Boundary a1 Included) == LT
