{-# OPTIONS_GHC -fno-warn-orphans #-}

module AvailerSpec.Arbitrary.Boundary where

-- QuickCheck
import Test.QuickCheck

import Availer.Boundary

instance Arbitrary IsIncluded where
  arbitrary = oneof [pure Included, pure Excluded]

instance Arbitrary a => Arbitrary (Boundary a) where
  arbitrary = Boundary <$> arbitrary <*> arbitrary