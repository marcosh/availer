{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AvailerSpec.Arbitrary.Availability where

-- QuickCheck
import Test.QuickCheck

import Availer.Availability
import AvailerSpec.Arbitrary.Interval()

instance (Arbitrary a, Ord a) => Arbitrary (Availability a) where
  arbitrary = foldr addInterval never <$> listOf arbitrary
