module AvailerSpec.AvailabilitySpec where

-- hspec
import Test.Hspec

-- Quickcheck
-- import Test.QuickCheck

import Availer.Availability

spec :: Spec
spec =
  describe "Availability" $ do

    it "creates a never availability" $
      isNever never
