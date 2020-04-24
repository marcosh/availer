module AvailerSpec.IntervalSpec where

-- hspec
import Test.Hspec

import Availer.Interval

spec :: Spec
spec = do
  describe "Interval" $ do
    it "checks that an emtpy interval is empty " $ do
      isEmpty empty
