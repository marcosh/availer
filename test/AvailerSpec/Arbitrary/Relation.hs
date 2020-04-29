{-# OPTIONS_GHC -fno-warn-orphans #-}

module AvailerSpec.Arbitrary.Relation where

-- QuickCheck
import Test.QuickCheck

import Availer.Relation

instance Arbitrary Relation where
  arbitrary = oneof
    [ pure Equal
    , pure Starts
    , pure Finishes
    , pure During
    , pure StartedBy
    , pure FinishedBy
    , pure Contains
    , pure Before
    , pure After
    , pure JustBefore
    , pure JustAfter
    , pure Overlaps
    , pure OverlappedBy
    ]
