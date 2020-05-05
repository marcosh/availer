module Availer.Availability
  ( Availability
  , never
  , addInterval
  ) where

import Availer.Interval

-- | An ordered collection of disjoint intervals
newtype Availability a = Availability {intervals :: [Interval a]}

never :: Availability a
never = Availability []

addInterval :: Ord a => Interval a -> Availability a -> Availability a
addInterval interval (Availability [])                             = Availability [interval]
addInterval interval (Availability (headInterval : intervalsTail)) =
  case interval `union` headInterval of
    Left  unionSingleInterval              -> addInterval unionSingleInterval (Availability intervalsTail)
    Right (unionInterval1, unionInterval2) -> Availability (unionInterval1 : intervals (addInterval unionInterval2 (Availability intervalsTail)))
