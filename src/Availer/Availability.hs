module Availer.Availability
  ( Availability
  , intervals
  , never
  , addInterval
  , isNever
  ) where

import Availer.Interval

-- | An ordered collection of disjoint non-empty intervals
newtype Availability a = Availability {intervals :: [Interval a]}
  deriving (Eq, Show)

never :: Availability a
never = Availability []

addInterval :: Ord a => Interval a -> Availability a -> Availability a
addInterval interval (Availability intervalsList) =
  if isEmpty interval
  -- `interval` is non-empty
  then Availability intervalsList
  else case intervalsList of
    []                           -> Availability [interval]
    headInterval : intervalsTail -> case interval `union` headInterval of
      Left  unionSingleInterval              -> addInterval unionSingleInterval (Availability intervalsTail)
      Right (unionInterval1, unionInterval2) -> Availability (unionInterval1 : intervals (addInterval unionInterval2 (Availability intervalsTail)))

isNever :: Availability a -> Bool
isNever (Availability []) = True
isNever _                 = False

instance Ord a => Semigroup (Availability a) where
  (Availability intervalsList) <> availability = foldr addInterval availability intervalsList

instance Ord a => Monoid (Availability a) where
  mempty = never
