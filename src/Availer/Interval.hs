module Availer.Interval
  ( Interval
  , boundsInterval
  , lengthInterval
  , meet
  , join
  ) where

import Prelude hiding (length)

-- either
import Data.Either.Combinators

-- lens
import Control.Lens (view)

import Availer.Action
import Availer.Boundary
import Availer.IntervalError
import Availer.Positive

data Interval a = Interval
  { _start :: Boundary a
  , _end   :: Boundary a
  }

boundsInterval :: Ord a => Boundary a -> Boundary a -> Either (IntervalError a b) (Interval a)
boundsInterval start end =
  if   start <= end
  then Right $ Interval  start end
  else Left  $ EndBeforeStart start end

lengthInterval :: (Action a b, Positive b) => Boundary a -> b -> IsIncluded -> Either (IntervalError a b) (Interval a)
lengthInterval start length isEndIncluded =
  if   isPositive length
  then Right $ Interval start (Boundary (add (view boundaryValue start) length) isEndIncluded)
  else Left  $ NegativeLength length

meet :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
meet (Interval start1 end1) (Interval start2 end2) =
  let
    newStart = max start1 start2
    newEnd   = min end1   end2
  in
    rightToMaybe $ boundsInterval newStart newEnd

join :: Ord a => Interval a -> Interval a -> [Interval a]
join (Interval start1 end1) (Interval start2 end2) =
  let
    minStart = min start1 start2
    maxStart = max start1 start2
    minEnd   = min end1 end2
    maxEnd   = max end1 end2
  in
    if maxStart <= minEnd
    then [Interval minStart maxEnd]
    else [Interval minStart minEnd, Interval maxStart maxEnd]
