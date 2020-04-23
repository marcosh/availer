module Availer.BoundInterval
  ( BoundInterval
  , boundsInterval
  , lengthInterval
  , intersection
  ) where

import Prelude hiding (length)

import Data.Either.Combinators

import Availer.Action
import Availer.IntervalError
import Availer.Positive

data BoundInterval a = BoundInterval
  { _start :: a
  , _end   :: a
  }

boundsInterval :: Ord a => a -> a -> Either (IntervalError a b) (BoundInterval a)
boundsInterval start end =
  if   (start <= end)
  then Right $ BoundInterval  start end
  else Left  $ EndBeforeStart start end

lengthInterval :: (Action a b, Positive b) => a -> b -> Either (IntervalError a b) (BoundInterval a)
lengthInterval start length =
  if   isPositive length
  then Right $ BoundInterval start (add start length)
  else Left  $ NegativeLength length

intersection :: Ord a => BoundInterval a -> BoundInterval a -> Maybe (BoundInterval a)
intersection (BoundInterval start1 end1) (BoundInterval start2 end2) =
  let
    newStart = max start1 start2
    newEnd   = min end1   end2
  in
    rightToMaybe $ boundsInterval newStart newEnd
