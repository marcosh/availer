module Availer.LengthInterval
  ( LengthInterval
  , boundsInterval
  , lengthInterval
  , intersection
  ) where

import Prelude hiding (length)

import Data.Either.Combinators

import Availer.Action
import Availer.IntervalError
import Availer.Positive

data LengthInterval a b = LengthInterval
  { _start  :: a
  , _length :: b
  }

boundsInterval :: (Action a b, Positive b) => a -> a -> Either (IntervalError a b) (LengthInterval a b)
boundsInterval start end =
  let
    diff = difference end start
  in
    if   isPositive diff
    then Right $ LengthInterval start diff
    else Left  $ EndBeforeStart start end

lengthInterval :: (Action a b, Positive b) => a -> b -> Either (IntervalError a b) (LengthInterval a b)
lengthInterval start length =
  if   isPositive length
  then Right $ LengthInterval start length
  else Left  $ NegativeLength length

intersection :: (Action a b, Ord a, Positive b) => LengthInterval a b -> LengthInterval a b -> Maybe (LengthInterval a b)
intersection (LengthInterval start1 length1) (LengthInterval start2 length2) =
  let
    newStart = max start1 start2
    newEnd = min (add start1 length1) (add start2 length2)
    newLength = difference newEnd newStart
  in
    if   isPositive newLength
    then rightToMaybe $ lengthInterval newStart newLength
    else Nothing
