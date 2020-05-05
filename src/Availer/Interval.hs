{-# LANGUAGE TemplateHaskell #-}

module Availer.Interval
  ( Interval
  , empty
  , boundsInterval
  , isEmpty
  , start
  , end
  , evalInterval
  , intersection
  , union
  ) where

import Prelude hiding (length)

-- base
import Data.Coerce (coerce)
import Data.Maybe (isJust)

-- lens
import Control.Lens hiding (Empty, _Empty)

import Availer.Boundary

-- | an interval could either be @Empty@ or be delimited by a @_start@ and an @_end@ boundary.
-- Even if it is not enforced by the type (but it is enforced by the constructors), a non-empty interval is always
-- supposed to satisfy @'LTBoundary' _start '<=' 'LTBoundary' _end@
data Interval a
  = Empty
  | Interval { _start :: Boundary a, _end   :: Boundary a }
  deriving (Eq, Show)

makePrisms ''Interval

-- | constructor for the empty interval
empty :: Interval a
empty = Empty

-- | constructs an interval from a starting and an ending bound. If the ending bound is smaller than the string one, we
-- return the empty interval
boundsInterval :: Ord a => Boundary a -> Boundary a -> Interval a
boundsInterval startBoundary endBoundary =
  if   GTBoundary startBoundary <= GTBoundary endBoundary
  then Interval startBoundary endBoundary
  else Empty

-- | checks if the interval is empty
isEmpty :: Interval a -> Bool
isEmpty = isJust . preview _Empty

-- | affine traversal to access the start of a non-empty interval
start :: Traversal' (Interval a) (Boundary a)
start = _Interval . _1

-- | affine traversal to access the end of a non-empty interval
end :: Traversal' (Interval a) (Boundary a)
end = _Interval . _2

-- | since the constructors of @'Interval'@ are not exposes, it is not possible to do pattern matching on them.
-- To compensate, we expose this function which provides the same functionality of pattern matching
evalInterval :: b -> (Boundary a -> Boundary a -> b) -> Interval a -> b
evalInterval ifEmpty _          Empty            = ifEmpty
evalInterval _       ifNotEmpty (Interval b1 b2) = ifNotEmpty b1 b2


-- | computes the intersection of two intervals
intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection Empty                  _                      = Empty
intersection _                      Empty                  = Empty
intersection (Interval start1 end1) (Interval start2 end2) =
  let
    maxStart = coerce $ max (StartBoundary start1) (StartBoundary start2)
    minEnd   = coerce $ min (EndBoundary   end1  ) (EndBoundary   end2  )
  in
    if   GTBoundary maxStart <= GTBoundary minEnd
    then Interval maxStart minEnd
    else Empty

-- | computes the union of two intervals.
-- The result could be either a single interval or two separate intervals.
-- In the latter case, the first interval of the pair is always smaller than the second
union :: Ord a => Interval a -> Interval a -> Either (Interval a) (Interval a, Interval a)
union Empty                  interval               = Left interval
union interval               Empty                  = Left interval
union (Interval start1 end1) (Interval start2 end2) =
  let
    minStart = coerce $ min (StartBoundary start1) (StartBoundary start2)
    maxStart = coerce $ max (StartBoundary start1) (StartBoundary start2)
    minEnd   = coerce $ min (EndBoundary   end1  ) (EndBoundary   end2  )
    maxEnd   = coerce $ max (EndBoundary   end1  ) (EndBoundary   end2  )
  in
    if UnionBoundary minEnd < UnionBoundary maxStart
    then Right (boundsInterval minStart minEnd, boundsInterval maxStart maxEnd)
    else Left (boundsInterval minStart maxEnd)
