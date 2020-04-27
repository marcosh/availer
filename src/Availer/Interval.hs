{-# LANGUAGE TemplateHaskell #-}

module Availer.Interval
  ( Interval
  , empty
  , boundsInterval
  , isEmpty
  , start
  , end
  , intersection
  ) where

import Prelude hiding (length)

-- base
import Data.Maybe (isJust)

-- lens
import Control.Lens hiding (Empty, _Empty)

import Availer.Boundary

-- | an interval could either be `Empty` or be delimited by a `start` and an `end` boundary.
data Interval a
  = Empty
  | Interval { _start :: Boundary a, _end   :: Boundary a }
  deriving (Eq, Show)

makePrisms ''Interval

empty :: Interval a
empty = Empty

-- | constructs an interval from a starting and an ending bound. If the ending bound is smaller than the string one, we
-- | return the empty interval
boundsInterval :: Ord a => Boundary a -> Boundary a -> Interval a
boundsInterval startBoundary endBoundary =
  if   GTBoundary startBoundary <= GTBoundary endBoundary
  then Interval startBoundary endBoundary
  else Empty

isEmpty :: Interval a -> Bool
isEmpty = isJust . preview _Empty

start :: Traversal' (Interval a) (Boundary a)
start = _Interval . _1

end :: Traversal' (Interval a) (Boundary a)
end = _Interval . _2

intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection Empty                  _                      = Empty
intersection _                      Empty                  = Empty
intersection (Interval start1 end1) (Interval start2 end2) =
  let
    maxStart = max start1 start2
    minEnd   = min end1   end2
  in
    if   GTBoundary maxStart <= GTBoundary minEnd
    then Interval maxStart minEnd
    else Empty
