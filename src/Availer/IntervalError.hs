module Availer.IntervalError where

import Availer.Boundary

data IntervalError a b
  = EndBeforeStart (Boundary a) (Boundary a)
  | NegativeLength b
